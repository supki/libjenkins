{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Jenkins.REST.Internal where

import           Control.Applicative
import           Control.Concurrent.Async (concurrently)
import           Control.Exception (try, toException)
import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.Free.Church (F, iterM, liftF)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control (MonadTransControl(..))
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import           Control.Monad.Trans.Maybe (MaybeT(..), mapMaybeT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (ResourceT)
import           Data.Data (Data, Typeable)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit
import           Network.HTTP.Types (Status(..))

import           Jenkins.REST.Method
import qualified Network.HTTP.Conduit.Lens as L


-- | Jenkins REST API composable queries
newtype Jenkins a = Jenkins { unJenkins :: F JenkinsF a }
  deriving (Functor, Applicative, Monad)

instance MonadIO Jenkins where
  liftIO = liftJ . IO
  {-# INLINE liftIO #-}

-- | 'JenkinsF' terms
data JenkinsF a where
  Get  :: Method Complete f -> (BL.ByteString -> a) -> JenkinsF a
  Post :: (forall f. Method Complete f) -> BL.ByteString -> (BL.ByteString -> a) -> JenkinsF a
  Conc :: Jenkins a -> Jenkins b -> (a -> b -> c) -> JenkinsF c
  IO   :: IO a -> JenkinsF a
  With :: (Request -> Request) -> Jenkins b -> (b -> a) -> JenkinsF a
  Dcon :: JenkinsF a

instance Functor JenkinsF where
  fmap f (Get  m g)      = Get  m      (f . g)
  fmap f (Post m body g) = Post m body (f . g)
  fmap f (Conc m n g)    = Conc m n    (\a b -> f (g a b))
  fmap f (IO a)          = IO (fmap f a)
  fmap f (With h j g)    = With h j    (f . g)
  fmap _ Dcon            = Dcon
  {-# INLINE fmap #-}

-- | Lift 'JenkinsF' query to the 'Jenkins' query language
liftJ :: JenkinsF a -> Jenkins a
liftJ = Jenkins . liftF
{-# INLINE liftJ #-}


-- | Query Jenkins REST API
--
-- Successful result can be either @ 'Just' val @ or @ 'Nothing' @ in case of
-- premature disconnect (see 'reload', 'restart', or 'forceRestart')
--
-- Catches 'HttpException's thrown by @http-conduit@, but
-- does not catch exceptions thrown by embedded 'IO' actions
runJenkins :: ConnectInfo -> Jenkins a -> IO (Either HttpException (Maybe a))
runJenkins (ConnectInfo h p user token) jenk =
  try . withManager $ \manager -> do
    req <- liftIO $ parseUrl h
    let req' = req
          & L.port            .~ p
          & L.responseTimeout .~ Just (20 * 1000000)
    runReaderT (runMaybeT (iterJenkinsIO manager jenk)) (applyBasicAuth user token req')

-- | Interpret 'JenkinsF' AST in 'IO'
iterJenkinsIO
  :: Manager
  -> Jenkins a
  -> MaybeT (ReaderT Request (ResourceT IO)) a
iterJenkinsIO manager = iterJenkins (interpreter manager)
{-# INLINE iterJenkinsIO #-}

-- | Tear down 'JenkinsF' AST with a 'JenkinsF'-algebra
iterJenkins :: Monad m => (JenkinsF (m a) -> m a) -> Jenkins a -> m a
iterJenkins go = iterM go . unJenkins
{-# INLINE iterJenkins #-}

-- | 'JenkinsF' AST interpreter
interpreter
  :: Manager
  -> JenkinsF (MaybeT (ReaderT Request (ResourceT IO)) a)
  -> MaybeT (ReaderT Request (ResourceT IO)) a
interpreter manager = go where
  go (Get m next) = do
    req <- lift ask
    let req' = req
          & L.path   %~ (`slash` render m)
          & L.method .~ "GET"
    bs <- lift . lift $ httpLbs req' manager
    next (responseBody bs)
  go (Post m body next) = do
    req <- lift ask
    let req' = req
          & L.path          %~ (`slash` render m)
          & L.method        .~ "POST"
          & L.requestBody   .~ RequestBodyLBS body
          & L.redirectCount .~ 0
          & L.checkStatus   .~ \s@(Status st _) hs cookie_jar ->
            if 200 <= st && st < 400
                then Nothing
                else Just . toException $ StatusCodeException s hs cookie_jar
    res <- lift . lift $ httpLbs req' manager
    next (responseBody res)
  go (Conc jenka jenkb next) = do
    (a, b) <- liftWith $ \run' -> liftWith $ \run'' -> liftWith $ \run''' ->
      let run :: Jenkins t -> IO (StT ResourceT (StT (ReaderT Request) (StT MaybeT t)))
          run = run''' . run'' . run' . iterJenkinsIO manager
      in concurrently (run jenka) (run jenkb)
    c <- restoreT . restoreT . restoreT $ return a
    d <- restoreT . restoreT . restoreT $ return b
    next c d
  go (IO action) = join (liftIO action)
  go (With f jenk next) = do
    res <- mapMaybeT (local f) (iterJenkinsIO manager jenk)
    next res
  go Dcon = fail "disconnect"


-- | Jenkins connection settings
--
-- '_jenkinsApiToken' may be user's password, Jenkins
-- does not make any distinction between these concepts
data ConnectInfo = ConnectInfo
  { _jenkinsUrl      :: String       -- ^ Jenkins URL, e.g. @http:\/\/example.com\/jenkins@
  , _jenkinsPort     :: Int          -- ^ Jenkins port, e.g. @8080@
  , _jenkinsUser     :: B.ByteString -- ^ Jenkins user, e.g. @jenkins@
  , _jenkinsApiToken :: B.ByteString -- ^ Jenkins user API token
  } deriving (Show, Eq, Typeable, Data, Generic)


-- | Default Jenkins connection settings
--
-- @
-- defaultConnectInfo = ConnectInfo
--   { _jenkinsUrl      = \"http:\/\/example.com\/jenkins\"
--   , _jenkinsPort     = 8080
--   , _jenkinsUser     = \"jenkins\"
--   , _jenkinsApiToken = \"\"
--   }
-- @
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo
  { _jenkinsUrl      = "http://example.com/jenkins"
  , _jenkinsPort     = 8080
  , _jenkinsUser     = "jenkins"
  , _jenkinsApiToken = ""
  }

-- | A lens into Jenkins URL
jenkinsUrl :: Lens' ConnectInfo String
jenkinsUrl f req = (\u' -> req { _jenkinsUrl = u' }) <$> f (_jenkinsUrl req)
{-# INLINE jenkinsUrl #-}

-- | A lens into Jenkins port
jenkinsPort :: Lens' ConnectInfo Int
jenkinsPort f req = (\p' -> req { _jenkinsPort = p' }) <$> f (_jenkinsPort req)
{-# INLINE jenkinsPort #-}

-- | A lens into Jenkins user
jenkinsUser :: Lens' ConnectInfo B.ByteString
jenkinsUser f req = (\u' -> req { _jenkinsUser = u' }) <$> f (_jenkinsUser req)
{-# INLINE jenkinsUser #-}

-- | A lens into Jenkins user API token
jenkinsApiToken :: Lens' ConnectInfo B.ByteString
jenkinsApiToken f req = (\a' -> req { _jenkinsApiToken = a' }) <$> f (_jenkinsApiToken req)
{-# INLINE jenkinsApiToken #-}

-- | A lens into Jenkins password
--
-- @
-- jenkinsPassword = jenkinsApiToken
-- @
jenkinsPassword :: Lens' ConnectInfo B.ByteString
jenkinsPassword = jenkinsApiToken
{-# INLINE jenkinsPassword #-}
