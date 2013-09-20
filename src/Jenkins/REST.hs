{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Jenkins.REST
  ( module Jenkins.REST
  , module Jenkins.REST.Lens
  , module Jenkins.REST.Method
  , Request
  , SomeException
  ) where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception (SomeException, try, toException)
import           Control.Lens
import           Control.Applicative (Applicative(..))
import           Control.Monad.Free.Church (F, iterM, liftF)
import           Control.Monad.Trans.Control (liftWith, restoreT)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (ResourceT)
import           Data.Default (Default(..))
import           Data.Monoid (mempty)
import           Network.HTTP.Conduit
  ( Manager, Request, RequestBody(..)
  , withManager, applyBasicAuth, httpLbs, parseUrl, responseBody
  , HttpException(..)
  )
import           Network.HTTP.Types
  (Status(..))
import           Text.XML (Document, renderLBS)

import           Jenkins.REST.Method
import qualified Jenkins.REST.Lens
import           Jenkins.REST.Lens as L

{-# ANN module ("HLint: Use const" :: String) #-}


newtype Jenkins a = Jenkins { unJenkins :: F JenkinsF a }

instance Functor Jenkins where
  fmap f = Jenkins . fmap f . unJenkins
  {-# INLINE fmap #-}

instance Applicative Jenkins where
  pure = Jenkins . pure
  {-# INLINE pure #-}
  Jenkins f <*> Jenkins x = Jenkins (f <*> x)
  {-# INLINE (<*>) #-}

instance Monad Jenkins where
  return = pure
  {-# INLINE return #-}
  Jenkins x >>= k = Jenkins (x >>= unJenkins . k)
  {-# INLINE (>>=) #-}

instance MonadIO Jenkins where
  liftIO = liftJ . IO
  {-# INLINE liftIO #-}

-- | 'JenkinsF' terms
data JenkinsF a where
  Get  :: Method Complete f -> (BL.ByteString -> a) -> JenkinsF a
  Post :: (forall f. Method Complete f) -> BL.ByteString -> (BL.ByteString -> a) -> JenkinsF a
  Conc :: [Jenkins b] -> ([b] -> a) -> JenkinsF a
  IO   :: IO a -> JenkinsF a
  With :: (forall m. Request m -> Request m) -> Jenkins b -> (b -> a) -> JenkinsF a

instance Functor JenkinsF where
  fmap f (Get  m g)      = Get  m      (f . g)
  fmap f (Post m body g) = Post m body (f . g)
  fmap f (Conc ms g)     = Conc ms     (f . g)
  fmap f (IO a)          = IO (fmap f a)
  fmap f (With h j g)    = With h j    (f . g)
  {-# INLINE fmap #-}


-- | List 'JenkinsF' term to the 'Jenkins' language
liftJ :: JenkinsF a -> Jenkins a
liftJ = Jenkins . liftF
{-# INLINE liftJ #-}


-- | @GET@ query
get :: Method Complete f -> Jenkins BL.ByteString
get m = liftJ $ Get m id
{-# INLINE get #-}

-- | @POST@ query (with payload)
post :: (forall f. Method Complete f) -> BL.ByteString -> Jenkins ()
post m body = liftJ $ Post m body (\_ -> ())
{-# INLINE post #-}

-- | @POST@ query (without payload)
post_ :: (forall f. Method Complete f) -> Jenkins ()
post_ m = post m mempty
{-# INLINE post_ #-}

-- | Do a list of queries 'concurrently'
concurrently :: [Jenkins a] -> Jenkins [a]
concurrently js = liftJ $ Conc js id
{-# INLINE concurrently #-}

-- | Lift arbitrary 'IO' action
io :: IO a -> Jenkins a
io = liftIO
{-# INLINE io #-}

-- | Make custom local changes to 'Request'
with :: (forall m. Request m -> Request m) -> Jenkins a -> Jenkins a
with f j = liftJ $ With f j id
{-# INLINE with #-}


-- * Convenience

-- | @POST@ job's @config.xml@ (in @xml-conduit@ format)
postXML :: (forall f. Method Complete f) -> Document -> Jenkins ()
postXML m =
  with (requestHeaders <>~ [("Content-Type", "text/xml")]) . post m . renderLBS def
{-# INLINE postXML #-}

-- | Reload jenkins configuration from disk
reload :: Jenkins ()
reload = post_ "reload"
{-# INLINE reload #-}

-- | Restart jenkins
restart :: Jenkins ()
restart = post_ "restart"
{-# INLINE restart #-}


type Host     = String
type Port     = Int
type User     = B.ByteString
type Password = B.ByteString
type APIToken = B.ByteString


-- | Jenkins settings
data Settings = Settings
  { _jenkins_host      :: Host
  , _jenkins_port      :: Port
  , _jenkins_user      :: User
  , _jenkins_api_token :: APIToken
  }  deriving (Show)

instance Default Settings where
  def = Settings
    { _jenkins_host      = "http://example.com"
    , _jenkins_port      = 80
    , _jenkins_user      = "anonymous"
    , _jenkins_api_token = "secret"
    }

makeLenses ''Settings


-- | Communicate with Jenkins REST API. Catches all exceptions.
jenkins :: Settings -> Jenkins a -> IO (Either SomeException a)
jenkins (Settings h p u t) jenk = try . withManager $ \manager -> do
  req <- liftIO $ parseUrl h
  let req' = req
        & L.port            .~ p
        & L.responseTimeout .~ Just (20 * 1000000)
  runReaderT (runIO manager jenk) (applyBasicAuth u t req')

runIO :: Manager -> Jenkins a -> ReaderT (Request (ResourceT IO)) (ResourceT IO) a
runIO manager = iterM go . unJenkins where
  go (Get m next) = do
    req <- ask
    let req' = req
          & L.path   %~ (`slash` render m)
          & L.method .~ "GET"
    bs <- lift $ httpLbs req' manager
    next (responseBody bs)
  go (Post m body next) = do
    req <- ask
    let req' = req
          & L.path          %~ (`slash` render m)
          & L.method        .~ "POST"
          & L.requestBody   .~ RequestBodyLBS body
          & L.redirectCount .~ 0
          & L.checkStatus   .~ \s@(Status st _) hs cookie_jar ->
            if 200 <= st && st < 400
                then Nothing
                else Just . toException $ StatusCodeException s hs cookie_jar
    res <- lift $ httpLbs req' manager
    next (responseBody res)
  go (Conc js next) = do
    xs <- liftWith $ \run ->
           liftWith $ \run' ->
             mapConcurrently (run' . run . runIO manager) js
    ys <- mapM (restoreT . restoreT . return) xs
    next ys
  go (IO action) = do
    next <- liftIO action
    next
  go (With f j next) = do
    res <- local f (runIO manager j)
    next res
