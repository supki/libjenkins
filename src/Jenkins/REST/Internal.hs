{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Jenkins.REST.Internal where

import           Control.Applicative (Applicative(..))
import           Control.Concurrent.Async (concurrently)
import           Control.Exception (toException)
import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.Free.Church (F, iterM, liftF)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control (MonadTransControl(..))
import           Control.Monad.Trans.Reader (ReaderT, ask, local)
import           Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (ResourceT)
import           Network.HTTP.Conduit
import           Network.HTTP.Types (Status(..))

import           Jenkins.REST.Lens as L
import           Jenkins.REST.Method


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


runJenkinsIO
  :: Manager
  -> Jenkins a
  -> MaybeT (ReaderT Request (ResourceT IO)) a
runJenkinsIO manager = runJenkinsP (jenkinsIO manager)
{-# INLINE runJenkinsIO #-}

-- | Generic Jenkins REST API queries interpreter
--
-- Particularly useful for testing (with @m ≡ 'Identity'@)
runJenkinsP :: Monad m => (JenkinsF (m a) -> m a) -> Jenkins a -> m a
runJenkinsP go = iterM go . unJenkins
{-# INLINE runJenkinsP #-}

jenkinsIO
  :: Manager
  -> JenkinsF (MaybeT (ReaderT Request (ResourceT IO)) a)
  -> MaybeT (ReaderT Request (ResourceT IO)) a
jenkinsIO manager = go where
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
          run = run''' . run'' . run' . runJenkinsIO manager
      in concurrently (run jenka) (run jenkb)
    c <- restoreT . restoreT . restoreT $ return a
    d <- restoreT . restoreT . restoreT $ return b
    next c d
  go (IO action) = join (liftIO action)
  go (With f jenk next) = do
    res <- mapMaybeT (local f) (runJenkinsIO manager jenk)
    next res
  go Dcon = fail "disconnect"
