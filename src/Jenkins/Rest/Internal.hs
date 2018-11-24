{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Jenkins REST API interface internals
module Jenkins.Rest.Internal
  ( JenkinsT(..)
  , liftJ
  , runInternal
  , JF(..)
  , JenkinsException(..)
  , iter
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Unlifted
import           Control.Exception (Exception(..), SomeException, throwIO)
import qualified Control.Exception as Unlifted
import           Control.Monad
import           Control.Monad.Free.Church (liftF)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.State (MonadState(..))
import           Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import           Control.Monad.Trans.Control (MonadBaseControl(..), control, liftBaseOp_)
import           Control.Monad.Trans.Free.Church (FT, iterTM)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Writer (MonadWriter(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
#if (MIN_VERSION_conduit(1,3,0)) || (MIN_VERSION_http_conduit(2,3,0))
import           Data.Conduit (ConduitM)
#else
import           Data.Conduit (ResumableSource)
#endif
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable)
import           Network.HTTP.Conduit (Request, HttpException)
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Client as Http (brReadSome)
import qualified Network.HTTP.Client.Internal as Http (throwHttp)
import           Network.HTTP.Types (Status(..))

import           Jenkins.Rest.Method.Internal (Method, Type(..), render, slash)

{-# ANN module ("HLint: ignore Use join" :: String) #-}

#if (MIN_VERSION_conduit(1,3,0)) || (MIN_VERSION_http_conduit(2,3,0))
type HttpSource m o = ConduitM () o m ()
#else
type HttpSource m o = ResumableSource m o
#endif

-- | The value of this type describes Jenkins REST API requests sequence
newtype JenkinsT m a = JenkinsT { unJenkinsT :: FT (JF m) m a }
  deriving (Functor)

instance MonadIO m => MonadIO (JenkinsT m) where
  liftIO = JenkinsT . liftIO

instance MonadTrans JenkinsT where
  lift = JenkinsT . lift

instance Applicative (JenkinsT m) where
  pure = JenkinsT . pure
  JenkinsT f <*> JenkinsT x = JenkinsT (f <*> x)

instance Monad (JenkinsT m) where
  return = JenkinsT . return
  JenkinsT m >>= k = JenkinsT (m >>= unJenkinsT . k)

instance MonadReader r m => MonadReader r (JenkinsT m) where
  ask = JenkinsT ask
  local f = JenkinsT . local f . unJenkinsT

instance MonadWriter w m => MonadWriter w (JenkinsT m) where
  tell = JenkinsT . tell
  listen = JenkinsT . listen . unJenkinsT
  pass = JenkinsT . pass . unJenkinsT
  writer = JenkinsT . writer

instance MonadState s m => MonadState s (JenkinsT m) where
  get = JenkinsT get
  put = JenkinsT . put
  state = JenkinsT . state

instance MonadError e m => MonadError e (JenkinsT m) where
  throwError = JenkinsT . throwError
  m `catchError` f = JenkinsT (unJenkinsT m `catchError` (unJenkinsT . f))


data JF :: (* -> *) -> * -> * where
  Get    :: Method 'Complete f -> (Lazy.ByteString -> a) -> JF m a
  Stream :: MonadResource m => Method 'Complete f -> (HttpSource m ByteString -> a) -> JF m a
  Post   :: (forall f. Method 'Complete f) -> Lazy.ByteString -> (Lazy.ByteString -> a) -> JF m a
  Conc   :: JenkinsT m a -> JenkinsT m b -> (a -> b -> c) -> JF m c
  Or     :: JenkinsT m a -> (JenkinsException -> JenkinsT m a) -> JF m a
  With   :: (Request -> Request) -> JenkinsT m b -> (b -> a) -> JF m a

instance Functor (JF m) where
  fmap f (Get m g)       = Get m (f . g)
  fmap f (Stream m g)    = Stream m (f . g)
  fmap f (Post m body g) = Post m body (f . g)
  fmap f (Conc m n g)    = Conc m n (\a b -> f (g a b))
  fmap f (Or a b)        = Or (fmap f a) (fmap f . b)
  fmap f (With h j g)    = With h j (f . g)

-- | Lift 'JF' to 'JenkinsT'
liftJ :: JF m a -> JenkinsT m a
liftJ = JenkinsT . liftF


-- | The kind of exceptions that can be thrown by performing requests
-- to the Jenkins REST API
newtype JenkinsException
  = JenkinsHttpException HttpException
    deriving (Show, Typeable)

instance Exception JenkinsException


runInternal
  :: (MonadIO m, MonadBaseControl IO m)
  => String -> Text -> Text -> JenkinsT m a -> m a
runInternal h user token jenk = do
  url <- liftIO (wrapException (Http.parseUrlThrow h))
  man <- liftIO (Http.newManager Http.tlsManagerSettings)
  runInterpT (iterInterpT man jenk)
             (Http.applyBasicAuth (Text.encodeUtf8 user) (Text.encodeUtf8 token) url)

newtype InterpT m a = InterpT
  { runInterpT :: Request -> m a
  } deriving (Functor)

instance (Functor m, Monad m) => Applicative (InterpT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (InterpT m) where
  return = InterpT . return . return
  InterpT m >>= k = InterpT (\req -> m req >>= \a -> runInterpT (k a) req)

instance MonadTrans InterpT where
  lift = InterpT . const

-- | Interpret the 'JF' AST in 'InterpT'
iterInterpT :: (MonadIO m, MonadBaseControl IO m) => Http.Manager -> JenkinsT m a -> InterpT m a
iterInterpT manager = iter (interpreter manager)

-- | Tear down the 'JF' AST with a 'JF'-algebra
iter
  :: (Monad m, Monad (t m), MonadTrans t)
  => (JF m (t m a) -> t m a) -> JenkinsT m a -> t m a
iter go = iterTM go . unJenkinsT

-- | 'JF' AST interpreter
interpreter
  :: forall m a. (MonadIO m, MonadBaseControl IO m)
  => Http.Manager
  -> JF m (InterpT m a) -> InterpT m a
interpreter man = go where
  go :: JF m (InterpT m a) -> InterpT m a
  go (Get m next) = InterpT $ \req -> do
    res <- oneshotReq (prepareGet m req) man
    runInterpT (next res) req
  go (Stream m next) = InterpT $ \req -> do
    res <- streamReq (prepareGet m req) man
    runInterpT (next res) req
  go (Post m body next) = InterpT $ \req -> do
    res <- oneshotReq (preparePost m body req) man
    runInterpT (next res) req
  go (Conc ja jb next) = do
    (a, b) <- intoM man $ \run -> concurrently (run ja) (run jb)
    next a b
  go (Or ja jb) = do
    res <- intoM man $ \run -> run ja `catch` (run . jb)
    res
  go (With f jenk next) = InterpT $ \req -> do
    res <- runInterpT (iterInterpT man jenk) (f req)
    runInterpT (next res) req

oneshotReq :: MonadIO m => Request -> Http.Manager -> m Lazy.ByteString
oneshotReq req =
  liftIO . wrapException . fmap Http.responseBody . Http.httpLbs req

streamReq
  :: (MonadBaseControl IO m, MonadResource m)
  => Request -> Http.Manager -> m (HttpSource m ByteString)
streamReq req =
  wrapException . fmap Http.responseBody . Http.http req

intoM
  :: forall m a. (MonadIO m, MonadBaseControl IO m)
  => Http.Manager
  -> ((forall b. JenkinsT m b -> m b) -> m a)
  -> InterpT m a
intoM m f = InterpT $ \req -> f (\x -> runInterpT (iterInterpT m x) req)

prepareGet :: Method 'Complete f -> Request -> Request
prepareGet m r = r
  { Http.method = "GET"
  , Http.path   = Http.path r `slash` render m
  }

preparePost :: Method 'Complete f -> Lazy.ByteString -> Request -> Request
preparePost m body r = r
  { Http.checkResponse = statusCheck
  , Http.redirectCount = 0
  , Http.requestBody   = Http.RequestBodyLBS body
  , Http.method        = "POST"
  , Http.path          = Http.path r `slash` render m
  }
 where
  statusCheck _req res =
    unless (200 <= st && st < 400) $ do
      chunk <- Http.brReadSome (Http.responseBody res) 1024
      Http.throwHttp (Http.StatusCodeException (() <$ res) (ByteString.Lazy.toStrict chunk))
   where
    Status st _ = Http.responseStatus res

wrapException :: (MonadBaseControl IO m, MonadIO m) => m a -> m a
wrapException m = m `catch` (liftIO . throwIO .  JenkinsHttpException)

concurrently :: (MonadBaseControl IO m, MonadIO m) => m a -> m b -> m (a, b)
concurrently ma mb =
  withAsync ma $ \a ->
  withAsync mb $ \b ->
  waitBoth a b
{-# INLINABLE concurrently #-}

withAsync :: (MonadBaseControl IO m, MonadIO m) => m a -> (Async (StM m a) -> m b) -> m b
withAsync action inner = mask $ \restore -> do
  a <- liftBaseWith (\magic -> Unlifted.async (magic (restore action)))
  r <- restore (inner a) `catch` \e ->
    liftIO (do Unlifted.cancel a; throwIO (e :: SomeException))
  liftIO (Unlifted.cancel a)
  return r
{-# INLINABLE withAsync #-}

waitBoth :: (MonadBaseControl IO m, MonadIO m) => Async (StM m a) -> Async (StM m b) -> m (a, b)
waitBoth aa ab = do
  (ma, mb) <- liftIO (Unlifted.waitBoth aa ab)
  a <- restoreM ma
  b <- restoreM mb
  return (a, b)
{-# INLINABLE waitBoth #-}

mask :: MonadBaseControl IO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = control $ \magic -> Unlifted.mask (\g -> magic (f (liftBaseOp_ g)))
{-# INLINABLE mask #-}

catch :: (MonadBaseControl IO m, Exception e) => m a -> (e -> m a) -> m a
catch m h = control (\magic -> Unlifted.catch (magic m) (magic . h))
{-# INLINABLE catch #-}
