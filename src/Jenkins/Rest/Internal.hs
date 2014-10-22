{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Jenkins REST API interface internals
module Jenkins.Rest.Internal where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted (concurrently)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch (MonadCatch, Exception(..), catch, throwM)
import           Control.Monad.Free.Church (liftF)
import           Control.Monad.Error (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (MonadReader(..))
import           Control.Monad.State (MonadState(..))
import           Control.Monad.Trans.Free.Church (FT, iterTM)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl(..))
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.Resource (ResourceT, MonadResource)
import           Control.Monad.Trans.Maybe (MaybeT(..), mapMaybeT)
import           Control.Monad.Writer (MonadWriter(..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as Strict
import           Data.Conduit (ResumableSource)
import qualified Data.Conduit as C
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable)
import           Network.HTTP.Conduit
import           Network.HTTP.Types (Status(..))

import           Jenkins.Rest.Method.Internal (Method, Type(..), render, slash)
import qualified Network.HTTP.Conduit.Lens as Lens

{-# ANN module ("HLint: ignore Use join" :: String) #-}


-- | The value of this type describes Jenkins REST API requests sequence
newtype JenkinsT m a = JenkinsT { unJenkinsT :: FT (JenkinsF m) m a }
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


-- | Jenkins REST API query
data JenkinsF m a where
  Get
    :: Method Complete f
    -> (((MonadResource m, MonadCatch m) => m (ResumableSource m Strict.ByteString)) -> a)
    -> JenkinsF n a
  Post :: (forall f. Method Complete f) -> ByteString -> a -> JenkinsF m a
  Conc :: JenkinsT m a -> JenkinsT m b -> (a -> b -> c) -> JenkinsF m c
  Or   :: JenkinsT m a -> JenkinsT m a -> JenkinsF m a
  With :: (Request -> Request) -> JenkinsT m b -> (b -> a) -> JenkinsF m a
  Dcon :: JenkinsF m a

instance Functor (JenkinsF m) where
  fmap f (Get  m g)      = Get  m      (f . g)
  fmap f (Post m body a) = Post m body (f a)
  fmap f (Conc m n g)    = Conc m n    (\a b -> f (g a b))
  fmap f (Or a b)        = Or (fmap f a) (fmap f b)
  fmap f (With h j g)    = With h j    (f . g)
  fmap _ Dcon            = Dcon

-- | Lift 'JenkinsF' to 'JenkinsT'
liftJ :: JenkinsF m a -> JenkinsT m a
liftJ = JenkinsT . liftF


newtype JenkinsException
  = JenkinsHttpException HttpException
    deriving (Show, Typeable)

instance Exception JenkinsException


runJenkinsInternal
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => String -> Text -> Text -> JenkinsT m a -> m (Maybe a)
runJenkinsInternal h user token jenk = do
  url <- wrapJenkinsException (parseUrl h)
  withManager $ \m ->
    Reader.runReaderT (runMaybeT (runInterpT (iterJenkinsIO m jenk)))
      . applyBasicAuth (Text.encodeUtf8 user) (Text.encodeUtf8 token)
      $ url


newtype InterpT m a = InterpT
  { runInterpT :: MaybeT (ReaderT Request (ResourceT m)) a
  } deriving (Functor)

instance (Functor m, Monad m) => Applicative (InterpT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (InterpT m) where
  return = InterpT . return
  InterpT m >>= k = InterpT (m >>= runInterpT . k)

instance (Functor m, Monad m) => Alternative (InterpT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (InterpT m) where
  mzero = InterpT mzero
  InterpT x `mplus` InterpT y = InterpT (x `mplus` y)

instance MonadTrans InterpT where
  lift = InterpT . lift . lift . lift

-- | Interpret 'JenkinsF' AST in 'IO'
iterJenkinsIO :: (MonadIO m, MonadBaseControl IO m, MonadCatch m) => Manager -> JenkinsT m a -> InterpT m a
iterJenkinsIO manager = iterJenkins (interpreter manager)

-- | Tear down 'JenkinsF' AST with a 'JenkinsF'-algebra
iterJenkins
  :: (Monad m, Monad (t m), MonadTrans t)
  => (JenkinsF m (t m a) -> t m a) -> JenkinsT m a -> t m a
iterJenkins go = iterTM go . unJenkinsT

-- | 'JenkinsF' AST interpreter
interpreter
  :: forall m a. (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => Manager
  -> JenkinsF m (InterpT m a) -> InterpT m a
interpreter man = go where
  go :: JenkinsF m (InterpT m a) -> InterpT m a
  go (Get m next) = InterpT $ do
    req <- lift Reader.ask
    runInterpT (next (wrapJenkinsException (liftM responseBody (http (prepareGet m req) man))))
  go (Post m body next) = InterpT $ do
    req <- lift Reader.ask
    res <- lift . lift $ wrapJenkinsException (http (preparePost m body req) man)
    ()  <- lift . lift $ C.closeResumableSource (responseBody res)
    runInterpT next
  go (Conc ja jb next) = do
    (a, b) <- intoM man $ \run -> concurrently (run ja) (run jb)
    c      <- outoM (return a)
    d      <- outoM (return b)
    next c d
  go (Or ja jb) = do
    res  <- intoM man $ \run -> run ja `catch` \(JenkinsHttpException _) -> run jb
    next <- outoM (return res)
    next
  go (With f jenk next) = InterpT $ do
    res <- mapMaybeT (Reader.local f) (runInterpT (iterJenkinsIO man jenk))
    runInterpT (next res)
  go Dcon = mzero

intoM
  :: forall m a. (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => Manager
  -> ((forall b. JenkinsT m b -> m (StT ResourceT (StT (ReaderT Request) (StT MaybeT b)))) -> m a)
  -> InterpT m a
intoM m f = InterpT $
  liftWith $ \run' -> liftWith $ \run'' -> liftWith $ \run''' ->
    let
      run :: JenkinsT m t -> m (StT ResourceT (StT (ReaderT Request) (StT MaybeT t)))
      run = run''' . run'' . run' . runInterpT . iterJenkinsIO m
    in
      f run

outoM :: Monad m => m (StT ResourceT (StT (ReaderT Request) (StT MaybeT b))) -> InterpT m b
outoM = InterpT . restoreT . restoreT . restoreT

prepareGet :: Method Complete f -> Request -> Request
prepareGet m = set Lens.method "GET" . over Lens.path (`slash` render m)

preparePost :: Method Complete f -> ByteString -> Request -> Request
preparePost m body =
    set Lens.checkStatus statusCheck
  . set Lens.redirectCount 0
  . set Lens.requestBody (RequestBodyLBS body)
  . set Lens.method "POST"
  . over Lens.path (`slash` render m)
 where
  statusCheck s@(Status st _) hs cookie_jar =
    if 200 <= st && st < 400 then Nothing else Just . toException $ StatusCodeException s hs cookie_jar

wrapJenkinsException :: MonadCatch m => m a -> m a
wrapJenkinsException m = m `withException` JenkinsHttpException

withException :: (MonadCatch m, Exception e, Exception e') => m a -> (e -> e') -> m a
withException m f = m `catch` \e -> throwM (f e)
