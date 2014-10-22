{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Jenkins REST API interface
module Jenkins.Rest
  ( -- * Query Jenkins
    runJenkins
  , JenkinsT
  , Jenkins
  , Result(..)
  , HasConnectInfo(..)
  , ConnectInfo
  , defaultConnectInfo
    -- ** Combinators
  , get
  , post
  , post_
  , concurrently
  , orElse
  , liftIO
  , with
    -- *** Low-level
  , getS
    -- ** Method
  , module Jenkins.Rest.Method
    -- ** Convenience
  , postXml
  , traverseC
  , traverseC_
  , reload
  , restart
  , forceRestart
    -- * Optics
  , _Error
  , _Disconnect
  , _Result
  , JenkinsException(..)
    -- * Reexports
  , Request
  ) where

import           Control.Applicative ((<$))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch (MonadCatch, try)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Conduit (ResumableSource, ($$+-))
import qualified Data.Conduit.List as CL
import           Data.Data (Data, Typeable)
import qualified Data.Foldable as F
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Network.HTTP.Conduit (Request)
import           Text.XML (Document, renderLBS, def)

import           Jenkins.Rest.Internal
import           Jenkins.Rest.Method
import           Jenkins.Rest.Method.Internal
import           Network.HTTP.Conduit.Lens


-- | Query Jenkins API using 'Jenkins' description
--
-- Successful result is either 'Disconnect' or @ 'Result' v @
--
-- If 'HttpException' was thrown by @http-conduit@, 'runJenkins' catches it
-- and wraps in 'Error'. Other exceptions are /not/ catched
runJenkins
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, HasConnectInfo t)
  => t -> JenkinsT m a -> m (Result JenkinsException a)
runJenkins c jenk =
  either Error (maybe Disconnect Result)
 `liftM`
  try (runJenkinsInternal (c^.jenkinsUrl) (c^.jenkinsUser) (c^.jenkinsApiToken) jenk)

-- | A handy type synonym for the kind of 'JenkinsT' actions that's used the most
type Jenkins = JenkinsT IO

-- | The result of Jenkins REST API queries
data Result e v =
    Error e    -- ^ Exception @e@ was thrown while querying Jenkins
  | Disconnect -- ^ The client was explicitly disconnected
  | Result v   -- ^ Querying successfully finished the with value @v@
    deriving (Show, Eq, Ord, Typeable, Data)

-- | A prism into Jenkins error
_Error :: Prism (Result e a) (Result e' a) e e'
_Error = prism Error $ \case
  Error e    -> Right e
  Disconnect -> Left Disconnect
  Result a   -> Left (Result a)
{-# INLINE _Error #-}

-- | A prism into disconnect
_Disconnect :: Prism' (Result e a) ()
_Disconnect = prism' (\_ -> Disconnect) $ \case
  Disconnect -> Just ()
  _          -> Nothing
{-# INLINE _Disconnect #-}
{-# ANN _Disconnect ("HLint: ignore Use const" :: String) #-}

-- | A prism into result
_Result :: Prism (Result e a) (Result e b) a b
_Result = prism Result $ \case
  Error e    -> Left (Error e)
  Disconnect -> Left Disconnect
  Result a   -> Right a
{-# INLINE _Result #-}

-- | Jenkins connection settings
data ConnectInfo = ConnectInfo
  { _jenkinsUrl      :: String -- ^ Jenkins URL, e.g. @http:\/\/example.com\/jenkins@
  , _jenkinsUser     :: Text   -- ^ Jenkins user, e.g. @jenkins@
  , _jenkinsApiToken :: Text   -- ^ Jenkins user API token or password
  } deriving (Show, Eq, Typeable, Data)

-- | Default Jenkins connection settings
--
-- @
-- defaultConnectInfo = ConnectInfo
--   { _jenkinsUrl      = \"http:\/\/example.com\/jenkins\"
--   , _jenkinsUser     = \"jenkins\"
--   , _jenkinsApiToken = \"\"
--   }
-- @
defaultConnectInfo :: ConnectInfo
defaultConnectInfo = ConnectInfo
  { _jenkinsUrl      = "http://example.com/jenkins"
  , _jenkinsUser     = "jenkins"
  , _jenkinsApiToken = ""
  }

-- | Convenience class aimed at elimination of long
-- chains of lenses to access jenkins connection configuration
--
-- For example, if you have a configuration record in your application:
--
-- @
-- data Config = Config
--   { ...
--   , _jenkinsConnectInfo :: ConnectInfo
--   , ...
--   }
-- @
--
-- you can make it an instance of 'HasConnectInfo':
--
-- @
-- instance HasConnectInfo Config where
--   connectInfo f x = (\p -> x { _jenkinsConnectInfo = p }) \<$\> f (_jenkinsConnectInfo x)
-- @
--
-- and then use e.g. @view jenkinsUrl config@ to get the url part of the jenkins connection
class HasConnectInfo t where
  connectInfo :: Lens' t ConnectInfo

  -- | A lens into Jenkins URL
  jenkinsUrl :: HasConnectInfo t => Lens' t String
  jenkinsUrl = connectInfo . \f x ->  f (_jenkinsUrl x) <&> \p -> x { _jenkinsUrl = p }
  {-# INLINE jenkinsUrl #-}

  -- | A lens into username to access the Jenkins instance with
  jenkinsUser :: HasConnectInfo t => Lens' t Text
  jenkinsUser = connectInfo . \f x -> f (_jenkinsUser x) <&> \p -> x { _jenkinsUser = p }
  {-# INLINE jenkinsUser #-}

  -- | A lens into user API token or password
  jenkinsApiToken :: HasConnectInfo t => Lens' t Text
  jenkinsApiToken = connectInfo . \f x -> f (_jenkinsApiToken x) <&> \p -> x { _jenkinsApiToken = p }
  {-# INLINE jenkinsApiToken #-}

instance HasConnectInfo ConnectInfo where
  connectInfo = id
  {-# INLINE connectInfo #-}

-- | @GET@ query
--
-- While the return type is the lazy @Bytestring@, the entire response
-- sits in the memory anyway: lazy I/O is not used at the least
get :: MonadIO m => Formatter f -> (forall g. Method Complete g) -> JenkinsT m Lazy.ByteString
get f m = do
  ms <- getS f m
  liftIO $ fmap Lazy.fromChunks . runResourceT $ do
    s <- ms
    s $$+- CL.consume

-- |
--
-- 'getS' prepares an action to run to make a @GET@ query to the Jenkins instance.
-- The function provides an option of tight control over sending queries and consuming responses;
-- unless you really need it, you'll be better served by the simpler 'get' function
--
-- /Note:/ if you don't close the source eventually (either explicitly with
-- 'Data.Conduit.closeResumableSource' or implicitly by reading from it)
-- it will leak a socket.
getS
  :: (MonadCatch n, MonadResource n)
  => Formatter f
  -> (forall g. Method Complete g)
  -> JenkinsT m (n (ResumableSource n Strict.ByteString))
getS (Formatter f) m = liftJ (Get (f m) (\x -> x))
{-# ANN getS ("HLint: ignore Use id" :: String) #-}

-- | @POST@ query (with a payload)
post :: (forall f. Method Complete f) -> Lazy.ByteString -> JenkinsT m ()
post m body = liftJ (Post m body ())

-- | @POST@ query (without payload)
post_ :: (forall f. Method Complete f) -> JenkinsT m ()
post_ m = post m mempty

-- | Do both queries 'concurrently'
concurrently :: JenkinsT m a -> JenkinsT m b -> JenkinsT m (a, b)
concurrently ja jb = liftJ (Conc ja jb (,))

-- | @orElse a b@ runs @a@ and only runs @b@ if @a@ has thrown a @JenkinsException@
orElse :: JenkinsT m a -> JenkinsT m a -> JenkinsT m a
orElse ja jb = liftJ (Or ja jb)

-- | Make local changes to the 'Request'
with :: (Request -> Request) -> JenkinsT m a -> JenkinsT m a
with f j = liftJ $ With f j id

-- | @POST@ job's @config.xml@ (or any other xml, really) in @xml-conduit@ format
postXml :: (forall f. Method Complete f) -> Document -> JenkinsT m ()
postXml m = with (requestHeaders <>~ [("Content-Type", "text/xml")]) . post m . renderLBS def

-- | Make a bunch of queries 'concurrently'
traverseC :: (a -> JenkinsT m b) -> [a] -> JenkinsT m [b]
traverseC f = foldr go (return [])
 where
  go x xs = do (y, ys) <- concurrently (f x) xs; return (y : ys)

-- | Make a bunch of queries 'concurrently' ignoring their results
traverseC_ :: F.Foldable f => (a -> JenkinsT m b) -> f a -> JenkinsT m ()
traverseC_ f = F.foldr (\x xs -> () <$ concurrently (f x) xs) (return ())

-- | Reload jenkins configuration from disk
--
-- Calls @/reload@ and disconnects
reload :: JenkinsT m a
reload = do post_ "reload"; disconnect

-- | Restart jenkins safely
--
-- Calls @/safeRestart@ and disconnects
--
-- @/safeRestart@ allows all running jobs to complete
restart :: JenkinsT m a
restart = do post_ "safeRestart"; disconnect

-- | Force jenkins to restart without waiting for running jobs to finish
--
-- Calls @/restart@ and disconnects
forceRestart :: JenkinsT m a
forceRestart = do post_ "restart"; disconnect

-- Disconnect from Jenkins. Any following queries won't be executed
disconnect :: JenkinsT m a
disconnect = liftJ Dcon
{-# INLINE disconnect #-}
