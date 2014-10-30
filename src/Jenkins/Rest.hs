{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Jenkins REST API interface
--
-- This module is intended to be imported qualified.
module Jenkins.Rest
  ( -- * Query Jenkins
    run
  , JenkinsT
  , Jenkins
  , Result(..)
  , HasMaster(..)
  , Master
  , defaultMaster
    -- ** Combinators
  , get
  , post
  , post_
  , orElse
  , locally
  , disconnect
    -- ** Method
  , module Jenkins.Rest.Method
    -- ** Concurrency
  , concurrently
  , Jenkins.Rest.traverse
  , Jenkins.Rest.traverse_
    -- ** Convenience
  , postXml
  , reload
  , restart
  , forceRestart
    -- * Optics
  , _Exception
  , _Disconnect
  , _Ok
  , JenkinsException(..)
    -- * Reexports
  , liftIO
  , Request
  ) where

import           Control.Applicative ((<$))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch (MonadCatch, try)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Data (Data, Typeable)
import qualified Data.Foldable as F
import           Data.Monoid (mempty)
import           Data.Text (Text)
import           Network.HTTP.Client (Request)
import           Text.XML (Document, renderLBS, def)

import           Jenkins.Rest.Internal
import           Jenkins.Rest.Method
import           Jenkins.Rest.Method.Internal
import           Network.HTTP.Client.Lens


-- | Run a 'JenkinsT' action
--
-- A successful 'Result' is either @'Disconnect'@ or @'Ok' v@
--
-- If a 'JenkinsException' is thrown by performing a request to Jenkins,
-- 'runJenkins' will catch and wrap it in @'Exception'@. Other exceptions
-- will propagate further
run
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, HasMaster t)
  => t -> JenkinsT m a -> m (Result a)
run c jenk =
  either Exception (maybe Disconnect Ok)
 `liftM`
  try (runInternal (c^.url) (c^.user) (c^.apiToken) jenk)

-- | A handy type synonym for the kind of 'JenkinsT' actions that's used the most
type Jenkins = JenkinsT IO

-- | The result of Jenkins REST API queries
data Result v =
    Exception JenkinsException
    -- ^ Exception was thrown while making requests to Jenkins
  | Disconnect
    -- ^ The client was explicitly disconnected by the user
  | Ok v
    -- ^ The result of uninterrupted execution of a 'JenkinsT' value
    deriving (Show, Typeable)

-- | A prism into Jenkins error
_Exception :: Prism (Result a) (Result a) JenkinsException JenkinsException
_Exception = prism' Exception $ \case
  Exception e -> Just e
  _           -> Nothing
{-# INLINE _Exception #-}

-- | A prism into disconnect
_Disconnect :: Prism' (Result a) ()
_Disconnect = prism' (\_ -> Disconnect) $ \case
  Disconnect -> Just ()
  _          -> Nothing
{-# INLINE _Disconnect #-}
{-# ANN _Disconnect ("HLint: ignore Use const" :: String) #-}

-- | A prism into result
_Ok :: Prism (Result a) (Result b) a b
_Ok = prism Ok $ \case
  Exception e -> Left (Exception e)
  Disconnect  -> Left Disconnect
  Ok a        -> Right a
{-# INLINE _Ok #-}

-- | Jenkins master node connection settings
class HasMaster t where
  master :: Lens' t Master

  -- | Jenkins master node URL
  url :: HasMaster t => Lens' t String
  url = master . \f x ->  f (_jenkinsUrl x) <&> \p -> x { _jenkinsUrl = p }
  {-# INLINE url #-}

  -- | Jenkins user
  user :: HasMaster t => Lens' t Text
  user = master . \f x -> f (_jenkinsUser x) <&> \p -> x { _jenkinsUser = p }
  {-# INLINE user #-}

  -- | Jenkins user's password or API token
  apiToken :: HasMaster t => Lens' t Text
  apiToken = master . \f x -> f (_jenkinsApiToken x) <&> \p -> x { _jenkinsApiToken = p }
  {-# INLINE apiToken #-}

-- | Jenkins master node connection settings token
data Master = Master
  { _jenkinsUrl      :: String -- ^ Jenkins URL
  , _jenkinsUser     :: Text   -- ^ Jenkins user
  , _jenkinsApiToken :: Text   -- ^ Jenkins user API token or password
  } deriving (Show, Eq, Typeable, Data)

instance HasMaster Master where
  master = id
  {-# INLINE master #-}

-- | Default Jenkins master node connection settings token
--
-- @
-- 'view' 'url'      defaultConnectInfo = \"http:\/\/example.com\/jenkins\"
-- 'view' 'user'     defaultConnectInfo = \"jenkins\"
-- 'view' 'apiToken' defaultConnectInfo = \"secret\"
-- @
defaultMaster :: Master
defaultMaster = Master
  { _jenkinsUrl      = "http://example.com/jenkins"
  , _jenkinsUser     = "jenkins"
  , _jenkinsApiToken = "secret"
  }


-- | Perform a @GET@ request
--
-- While the return type is the /lazy/ @Bytestring@, the entire response
-- sits in the memory anyway: lazy I/O is not used at the least
get :: Formatter f -> (forall g. Method Complete g) -> JenkinsT m Lazy.ByteString
get (Formatter f) m = liftJ (Get (f m) id)

-- | Perform a @POST@ request
post :: (forall f. Method Complete f) -> Lazy.ByteString -> JenkinsT m ()
post m body = liftJ (Post m body ())

-- | Perform a @POST@ request without payload
post_ :: (forall f. Method Complete f) -> JenkinsT m ()
post_ m = post m mempty

-- | @orElse a b@ runs @a@ and only runs @b@ if @a@ has thrown a @JenkinsException@
orElse :: JenkinsT m a -> JenkinsT m a -> JenkinsT m a
orElse ja jb = liftJ (Or ja jb)

-- | @locally f x@ modifies the base 'Request' with @f@ for the execution of @x@
-- (think 'Control.Monad.Trans.Reader.local')
--
-- This is useful for setting the appropriate headers, response timeouts and the like
locally :: (Request -> Request) -> JenkinsT m a -> JenkinsT m a
locally f j = liftJ (With f j id)

-- | Disconnect from Jenkins. No following actions will be executed.
disconnect :: JenkinsT m a
disconnect = liftJ Dcon


-- | Run two actions concurrently
concurrently :: JenkinsT m a -> JenkinsT m b -> JenkinsT m (a, b)
concurrently ja jb = liftJ (Conc ja jb (,))

-- | Map every list element to an action, run them concurrently and collect the results
--
-- @'traverse' : 'Data.Traversable.traverse' :: 'concurrently' : 'Control.Applicative.liftA2' (,)@
traverse :: (a -> JenkinsT m b) -> [a] -> JenkinsT m [b]
traverse f = foldr go (return [])
 where
  go x xs = do (y, ys) <- concurrently (f x) xs; return (y : ys)

-- | Map every list element to an action and run them concurrently ignoring the results
--
-- @'traverse_' : 'Data.Foldable.traverse_' :: 'concurrently' : 'Control.Applicative.liftA2' (,)@
traverse_ :: F.Foldable f => (a -> JenkinsT m b) -> f a -> JenkinsT m ()
traverse_ f = F.foldr (\x xs -> () <$ concurrently (f x) xs) (return ())


-- | Perform a @POST@ request to Jenkins with the XML document
--
-- Sets up the correct @Content-Type@ header. Mostly useful for updating @config.xml@
-- files for jobs, views, etc
postXml :: (forall f. Method Complete f) -> Document -> JenkinsT m ()
postXml m = locally (requestHeaders <>~ [("Content-Type", "text/xml")]) . post m . renderLBS def

-- | Reload jenkins configuration from disk
--
-- Calls @/reload@ and disconnects
reload :: JenkinsT m a
reload = do post_ "reload"; disconnect

-- | Restart jenkins safely
--
-- Calls @/safeRestart@ and /disconnects/
--
-- @/safeRestart@ allows all running jobs to complete
restart :: JenkinsT m a
restart = do post_ "safeRestart"; disconnect

-- | Restart jenkins
--
-- Calls @/restart@ and /disconnects/
--
-- @/restart@ restart Jenkins immediately, without waiting for the completion of
-- the building and/or waiting jobs
forceRestart :: JenkinsT m a
forceRestart = do post_ "restart"; disconnect
