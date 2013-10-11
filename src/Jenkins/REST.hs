{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Interface to Jenkins REST API
module Jenkins.REST
  ( -- * Types
    Jenkins, Disconnect(..)
  , Settings(..), Host(..), Port(..), User(..), APIToken(..), Password
  , Request
    -- ** Lenses and prisms
  , jenkins_host, jenkins_port, jenkins_user, jenkins_api_token, jenkins_password
  , _Host, _Port, _User, _APIToken, _Password
    -- * Jenkins queries construction
  , get, post, post_, concurrently, io, disconnect, with
  , module Jenkins.REST.Method
    -- ** Little helpers
  , concurrentlys, concurrentlys_, postXML, reload, restart
    -- * Jenkins queries execution
  , runJenkins, runJenkinsP
    -- * Usable @http-conduit@ 'Request' type API
  , module Jenkins.REST.Lens
  ) where

import           Control.Applicative ((<$))
import           Control.Exception (handle)
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Data (Data, Typeable)
import           Data.Default (Default(..))
import           Data.Monoid (mempty)
import           Data.String (IsString)
import           GHC.Generics (Generic)
import           Network.HTTP.Conduit (Request, HttpException, withManager, applyBasicAuth, parseUrl)
import           Text.XML (Document, renderLBS)

import           Jenkins.REST.Internal
import qualified Jenkins.REST.Lens
import           Jenkins.REST.Lens as L
import           Jenkins.REST.Method

{-# ANN module ("HLint: Use const" :: String) #-}


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

-- | Do both queries 'concurrently'
concurrently :: Jenkins a -> Jenkins b -> Jenkins (a, b)
concurrently ja jb = liftJ $ Conc ja jb (,)
{-# INLINE concurrently #-}

-- | Lift arbitrary 'IO' action
io :: IO a -> Jenkins a
io = liftIO
{-# INLINE io #-}

-- | Disconnect from Jenkins
--
-- Following queries won't be executed
disconnect :: Jenkins a
disconnect = liftJ Dcon
{-# INLINE disconnect #-}

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

-- | Do a list of queries 'concurrently'
concurrentlys :: [Jenkins a] -> Jenkins [a]
concurrentlys = foldr go (return [])
 where
  go x xs = do
    (y, ys) <- concurrently x xs
    return (y : ys)
{-# INLINE concurrentlys #-}

-- | Do a list of queries 'concurrently' ignoring their results
concurrentlys_ :: [Jenkins a] -> Jenkins ()
concurrentlys_ = foldr (\x xs -> () <$ concurrently x xs) (return ())
{-# INLINE concurrentlys_ #-}

-- | Reload jenkins configuration from disk
reload :: Jenkins a
reload = do
  post_ "reload"
  disconnect
{-# INLINE reload #-}

-- | Restart jenkins
restart :: Jenkins a
restart = do
  post_ "restart"
  disconnect
{-# INLINE restart #-}


-- | Jenkins settings
--
-- '_jenkins_api_token' might as well be the user's password, Jenkins
-- does not make any distinction between these concepts
data Settings = Settings
  { _jenkins_host      :: Host
  , _jenkins_port      :: Port
  , _jenkins_user      :: User
  , _jenkins_api_token :: APIToken
  } deriving (Show, Eq, Typeable, Data, Generic)

instance Default Settings where
  def = Settings
    { _jenkins_host      = "http://example.com"
    , _jenkins_port      = 80
    , _jenkins_user      = "anonymous"
    , _jenkins_api_token = "secret"
    }

-- | Jenkins host address
newtype Host = Host { unHost :: String }
  deriving (Show, Eq, Typeable, Data, Generic, IsString)

-- | Jenkins port
newtype Port = Port { unPort :: Int }
  deriving (Show, Eq, Typeable, Data, Generic, Num)

-- | Jenkins user
newtype User = User { unUser :: B.ByteString }
  deriving (Show, Eq, Typeable, Data, Generic, IsString)

-- | Jenkins user API token
newtype APIToken = APIToken { unAPIToken :: B.ByteString }
  deriving (Show, Eq, Typeable, Data, Generic, IsString)

-- | Jenkins user password
type Password = APIToken

makeLenses ''Settings

jenkins_password :: Lens' Settings Password
jenkins_password = jenkins_api_token
{-# INLINE jenkins_password #-}

makePrisms ''Host
makePrisms ''Port
makePrisms ''User
makePrisms ''APIToken

_Password :: Prism' Password B.ByteString
_Password = _APIToken
{-# INLINE _Password #-}


data Disconnect =
    Disconnect
  | JenkinsException HttpException
    deriving (Show, Typeable, Generic)

-- | Communicate with Jenkins REST API
--
-- Catches 'HttpException's thrown by @http-conduit@, but
-- does not catch exceptions thrown by embedded 'IO' actions
runJenkins :: Settings -> Jenkins a -> IO (Either Disconnect a)
runJenkins (Settings (Host h) (Port p) (User u) (APIToken t)) jenk =
  handle (return . Left . JenkinsException) $
    withManager $ \manager -> do
      req <- liftIO $ parseUrl h
      let req' = req
            & L.port            .~ p
            & L.responseTimeout .~ Just (20 * 1000000)
      res <- runReaderT (runMaybeT (runJenkinsIO manager jenk)) (applyBasicAuth u t req')
      return $ maybe (Left Disconnect) Right res
