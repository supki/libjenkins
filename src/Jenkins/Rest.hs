{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Jenkins REST API interface
module Jenkins.Rest
  ( -- * Query Jenkins
    runJenkins, ConnectInfo(..), defaultConnectInfo, Jenkins, Result(..)
    -- ** Combinators
  , get, post, post_, concurrently, io, disconnect, with
    -- ** Method
  , module Jenkins.Rest.Method
    -- ** Convenience
  , postXML, concurrentlys, concurrentlys_, reload, restart, forceRestart
    -- * Lensy things
  , jenkinsUrl, jenkinsPort, jenkinsUser, jenkinsApiToken, jenkinsPassword
  , _Error, _Disconnect, _Result
    -- * Type reexports
  , Request, HttpException
  ) where

import Control.Applicative ((<$))
import Data.Foldable (Foldable, foldr)
import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mempty)
import Network.HTTP.Conduit (Request, HttpException)
import Prelude hiding (foldr)
import Text.XML (Document, renderLBS, def)

import Jenkins.Rest.Internal
import Jenkins.Rest.Method
import Network.HTTP.Conduit.Lens

{-# ANN module ("HLint: ignore Use const" :: String) #-}


-- | @GET@ query
get :: Method Complete f -> Jenkins ByteString
get m = liftJ $ Get m id
{-# INLINE get #-}

-- | @POST@ query (with a payload)
post :: (forall f. Method Complete f) -> ByteString -> Jenkins ()
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
-- Any following queries won't be executed
disconnect :: Jenkins a
disconnect = liftJ Dcon
{-# INLINE disconnect #-}

-- | Make local changes to the 'Request'
with :: (Request -> Request) -> Jenkins a -> Jenkins a
with f j = liftJ $ With f j id
{-# INLINE with #-}


-- | @POST@ job's @config.xml@ (or any other xml, really) in @xml-conduit@ format
postXML :: (forall f. Method Complete f) -> Document -> Jenkins ()
postXML m =
  with (requestHeaders <>~ [("Content-Type", "text/xml")]) . post m . renderLBS def
{-# INLINE postXML #-}

-- | Send a list of queries 'concurrently'
concurrentlys :: Foldable f => f (Jenkins a) -> Jenkins [a]
concurrentlys = foldr go (return [])
 where
  go x xs = do
    (y, ys) <- concurrently x xs
    return (y : ys)
{-# INLINE concurrentlys #-}

-- | Send a list of queries 'concurrently' ignoring their results
--
-- /Note/: exceptions are still raised
concurrentlys_ :: Foldable f => f (Jenkins a) -> Jenkins ()
concurrentlys_ = foldr (\x xs -> () <$ concurrently x xs) (return ())
{-# INLINE concurrentlys_ #-}

-- | Reload jenkins configuration from disk
--
-- Calls @/reload@ and disconnects
reload :: Jenkins a
reload = do
  post_ "reload"
  disconnect
{-# INLINE reload #-}

-- | Restart jenkins safely
--
-- Calls @/safeRestart@ and disconnects
--
-- @/safeRestart@ allows all running jobs to complete
restart :: Jenkins a
restart = do
  post_ "safeRestart"
  disconnect
{-# INLINE restart #-}

-- | Force jenkins to restart without waiting for running jobs to finish
--
-- Calls @/restart@ and disconnects
forceRestart :: Jenkins a
forceRestart = do
  post_ "restart"
  disconnect
{-# INLINE forceRestart #-}
