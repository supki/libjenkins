{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Jenkins REST API interface
module Jenkins.REST
  ( -- * Query Jenkins
    runJenkins, ConnectInfo(..), defaultConnectInfo, Jenkins
    -- ** Combinators
  , get, post, post_, concurrently, io, disconnect, with
    -- ** Method
  , module Jenkins.REST.Method
    -- ** Convenience
  , postXML, concurrentlys, concurrentlys_, reload, restart, forceRestart
    -- * Lenses
  , jenkinsUrl, jenkinsPort, jenkinsUser, jenkinsApiToken, jenkinsPassword
  , module L
    -- * Misc
  , Request, HttpException
  ) where

import           Control.Applicative ((<$))
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as BL
import           Data.Default (Default(..))
import           Data.Monoid (mempty)
import           Network.HTTP.Conduit (Request, HttpException)
import           Text.XML (Document, renderLBS)

import           Jenkins.REST.Internal
import           Jenkins.REST.Method
import qualified Network.HTTP.Conduit.Lens as L

{-# ANN module ("HLint: ignore Use const" :: String) #-}


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
with :: (Request -> Request) -> Jenkins a -> Jenkins a
with f j = liftJ $ With f j id
{-# INLINE with #-}


-- | @POST@ job's @config.xml@ (in @xml-conduit@ format)
postXML :: (forall f. Method Complete f) -> Document -> Jenkins ()
postXML m =
  with (L.requestHeaders <>~ [("Content-Type", "text/xml")]) . post m . renderLBS def
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

-- | Restart jenkins safely
--
-- Allows all running jobs to complete
restart :: Jenkins a
restart = do
  post_ "safeRestart"
  disconnect
{-# INLINE restart #-}

-- | Force jenkins to restart without waiting running jobs to finish
forceRestart :: Jenkins a
forceRestart = do
  post_ "restart"
  disconnect
{-# INLINE forceRestart #-}
