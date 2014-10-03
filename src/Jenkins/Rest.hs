{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Jenkins REST API interface
module Jenkins.Rest
  ( -- * Query Jenkins
    Jenkins
  , HasConnectInfo(..)
  , ConnectInfo(..)
  , defaultConnectInfo
  , Result(..)
  , runJenkins
    -- ** Combinators
  , get
  , getS
  , post
  , post_
  , concurrently
  , orElse
  , liftIO
  , with
    -- ** Method
  , module Jenkins.Rest.Method
    -- ** Convenience
  , postXML
  , concurrentlys
  , concurrentlys_
  , reload
  , restart
  , forceRestart
    -- * Optics
  , jenkinsUrl
  , jenkinsPort
  , jenkinsUser
  , jenkinsApiToken
  , jenkinsPassword
  , _Error
  , _Disconnect
  , _Result
  , JenkinsException(..)
    -- * Reexports
  , Request
  , HttpException
  ) where

import           Control.Applicative ((<$))
import           Data.Foldable (Foldable)
import qualified Data.Foldable as F
import           Control.Lens
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Conduit (ResumableSource, ($$+-))
import qualified Data.Conduit.List as CL
import           Data.Monoid (mempty)
import           Network.HTTP.Conduit (Request, HttpException)
import           Text.XML (Document, renderLBS, def)

import           Jenkins.Rest.Internal
import           Jenkins.Rest.Method
import           Network.HTTP.Conduit.Lens


-- | @GET@ query
--
-- While the return type is a lazy bytestring, the entire response
-- sits in memory anyway: lazy I/O is not used
get :: Method Complete f -> Jenkins Lazy.ByteString
get m = fmap Lazy.fromChunks . liftIO . runResourceT =<< liftJ (Get m ($$+- CL.consume))

-- | @GET@ query
--
-- If you don't close the source eventually (either explicitly with
-- 'Data.Conduit.closeResumableSource' or implicitly by reading from it)
-- it will leak a socket.
getS :: Method Complete f -> Jenkins (ResumableSource (ResourceT IO) Strict.ByteString)
getS m = liftJ (Get m id)

-- | @POST@ query (with a payload)
post :: (forall f. Method Complete f) -> Lazy.ByteString -> Jenkins ()
post m body = liftJ (Post m body ())

-- | @POST@ query (without payload)
post_ :: (forall f. Method Complete f) -> Jenkins ()
post_ m = post m mempty

-- | Do both queries 'concurrently'
concurrently :: Jenkins a -> Jenkins b -> Jenkins (a, b)
concurrently ja jb = liftJ (Conc ja jb (,))

-- | @orElse a b@ runs @a@ and only runs @b@ if @a@ has thrown a @JenkinsException@
orElse :: Jenkins a -> Jenkins a -> Jenkins a
orElse ja jb = liftJ (Or ja jb)

-- | Make local changes to the 'Request'
with :: (Request -> Request) -> Jenkins a -> Jenkins a
with f j = liftJ $ With f j id

-- | @POST@ job's @config.xml@ (or any other xml, really) in @xml-conduit@ format
postXML :: (forall f. Method Complete f) -> Document -> Jenkins ()
postXML m = with (requestHeaders <>~ [("Content-Type", "text/xml")]) . post m . renderLBS def

-- | Send a list of queries 'concurrently'
concurrentlys :: Foldable f => f (Jenkins a) -> Jenkins [a]
concurrentlys = F.foldr go (return [])
 where
  go x xs = do
    (y, ys) <- concurrently x xs
    return (y : ys)

-- | Send a list of queries 'concurrently' ignoring their results
--
-- /Note/: exceptions are still raised
concurrentlys_ :: Foldable f => f (Jenkins a) -> Jenkins ()
concurrentlys_ = F.foldr (\x xs -> () <$ concurrently x xs) (return ())

-- | Reload jenkins configuration from disk
--
-- Calls @/reload@ and disconnects
reload :: Jenkins a
reload = do post_ "reload"; disconnect

-- | Restart jenkins safely
--
-- Calls @/safeRestart@ and disconnects
--
-- @/safeRestart@ allows all running jobs to complete
restart :: Jenkins a
restart = do post_ "safeRestart"; disconnect

-- | Force jenkins to restart without waiting for running jobs to finish
--
-- Calls @/restart@ and disconnects
forceRestart :: Jenkins a
forceRestart = do post_ "restart"; disconnect

-- Disconnect from Jenkins. Any following queries won't be executed
disconnect :: Jenkins a
disconnect = liftJ Dcon
{-# INLINE disconnect #-}
