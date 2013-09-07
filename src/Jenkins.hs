{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -W #-}
module Jenkins where

import           Control.Lens
import           Control.Applicative (Applicative)
import           Control.Monad.Free
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (MonadResource)
import           Network.HTTP.Conduit
import qualified Network.HTTP.Conduit.Lens as L

import           Jenkins.REST.Method


newtype Jenk a = Jenk { unJenk :: Free JenkF a }
  deriving (Functor, Applicative, Monad)


data JenkF a =
    forall f. Get (Method f) (BL.ByteString -> a)
  | Post (forall f. (Method f)) (BL.ByteString -> a)

instance Functor JenkF where
  fmap f (Get  m g) = Get m  (f . g)
  fmap f (Post m g) = Post m (f . g)
  {-# INLINE fmap #-}


get :: Method f -> Jenk BL.ByteString
get m = Jenk . liftF $ Get m id

post :: (forall f. Method f) -> Jenk BL.ByteString
post m = Jenk . liftF $ Post m id


type Host     = String
type Port     = Int
type User     = B.ByteString
type Password = B.ByteString


withJenkins :: Host -> Port -> User -> Password -> Jenk a -> IO a
withJenkins host port user password jenk = withManager $ \manager -> do
  req <- liftIO $ parseUrl host
  interpret manager (applyBasicAuth user password (req { port })) jenk

interpret
  :: (io ~ IO, MonadBaseControl io m, MonadIO m, MonadResource m)
  => Manager -> Request m -> Jenk a -> m a
interpret manager request = iterM go . unJenk where
  go (Get m next) = do
    let request' = request
          & L.path   %~ (`combine` render m)
          & L.method .~ "GET"
    bs <- httpLbs request' manager
    next (responseBody bs)
  go (Post m next) = do
    let request' = request
          & L.path   %~ (`combine` render m)
          & L.method .~ "POST"
    bs <- httpLbs request' manager
    next (responseBody bs)
