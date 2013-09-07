{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -W #-}
module Jenkins where

import           Control.Lens
import           Control.Applicative (Applicative)
import           Control.Monad.Free
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit (MonadResource)
import           Data.Monoid (Monoid(..), (<>))
import           Data.String (IsString(..))
import           Network.HTTP.Conduit
import qualified Network.HTTP.Conduit.Lens as L

-- $setup
-- >>> :set -XOverloadedStrings


newtype Jenk a = Jenk { unJenk :: (Free JenkF a) }
  deriving (Functor, Applicative, Monad)


data JenkF a =
    forall f. Get (Method f) (BL.ByteString -> a)
  | Post (forall f. (Method f)) (BL.ByteString -> a)

instance Functor JenkF where
  fmap f (Get  m g) = Get m  (f . g)
  fmap f (Post m g) = Post m (f . g)
  {-# INLINE fmap #-}

infix  6 :-, `as`
infixr 7 :/
data Method :: Format -> * where
  Empty  :: Method a
  Number :: Integer -> Method f
  String :: String -> Method f
  (:/)   :: Method f -> Method f -> Method f
  (:-)   :: Method f -> As f -> Method f

instance Num (Method f) where
  (+)         = error "Method.(+): not supposed to be used"
  (*)         = error "Method.(*): not supposed to be used"
  abs         = error "Method.abs: not supposed to be used"
  signum      = error "Method.signum: not supposed to be used"
  fromInteger = Number

instance IsString (Method f) where
  fromString = String

data Format = JSON | XML | Python

data As :: Format -> * where
  AsJSON   :: As JSON
  AsXML    :: As XML
  AsPython :: As Python

(-/-) :: Method f -> Method f -> Method f
(-/-) = (:/)

as :: Method f -> As f -> Method f
as = (:-)

json :: As JSON
json = AsJSON

xml :: As XML
xml = AsXML

python :: As Python
python = AsPython


-- |
--
-- >>> nicely ("build" :/ 7 :- AsXML)
-- "build/7/api/xml"
nicely :: Method f -> String
nicely Empty           = ""
nicely (Number n)      = show n
nicely (String s)      = s
nicely (x :/ y)        = nicely x `combine` nicely y
nicely (x :- AsJSON)   = nicely x `combine` "api" `combine` "json"
nicely (x :- AsXML)    = nicely x `combine` "api" `combine` "xml"
nicely (x :- AsPython) = nicely x `combine` "api" `combine` "python"

combine :: (IsString m, Monoid m) => m -> m -> m
combine x y = x <> "/" <> y


type Host     = String
type Port     = Int
type User     = B.ByteString
type Password = B.ByteString


get :: Method f -> Jenk BL.ByteString
get m = Jenk . liftF $ Get m id

post :: (forall f. Method f) -> Jenk BL.ByteString
post m = Jenk . liftF $ Post m id


withJenkins :: Host -> Port -> User -> Password -> Jenk a -> IO a
withJenkins host port user password jenk = withManager $ \manager -> do
  req <- liftIO $ parseUrl host
  interpret manager (applyBasicAuth user password (req { port })) jenk

interpret
  :: (io ~ IO, MonadBaseControl io m, MonadIO m, MonadResource m)
  => Manager -> Request m -> Jenk a -> m a
interpret manager request = iterM go . unJenk where
  go (Get method next) = do
    let request' = request
          & L.path   %~ (`combine` BC.pack (nicely method))
          & L.method .~ "GET"
    bs <- httpLbs request' manager
    next (responseBody bs)
  go (Post method next) = do
    let request' = request
          & L.path   %~ (`combine` BC.pack (nicely method))
          & L.method .~ "POST"
    bs <- httpLbs request' manager
    next (responseBody bs)
