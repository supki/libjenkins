{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Jenkins REST API method construction
module Jenkins.Rest.Method.Internal where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Data (Data, Typeable)
import           Data.Monoid (Monoid(..), (<>))
import           Data.String (IsString(..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import           Network.URI (escapeURIChar, isUnreserved)

-- $setup
-- >>> :set -XOverloadedStrings


infix  1 :?
infix  3 :@
infix  7 :=
infixr 5 :/, :&

-- | Jenkins RESTFul API method encoding
data Method :: Type -> Format -> * where
  Empty :: Method Query f
  Text  :: Text -> Method Complete f
  (:/)  :: Method Complete f -> Method Complete f -> Method Complete f
  (:=)  :: Text -> Maybe Text -> Method Query f
  (:&)  :: Method Query f -> Method Query f -> Method Query f
  (:?)  :: Method Complete f -> Method Query f -> Method Complete f
  (:@)  :: Method Complete f -> SFormat f -> Method Complete f

deriving instance Show (SFormat f) => Show (Method t f)

-- | Only to support numeric literals
instance t ~ Complete => Num (Method t f) where
  (+)         = error "Method.(+): not supposed to be used"
  (-)         = error "Method.(-): not supposed to be used"
  (*)         = error "Method.(*): not supposed to be used"
  abs         = error "Method.abs: not supposed to be used"
  signum      = error "Method.signum: not supposed to be used"
  fromInteger = fromString . show

instance IsString (Method Complete f) where
  fromString = Text . fromString

instance IsString (Method Query f) where
  fromString str = fromString str := Nothing

-- | Method types
data Type = Query | Complete
  deriving (Show, Eq, Typeable, Data)

-- | Response formats
data Format = Json | Xml | Python
  deriving (Show, Eq, Typeable, Data)

data SFormat :: Format -> * where
  SJson   :: SFormat Json
  SXml    :: SFormat Xml
  SPython :: SFormat Python


newtype Formatter g = Formatter
  { unFormatter :: (forall f. Method Complete f) -> Method Complete g
  }

format :: Formatter f -> (forall g. Method Complete g) -> ByteString
format f m = render (unFormatter f m)

-- | Render 'Method' to something that can be sent over the wire
render :: Method Complete f -> ByteString
render m = maybe id (flip (insert "?")) (renderQ m) . maybe id (flip (insert "/")) (renderF m) . renderP $ m

-- | Render the method path
renderP :: Method Complete f -> ByteString
renderP (Text s) = renderT s
renderP (x :/ y) = renderP x `slash` renderP y
renderP (x :? _) = renderP x
renderP (x :@ _) = renderP x

-- | Render the query string
renderQ :: Method Complete f -> Maybe ByteString
renderQ (Text _)  = Nothing
renderQ (q :/ q') = renderQ q <|> renderQ q'
renderQ (q :@ _)  = renderQ q
renderQ (_ :? q)  = Just (renderQ' q)

renderQ' :: Method Query f -> ByteString
renderQ' (x :& y)       = insert "&" (renderQ' x) (renderQ' y)
renderQ' (x := Just y)  = insert "=" (renderT x)  (renderT y)
renderQ' (x := Nothing) = renderT x
renderQ' Empty          = renderT ""

-- | Render the response format string
renderF :: Method Complete f -> Maybe ByteString
renderF (_ :@ SJson)   = Just "api/json"
renderF (_ :@ SXml)    = Just "api/xml"
renderF (_ :@ SPython) = Just "api/python"
renderF _              = Nothing

-- | Render unicode text as a query string
--
-- >>> renderT "foo-bar-baz"
-- "foo-bar-baz"
--
-- >>> renderT "foo bar baz"
-- "foo%20bar%20baz"
--
-- >>> renderT "ДМИТРИЙ МАЛИКОВ"
-- "%D0%94%D0%9C%D0%98%D0%A2%D0%A0%D0%98%D0%99%20%D0%9C%D0%90%D0%9B%D0%98%D0%9A%D0%9E%D0%92"
renderT :: Text -> ByteString
renderT = Text.encodeUtf8 . Text.concatMap (fromString . escapeURIChar isUnreserved)

-- | Insert \"\/\" between two 'String'-like things and concatenate everything.
slash :: (IsString m, Monoid m, Eq m) => m -> m -> m
slash = insert "/"

-- | Insert 'String'-like thing between two 'String'-like things and concatenate everything.
--
-- >>> "foo" `slash` "bar"
-- "foo/bar"
--
-- >>> "foo" `slash` ""
-- "foo"
insert :: (Monoid m, Eq m) => m -> m -> m -> m
insert t x y
  | x == mempty = y
  | y == mempty = x
  | otherwise   = x <> t <> y
