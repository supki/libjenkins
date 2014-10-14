{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Jenkins REST API method construction
module Jenkins.Rest.Method.Internal where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Data (Data, Typeable)
import           Data.Monoid (Monoid(..), (<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.URI (escapeURIChar, isUnreserved)

-- $setup
-- >>> :set -XOverloadedStrings


infix  1 :~?
infix  3 :~@
infix  7 :~=
infixr 5 :~/, :~&

-- | Jenkins RESTFul API method encoding
data Method :: Type -> Format -> * where
  Empty :: Method t f
  Text  :: Text -> Method Complete f
  (:~/)  :: Method Complete f -> Method Complete f -> Method Complete f
  (:~@)  :: Method Complete f -> As f -> Method Complete f
  (:~=)  :: Text -> Maybe Text -> Method Query f
  (:~&)  :: Method Query f -> Method Query f -> Method Query f
  (:~?)  :: Method Complete f -> Method Query f -> Method Complete f

deriving instance Show (As f) => Show (Method t f)

-- | Only to support numeric literals
instance t ~ Complete => Num (Method t f) where
  (+)         = error "Method.(+): not supposed to be used"
  (-)         = error "Method.(-): not supposed to be used"
  (*)         = error "Method.(*): not supposed to be used"
  abs         = error "Method.abs: not supposed to be used"
  signum      = error "Method.signum: not supposed to be used"
  fromInteger = fromString . show

instance IsString (Method Complete f) where
  fromString = Text . T.pack

instance IsString (Method Query f) where
  fromString str = T.pack str :~= Nothing

-- | Method types
data Type = Query | Complete
  deriving (Show, Eq, Typeable, Data, Generic)

-- | Response formats
data Format = JSON | XML | Python
  deriving (Show, Eq, Typeable, Data, Generic)

-- | Response format singleton type
data As :: Format -> * where
  AsJSON   :: As JSON
  AsXML    :: As XML
  AsPython :: As Python

deriving instance Show (As f)
deriving instance Eq (As f)


-- | Render 'Method' to something that can be sent over the wire
render :: Method t f -> ByteString
render Empty            = ""
render (Text s)         = renderText s
render (x :~/ y)        = render x `slash` render y
render (x :~@ f)        =
  let prefix  = render x
      postfix = renderFormat f
  in if B.null prefix then  "api" `slash` postfix else prefix `slash` "api" `slash` postfix
render (x :~= Just y)   = renderText x `equals` renderText y
render (x :~= Nothing)  = renderText x
render (x :~& y)        = render x `ampersand` render y
render (x :~? y)        = render x `question` render y

renderFormat :: IsString s => As f -> s
renderFormat AsJSON   = "json"
renderFormat AsXML    = "xml"
renderFormat AsPython = "python"

-- | Render unicode text as a query string
--
-- >>> renderText "foo-bar-baz"
-- "foo-bar-baz"
--
-- >>> renderText "foo bar baz"
-- "foo%20bar%20baz"
--
-- >>> renderText "ДМИТРИЙ МАЛИКОВ"
-- "%D0%94%D0%9C%D0%98%D0%A2%D0%A0%D0%98%D0%99%20%D0%9C%D0%90%D0%9B%D0%98%D0%9A%D0%9E%D0%92"
renderText :: Text -> ByteString
renderText = T.encodeUtf8 . T.concatMap (T.pack . escapeURIChar isUnreserved)

-- | Insert \"\/\" between two 'String'-like things and concatenate everything.
slash :: (IsString m, Monoid m) => m -> m -> m
slash = insert "/"

-- | Insert \"=\" between two 'String'-like things and concatenate everything.
equals :: (IsString m, Monoid m) => m -> m -> m
equals = insert "="

-- | Insert \"&\" between two 'String'-like things and concatenate everything.
ampersand :: (IsString m, Monoid m) => m -> m -> m
ampersand = insert "&"

-- | Insert \"?\" between two 'String'-like things and concatenate everything.
question :: (IsString m, Monoid m) => m -> m -> m
question = insert "?"

-- | Insert 'String'-like thing between two 'String'-like things and concatenate everything.
--
-- >>> "foo" `slash` "bar"
-- "foo/bar"
--
-- >>> "" `ampersand` "foo"
-- "&foo"
--
-- >>> "foo" `question` ""
-- "foo?"
insert :: (IsString m, Monoid m) => m -> m -> m -> m
insert t x y = x <> t <> y
