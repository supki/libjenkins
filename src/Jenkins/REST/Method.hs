{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Jenkins RESTful API methods construction
module Jenkins.REST.Method
  ( -- * Types
    Method, Type(..), Format, As
    -- * User interface helpers
  , text, (-?-), (-/-), (-=-), (-&-), as, json, xml, python, query
    -- * Rendering
  , render, slash
  ) where

import           Data.ByteString (ByteString)
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 706)
import           Data.ByteString.Char8 ()
#endif
import           Data.Monoid (Monoid(..), (<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)

-- $setup
-- >>> :set -XOverloadedStrings


infix  1 :~?, -?-
infix  3 :~@, `as`
infix  7 :~=, -=-
infixr 5 :~/, -/-, :~&, -&-
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

-- | Only to support number literals
instance Num (Method Complete f) where
  (+)         = error "Method.(+): not supposed to be used"
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

-- | Response formats
data Format = JSON | XML | Python

-- | Response format singleton type
data As :: Format -> * where
  AsJSON   :: As JSON
  AsXML    :: As XML
  AsPython :: As Python

deriving instance Show (As f)

text :: Text -> Method Complete f
text = Text

-- | Append 2 paths
(-/-) :: Method Complete f -> Method Complete f -> Method Complete f
(-/-) = (:~/)

-- | Append 2 queries
(-&-) :: Method Query f -> Method Query f -> Method Query f
(-&-) = (:~&)

-- | Make a query
(-=-) :: Text -> Text -> Method Query f
x -=- y = x :~= Just y

-- | Choose response format
as :: Method Complete f -> As f -> Method Complete f
as = (:~@)

-- | JSON response format
json :: As JSON
json = AsJSON

-- | XML response format
xml :: As XML
xml = AsXML

-- | Python response format
python :: As Python
python = AsPython

-- | Append path and query
(-?-) :: Method Complete f -> Method Query f -> Method Complete f
(-?-) = (:~?)

-- | list-to-query combinator
--
-- >>> render (query [("foo", Nothing), ("bar", Just "baz"), ("quux", Nothing)])
-- "foo&bar=baz&quux"
--
-- >>> render (query [])
-- ""
query :: [(Text, Maybe Text)] -> Method Query f
query [] = Empty
query xs = foldr1 (:~&) (map (uncurry (:~=)) xs)


-- | Render 'Method' to something that can be sent over the wire
--
-- >>> render ("job" -/- 7 `as` xml)
-- "job/7/api/xml"
--
-- >>> render ("job" -/- 7 `as` json)
-- "job/7/api/json"
--
-- >>> render (text "restart")
-- "restart"
--
-- >>> render ("job" -?- "name" -=- "foo" -&- "title" -=- "bar")
-- "job?name=foo&title=bar"
--
-- >>> render ("job" -?- "name" -&- "title" -=- "bar")
-- "job?name&title=bar"
--
-- >>> render ("job" -/- 7 `as` json -?- "name" -&- "title" -=- "bar")
-- "job/7/api/json?name&title=bar"
render :: Method t f -> ByteString
render Empty            = ""
render (Text s)         = T.encodeUtf8 s
render (x :~/ y)        = render x `slash` render y
render (x :~@ AsJSON)   = render x `slash` "api" `slash` "json"
render (x :~@ AsXML)    = render x `slash` "api" `slash` "xml"
render (x :~@ AsPython) = render x `slash` "api" `slash` "python"
render (x :~= Just y)   = T.encodeUtf8 x `equals` T.encodeUtf8 y
render (x :~= Nothing)  = T.encodeUtf8 x
render (x :~& y)        = render x `ampersand` render y
render (x :~? y)        = render x `question` render y

-- | Insert \"\/\" between two 'String'-like things and concat them.
slash :: (IsString m, Monoid m) => m -> m -> m
slash = insert "/"

-- | Insert \"=\" between two 'String'-like things and concat them.
equals :: (IsString m, Monoid m) => m -> m -> m
equals = insert "="

-- | Insert \"&\" between two 'String'-like things and concat them.
ampersand :: (IsString m, Monoid m) => m -> m -> m
ampersand = insert "&"

-- | Insert \"?\" between two 'String'-like things and concat them.
question :: (IsString m, Monoid m) => m -> m -> m
question = insert "?"

-- | Insert 'String'-like thing between two 'String'-like things and concat them.
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
