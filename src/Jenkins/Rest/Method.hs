{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Jenkins REST API method construction
module Jenkins.Rest.Method
  ( -- * Types
    Method
  , Type(..)
  , Format
  , As
    -- * Method construction
  , text, int
  , (-?-), (-/-), (-=-), (-&-)
  , query
  , as
  , JSONy(..)
  , XMLy(..)
  , Pythony(..)
    -- * Shortcuts
  , job
  , build
  , view
  , queue
  , overallLoad
  , computer
    -- * Rendering
  , render
  , slash
  ) where

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

-- | Convert 'Text' to 'Method'
text :: Text -> Method Complete f
text = Text

-- | Convert 'Integer' to 'Method'
int :: Integer -> Method Complete f
int = fromInteger

-- | Combine 2 paths
(-/-) :: Method Complete f -> Method Complete f -> Method Complete f
(-/-) = (:~/)

-- | Combine 2 queries
(-&-) :: Method Query f -> Method Query f -> Method Query f
(-&-) = (:~&)

-- | Make a field-value pair
(-=-) :: Text -> Text -> Method Query f
x -=- y = x :~= Just y

-- | Choose response format
as :: Method Complete f -> As f -> Method Complete f
as = (:~@)

-- | JSON response format
class JSONy t where
  json :: t JSON

instance JSONy As where
  json = AsJSON

instance t ~ Complete => JSONy (Method t) where
  json = "" `as` json

-- | XML response format
class XMLy t where
  xml :: t XML

instance XMLy As where
  xml = AsXML

instance t ~ Complete => XMLy (Method t) where
  xml = "" `as` xml

-- | Python response format
class Pythony t where
  python :: t Python

instance Pythony As where
  python = AsPython

instance t ~ Complete => Pythony (Method t) where
  python = "" `as` python

-- | Combine path and query
(-?-) :: Method Complete f -> Method Query f -> Method Complete f
(-?-) = (:~?)

-- | List-to-query convenience combinator
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
-- >>> render ("" `as` xml)
-- "api/xml"
--
-- >>> render xml
-- "api/xml"
--
-- >>> render ("job" -/- 7 `as` xml)
-- "job/7/api/xml"
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
--
-- >>> render ("job" -/- "ДМИТРИЙ" `as` xml)
-- "job/%D0%94%D0%9C%D0%98%D0%A2%D0%A0%D0%98%D0%99/api/xml"
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


-- | Job API method
--
-- >>> render (job "name" `as` json)
-- "job/name/api/json"
job :: Text -> Method Complete f
job name = "job" -/- text name

-- | Job build API method
--
-- >>> render (build "name" 4 `as` json)
-- "job/name/4/api/json"
build :: Integral a => Text -> a -> Method Complete f
build name num = "job" -/- text name -/- int (toInteger num)

-- | View API method
--
-- >>> render (view "name" `as` xml)
-- "view/name/api/xml"
view :: Text -> Method Complete f
view name = "view" -/- text name

-- | Queue API method
--
-- >>> render (queue `as` python)
-- "queue/api/python"
queue :: Method Complete f
queue = "queue"

-- | Statistics API method
--
-- >>> render (overallLoad `as` xml)
-- "overallLoad/api/xml"
overallLoad :: Method Complete f
overallLoad = "overallLoad"

-- | Node API method
--
-- >>> render (computer `as` python)
-- "computer/api/python"
computer :: Method Complete f
computer = "computer"
