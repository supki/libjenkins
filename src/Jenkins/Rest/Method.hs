{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Jenkins REST API method construction
module Jenkins.Rest.Method
  ( -- * API methods
    job
  , build
  , view
  , queue
  , overallLoad
  , computer
    -- * Method construction
  , text, int
  , (-?-), (-/-), (-=-), (-&-)
  , query
  , Formatter
  , json
  , xml
  , python
  , plain
    -- * Types
  , Method
  , Type(..)
  , Format
  ) where

import Data.Text (Text)

import Jenkins.Rest.Method.Internal


-- $setup
-- >>> :set -XOverloadedStrings


infix  1 -?-
infix  7 -=-
infixr 5 -/-, -&-


-- | Convert 'Text' to 'Method'
text :: Text -> Method Complete f
text = Text

-- | Convert 'Integer' to 'Method'
int :: Integer -> Method Complete f
int = fromInteger

-- | Combine 2 paths
(-/-) :: Method Complete f -> Method Complete f -> Method Complete f
(-/-) = (:/)

-- | Combine 2 queries
(-&-) :: Method Query f -> Method Query f -> Method Query f
(-&-) = (:&)

-- | Make a field-value pair
(-=-) :: Text -> Text -> Method Query f
x -=- y = x := Just y

-- | Asks Jenkins to render the response as a JSON document
--
-- >>> format json ("foo" -/- "bar")
-- "foo/bar/api/json"
json :: Formatter Json
json = Formatter (\m -> m :@ SJson)
{-# ANN json ("HLint: ignore Avoid lambda" :: String) #-}

-- | Asks Jenkins to render the response as an XML document
--
-- >>> format xml ("foo" -/- "bar")
-- "foo/bar/api/xml"
xml :: Formatter Xml
xml = Formatter (\m -> m :@ SXml)
{-# ANN xml ("HLint: ignore Avoid lambda" :: String) #-}

-- | Asks Jenkins to render the response as a Python dictionary
--
-- >>> format python ("foo" -/- "bar")
-- "foo/bar/api/python"
python :: Formatter Python
python = Formatter (\m -> m :@ SPython)
{-# ANN python ("HLint: ignore Avoid lambda" :: String) #-}

-- | The formatter that does nothing
--
-- >>> format plain ("foo" -/- "bar")
-- "foo/bar"
plain :: Formatter f
plain = Formatter (\m -> m)
{-# ANN plain ("HLint: ignore Use id" :: String) #-}

-- | Combine path and query
(-?-) :: Method Complete f -> Method Query f -> Method Complete f
(-?-) = (:?)

-- | List-to-query convenience combinator
--
-- >>> renderQ' (query [("foo", Nothing), ("bar", Just "baz"), ("quux", Nothing)])
-- "foo&bar=baz&quux"
--
-- >>> renderQ' (query [])
-- ""
query :: [(Text, Maybe Text)] -> Method Query f
query [] = Empty
query xs = foldr1 (:&) (map (uncurry (:=)) xs)

-- | Job information
--
-- >>> format json (job "name")
-- "job/name/api/json"
job :: Text -> Method Complete f
job name = "job" -/- text name

-- | Job build information
--
-- >>> format json (build "name" 4)
-- "job/name/4/api/json"
build :: Integral a => Text -> a -> Method Complete f
build name num = "job" -/- text name -/- int (toInteger num)

-- | View information
--
-- >>> format xml (view "name")
-- "view/name/api/xml"
view :: Text -> Method Complete f
view name = "view" -/- text name

-- | Build queue information
--
-- >>> format python queue
-- "queue/api/python"
queue :: Method Complete f
queue = "queue"

-- | Statistics
--
-- >>> format xml overallLoad
-- "overallLoad/api/xml"
overallLoad :: Method Complete f
overallLoad = "overallLoad"

-- | Nodes information
--
-- >>> format python computer
-- "computer/api/python"
computer :: Method Complete f
computer = "computer"
