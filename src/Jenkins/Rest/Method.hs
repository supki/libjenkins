{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Jenkins REST API methods
module Jenkins.Rest.Method
  ( -- * Construct URLs
    -- ** Path
    text
  , int
  , (-/-)
    -- ** Query
  , (-=-)
  , (-&-)
  , query
    -- ** Put together the segments and the query
  , (-?-)
    -- ** Format
  , Formatter
  , json
  , xml
  , python
  , plain
  , -- * Shortcuts
    job
  , build
  , view
  , queue
  , overallLoad
  , computer
    -- * Types
  , Method
  , Type(..)
  , Format(..)
  ) where

import Data.Text (Text)

import Jenkins.Rest.Method.Internal


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> class P t where pp :: Method t f -> Data.ByteString.ByteString
-- >>> instance P Complete where pp = render
-- >>> instance P Query    where pp = renderQ'
-- >>> let pp' = render


infix  1 -?-
infix  7 -=-
infixr 5 -/-, -&-


-- | Use a string as an URI segment
--
-- >>> pp (text "foo")
-- "foo"
--
-- /Note:/ with @-XOverloadedStrings@ extension enabled it's possible to use string
-- literals as segments of the Jenkins API method URL
--
-- >>> pp' "foo"
-- "foo"
--
-- /Note:/ don't put @/@ in the string literal unless you want it URL-encoded,
-- use @(-/-)@ instead
--
-- >>> pp' "foo/bar"
-- "foo%2Fbar"
text :: Text -> Method Complete f
text = Text

-- | Use an integer as an URI segment
--
-- >>> pp (int 4)
-- "4"
int :: Int -> Method Complete f
int = fromIntegral

-- | Combine two paths
--
-- >>> pp ("foo" -/- "bar" -/- "baz")
-- "foo/bar/baz"
(-/-) :: Method Complete f -> Method Complete f -> Method Complete f
(-/-) = (:/)

-- | Make a key-value pair
--
-- >>> pp ("foo" -=- "bar")
-- "foo=bar"
(-=-) :: Text -> Text -> Method Query f
x -=- y = x := Just y

-- | Create the union of two queries
--
-- >>> pp ("foo" -=- "bar" -&- "baz")
-- "foo=bar&baz"
(-&-) :: Method Query f -> Method Query f -> Method Query f
(-&-) = (:&)

-- | Take a list of key-value pairs and render them as a query
--
-- >>> pp (query [("foo", Nothing), ("bar", Just "baz"), ("quux", Nothing)])
-- "foo&bar=baz&quux"
--
-- >>> pp (query [])
-- ""
query :: [(Text, Maybe Text)] -> Method Query f
query = foldr ((:&) . uncurry (:=)) Empty

-- | Put path and query together
--
-- >>> pp ("qux" -/- "quux" -?- "foo" -=- "bar" -&- "baz")
-- "qux/quux?foo=bar&baz"
(-?-) :: Method Complete f -> Method Query f -> Method Complete f
(-?-) = (:?)

-- | Append the JSON formatting request to the method URL
--
-- >>> format json "foo"
-- "foo/api/json"
json :: Formatter Json
json = Formatter (\m -> m :@ SJson)
{-# ANN json ("HLint: ignore Avoid lambda" :: String) #-}

-- | Append the XML formatting request to the method URL
--
-- >>> format xml "foo"
-- "foo/api/xml"
xml :: Formatter Xml
xml = Formatter (\m -> m :@ SXml)
{-# ANN xml ("HLint: ignore Avoid lambda" :: String) #-}

-- | Append the Python formatting request to the method URL
--
-- >>> format python "foo"
-- "foo/api/python"
python :: Formatter Python
python = Formatter (\m -> m :@ SPython)
{-# ANN python ("HLint: ignore Avoid lambda" :: String) #-}

-- | The formatter that does exactly nothing
--
-- >>> format plain "foo"
-- "foo/bar"
plain :: Formatter f
plain = Formatter (\m -> m)
{-# ANN plain ("HLint: ignore Use id" :: String) #-}


-- | Job data
--
-- >>> format json (job "name")
-- "job/name/api/json"
--
-- >>> pp (job "name" -/- "config.xml")
-- "job/name/config.xml"
job :: Text -> Method Complete f
job name = "job" -/- text name

-- | Job build data
--
-- >>> format json (build "name" 4)
-- "job/name/4/api/json"
build :: Text -> Int -> Method Complete f
build name num = "job" -/- text name -/- int num

-- | View data
--
-- >>> format xml (view "name")
-- "view/name/api/xml"
view :: Text -> Method Complete f
view name = "view" -/- text name

-- | Build queue data
--
-- >>> format python queue
-- "queue/api/python"
queue :: Method Complete f
queue = "queue"

-- | Server statistics
--
-- >>> format xml overallLoad
-- "overallLoad/api/xml"
overallLoad :: Method Complete f
overallLoad = "overallLoad"

-- | Nodes data
--
-- >>> format python computer
-- "computer/api/python"
computer :: Method Complete f
computer = "computer"
