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
  , as
  , JSONy(..)
  , XMLy(..)
  , Pythony(..)
    -- * Types
  , Method
  , Type(..)
  , Format
  , As
  ) where

import Data.Text (Text)

import Jenkins.Rest.Method.Internal


-- $setup
-- >>> :set -XOverloadedStrings


infix  1 -?-
infix  3 `as`
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

-- | Job information
--
-- >>> render (job "name" `as` json)
-- "job/name/api/json"
job :: Text -> Method Complete f
job name = "job" -/- text name

-- | Job build information
--
-- >>> render (build "name" 4 `as` json)
-- "job/name/4/api/json"
build :: Integral a => Text -> a -> Method Complete f
build name num = "job" -/- text name -/- int (toInteger num)

-- | View information
--
-- >>> render (view "name" `as` xml)
-- "view/name/api/xml"
view :: Text -> Method Complete f
view name = "view" -/- text name

-- | Build queue information
--
-- >>> render (queue `as` python)
-- "queue/api/python"
queue :: Method Complete f
queue = "queue"

-- | Statistics
--
-- >>> render (overallLoad `as` xml)
-- "overallLoad/api/xml"
overallLoad :: Method Complete f
overallLoad = "overallLoad"

-- | Nodes information
--
-- >>> render (computer `as` python)
-- "computer/api/python"
computer :: Method Complete f
computer = "computer"
