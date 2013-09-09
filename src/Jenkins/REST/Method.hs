{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Jenkins RESTful API methods construction
module Jenkins.REST.Method
  ( -- * Types
    Method, As, Format
    -- * User interface helpers
  , (-/-), as, json, xml, python
    -- * Rendering
  , render, combine
  ) where

import           Data.ByteString (ByteString)
import           Data.Monoid (Monoid(..), (<>))
import           Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)

-- $setup
-- >>> :set -XOverloadedStrings


infix  6 :-, `as`
infixr 7 :/
-- | Jenkins RESTFul API method encoding
data Method :: Format -> * where
  Empty :: Method f
  Text  :: Text -> Method f
  (:/)  :: Method f -> Method f -> Method f
  (:-)  :: Method f -> As f -> Method f

deriving instance Show (As f) => Show (Method f)

instance Monoid (Method f) where
  mempty  = Empty
  mappend = (:/)

-- | Only to support number literals
instance Num (Method f) where
  (+)         = error "Method.(+): not supposed to be used"
  (*)         = error "Method.(*): not supposed to be used"
  abs         = error "Method.abs: not supposed to be used"
  signum      = error "Method.signum: not supposed to be used"
  fromInteger = fromString . show

instance IsString (Method f) where
  fromString = Text . T.pack

-- | Response formats
data Format = JSON | XML | Python

-- | Response format singleton type
data As :: Format -> * where
  AsJSON   :: As JSON
  AsXML    :: As XML
  AsPython :: As Python

deriving instance Show (As f)

-- | Append 2 methods
(-/-) :: Method f -> Method f -> Method f
(-/-) = mappend

-- | Choose response format
as :: Method f -> As f -> Method f
as = (:-)

-- | JSON response format
json :: As JSON
json = AsJSON

-- | XML response format
xml :: As XML
xml = AsXML

-- | Python response format
python :: As Python
python = AsPython


-- | Render 'Method' to something that can be sent over the wire
--
-- >>> render ("job" -/- 7 `as` xml)
-- "job/7/api/xml"
--
-- >>> render ("job" -/- 7 `as` json)
-- "job/7/api/json"
--
-- >>> render "restart"
-- "restart"
render :: Method f -> ByteString
render Empty           = ""
render (Text s)        = T.encodeUtf8 s
render (x :/ y)        = render x `combine` render y
render (x :- AsJSON)   = render x `combine` "api" `combine` "json"
render (x :- AsXML)    = render x `combine` "api" `combine` "xml"
render (x :- AsPython) = render x `combine` "api" `combine` "python"

-- | Insert \"\/\" between two 'String'-like things and concat them.
--
-- >>> combine "foo" "bar"
-- "foo/bar"
--
-- >>> combine "" "foo"
-- "/foo"
--
-- >>> combine "foo" ""
-- "foo/"
combine :: (IsString m, Monoid m) => m -> m -> m
combine x y = x <> "/" <> y
