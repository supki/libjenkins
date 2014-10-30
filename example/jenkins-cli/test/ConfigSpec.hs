{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec (spec) where

import           Control.Lens
import           Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Jenkins.Rest as Jenkins
import           Test.Hspec.Lens

import           Config


spec :: Spec
spec = do
  it "parses default configuration" $ do
    let configJson = Text.unlines
          [ "{ \"url\": \"http://example.com/jenkins\""
          , ", \"user\": \"jenkins\""
          , ", \"api-token\": \"12345678\""
          , "}"
          ]
        config = Config (Jenkins.defaultMaster & set Jenkins.apiToken "12345678")
    decodeStrict (Text.encodeUtf8 configJson) `shouldHave` _Just.only config

  it "parses custom configuration" $ do
    let customConfigText = Text.unlines
          [ "{ \"url\": \"https://google.com/hudson\""
          , ", \"user\": \"google\""
          , ", \"api-token\": \"87654321\""
          , "}"
          ]
        customConfig = Config
          ( Jenkins.defaultMaster
          & set Jenkins.url "https://google.com/hudson"
          & set Jenkins.user "google"
          & set Jenkins.apiToken "87654321"
          )
    decodeStrict (Text.encodeUtf8 customConfigText) `shouldHave` _Just.only customConfig
