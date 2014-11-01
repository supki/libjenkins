{-# LANGUAGE OverloadedStrings #-}
module Jenkins.DiscoverSpec (spec) where

import Data.ByteString (ByteString)
import Test.Hspec

import Jenkins.Discover


spec :: Spec
spec =
  describe "parse" $ do
    it "parses a real Jenkins response" $
      parseXml fullResponse
      `shouldBe`
      Just Discover
        { version  = "1.587"
        , url      = "https://example.com/jenkins/"
        , serverId = Just "1abcfaefb01da8b19ede7c35ced24f0f"
        , port     = Just "51667"
        }

    it "parses a Jenkins response without a server id" $
        parseXml noServerId
       `shouldBe`
        Just Discover
          { version  = "1.587"
          , url      = "https://example.com/jenkins/"
          , serverId = Nothing
          , port     = Just "51667"
          }

    it "parses a Jenkins response without a slave port" $
        parseXml noSlavePort
       `shouldBe`
        Just Discover
          { version  = "1.587"
          , url      = "https://example.com/jenkins/"
          , serverId = Just "1abcfaefb01da8b19ede7c35ced24f0f"
          , port     = Nothing
          }

fullResponse :: ByteString
fullResponse =
  "<hudson>\
  \<version>1.587</version>\
  \<url>https://example.com/jenkins/</url>\
  \<server-id>1abcfaefb01da8b19ede7c35ced24f0f</server-id>\
  \<slave-port>51667</slave-port>\
  \</hudson>"

noServerId :: ByteString
noServerId =
  "<hudson>\
  \<version>1.587</version>\
  \<url>https://example.com/jenkins/</url>\
  \<slave-port>51667</slave-port>\
  \</hudson>"

noSlavePort :: ByteString
noSlavePort =
  "<hudson>\
  \<version>1.587</version>\
  \<url>https://example.com/jenkins/</url>\
  \<server-id>1abcfaefb01da8b19ede7c35ced24f0f</server-id>\
  \</hudson>"
