{-# LANGUAGE OverloadedStrings #-}
module Jenkins.DiscoverSpec (spec) where

import Test.Hspec

import Jenkins.Discover


spec :: Spec
spec =
  describe "parse" $ do
    it "parses Jenkins xml response with the server-id tag" $
      let
        response = "<hudson><version>foo</version><url>bar</url><server-id>baz</server-id></hudson>"
      in
        parse response `shouldBe` Just (Discover "foo" "bar" (Just "baz"))

    it "parses Jenkins xml response without the server-id tag" $
      let
        response = "<hudson><version>foo</version><url>bar</url></hudson>"
      in
        parse response `shouldBe` Just (Discover "foo" "bar" Nothing)
