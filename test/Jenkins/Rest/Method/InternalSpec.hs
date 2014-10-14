{-# LANGUAGE OverloadedStrings #-}
module Jenkins.Rest.Method.InternalSpec where

import Test.Hspec

import Jenkins.Rest.Method.Internal
import Jenkins.Rest.Method


spec :: Spec
spec =
  describe "render" $ do

    it "has a bunch of example input/output pairs" $ do

      render ("" `as` xml)
        `shouldBe` "api/xml"

      render xml
        `shouldBe` "api/xml"

      render ("job" -/- 7 `as` xml)
        `shouldBe` "job/7/api/xml"

      render ("job" -/- 7 `as` xml)
        `shouldBe` "job/7/api/xml"

      render ("job" -/- 7 `as` json)
        `shouldBe` "job/7/api/json"

      render (text "restart")
        `shouldBe` "restart"

      render ("job" -?- "name" -=- "foo" -&- "title" -=- "bar")
        `shouldBe` "job?name=foo&title=bar"

      render ("job" -?- "name" -&- "title" -=- "bar")
        `shouldBe` "job?name&title=bar"

      render ("job" -/- 7 `as` json -?- "name" -&- "title" -=- "bar")
        `shouldBe` "job/7/api/json?name&title=bar"

      render ("job" -/- "ДМИТРИЙ" `as` xml)
        `shouldBe` "job/%D0%94%D0%9C%D0%98%D0%A2%D0%A0%D0%98%D0%99/api/xml"
