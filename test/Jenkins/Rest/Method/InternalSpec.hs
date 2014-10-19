{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Jenkins.Rest.Method.InternalSpec where

import Test.Hspec

import Jenkins.Rest.Method.Internal
import Jenkins.Rest.Method

spec :: Spec
spec =
  describe "render" $

    it "has a bunch of example input/output pairs" $ do

      format xml ""
        `shouldBe` "api/xml"

      format xml ("job" -/- 7)
        `shouldBe` "job/7/api/xml"

      format json ("job" -/- 7)
        `shouldBe` "job/7/api/json"

      render (text "restart")
        `shouldBe` "restart"

      render ("job" -?- "name" -=- "foo" -&- "title" -=- "bar")
        `shouldBe` "job?name=foo&title=bar"

      render ("job" -?- "name" -&- "title" -=- "bar")
        `shouldBe` "job?name&title=bar"

      format json ("job" -/- 7 -?- "name" -&- "title" -=- "bar")
        `shouldBe` "job/7/api/json?name&title=bar"

      format xml ("job" -/- "ДМИТРИЙ")
        `shouldBe` "job/%D0%94%D0%9C%D0%98%D0%A2%D0%A0%D0%98%D0%99/api/xml"
