{-# LANGUAGE OverloadedStrings #-}
module Jenkins.RESTSpec (spec) where

import           Data.ByteString (ByteString)
import           Data.Monoid (mempty)
import           Test.Hspec
import qualified Jenkins.REST as REST
import qualified Jenkins.REST.Internal as REST
import           Jenkins.REST.Method (render)


spec :: Spec
spec = do
  describe "reload" $
    it "calls $jenkins_url/reload with POST query and then disconnects" $
      interpret REST.reload `shouldBe` [Post "reload", Disconnect]

  describe "restart" $
    it "calls $jenkins_url/safeRestart with POST query and then disconnects" $
      interpret REST.restart `shouldBe` [Post "safeRestart", Disconnect]

  describe "forceRestart" $
    it "calls $jenkins_url/restart with POST query and then disconnects" $
      interpret REST.forceRestart `shouldBe` [Post "restart", Disconnect]


data Query =
    Get ByteString
  | Post ByteString
  | Disconnect
  | IO
    deriving (Show, Eq)

interpret :: REST.Jenkins Query -> [Query]
interpret = REST.runJenkinsP go where
  go (REST.Get m next) =
    Get (render m) : next mempty
  go (REST.Post m _ next) =
    Post (render m) : next mempty
  go REST.Dcon =
    [Disconnect]
