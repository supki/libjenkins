{-# LANGUAGE OverloadedStrings #-}
module Jenkins.RESTSpec (spec) where

import           Control.Applicative
import           Control.Monad.Trans.State (State, evalState, get, put)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Monoid (mempty)
import           Test.Hspec
import qualified Jenkins.REST as REST
import qualified Jenkins.REST.Internal as REST


spec :: Spec
spec = do
  context "POST requests" $ do
    it "post_ sends POST request with empty body" $ do
      interpret $ do
        REST.post_ "foo"
        REST.post_ "bar"
        REST.post_ "baz"
     `shouldBe`
      [Post 0 "" "foo", Post 1 "" "bar", Post 2 "" "baz"]

    it "post sends POST request with non-empty body" $ do
      interpret $ do
        REST.post "foo" "qux"
        REST.post "bar" "quux"
        REST.post "baz" "xyzzy"
     `shouldBe`
      [Post 0 "qux" "foo", Post 1 "quux" "bar", Post 2 "xyzzy" "baz"]

  context "GET requests" $
    it "get sends GET requests" $ do
      interpret $ do
        REST.get "foo"
        REST.get "bar"
        REST.get "baz"
     `shouldBe`
      [Get 0 "foo", Get 1 "bar", Get 2 "baz"]


  describe "reload" $
    it "calls $jenkins_url/reload with POST query and then disconnects" $ do
      interpret $ do
        REST.reload
        REST.post_ "foo"
     `shouldBe`
      [Post 0 "" "reload", Disconnect]

  describe "restart" $
    it "calls $jenkins_url/safeRestart with POST query and then disconnects" $ do
      interpret $ do
        REST.restart
        REST.post_ "bar"
     `shouldBe`
      [Post 0 "" "safeRestart", Disconnect]

  describe "forceRestart" $
    it "calls $jenkins_url/restart with POST query and then disconnects" $ do
      interpret $ do
        REST.forceRestart
        REST.post_ "baz"
     `shouldBe`
      [Post 0 "" "restart", Disconnect]


data Query =
    Get Int Strict.ByteString
  | Post Int Lazy.ByteString Strict.ByteString
  | Disconnect
  | IO
    deriving (Show, Eq)

newtype Requests a = Requests [a]
  deriving (Show, Eq)

interpret :: REST.Jenkins a -> [Query]
interpret adt = evalState (REST.runJenkinsP go ([] <$ adt)) (Requests [0..]) where
  go :: REST.JenkinsF (State (Requests Int) [Query]) -> State (Requests Int) [Query]
  go (REST.Get m n) = do
    r <- render Get m
    fmap (r :) (n mempty)
  go (REST.Post m body n) = do
    r <- render (\x y -> Post x body y) m
    fmap (r :) (n mempty)
  go REST.Dcon =
    return [Disconnect]

render :: (a -> Strict.ByteString -> Query) -> REST.Method f x -> State (Requests a) Query
render f m = do
  n <- next
  return $ f n (REST.render m)

next :: State (Requests a) a
next = do
  Requests (x:xs) <- get
  put (Requests xs)
  return x
