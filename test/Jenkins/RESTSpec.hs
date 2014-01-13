{-# LANGUAGE OverloadedStrings #-}
module Jenkins.RESTSpec (spec) where

import           Control.Applicative
import           Control.Monad.Trans.State (State, evalState, get, put)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Monoid (mempty)
import           Test.Hspec
import qualified Jenkins.REST as REST
import           Jenkins.REST.Internal


spec :: Spec
spec = do
  context "POST requests" $ do
    it "post_ sends POST request with empty body" $ do
      interpret $ do
        REST.post_ "foo"
        REST.post_ "bar"
        REST.post_ "baz"
     `shouldBe`
      [QPost 0 "" "foo", QPost 1 "" "bar", QPost 2 "" "baz"]

    it "post sends POST request with non-empty body" $ do
      interpret $ do
        REST.post "foo" "qux"
        REST.post "bar" "quux"
        REST.post "baz" "xyzzy"
     `shouldBe`
      [QPost 0 "qux" "foo", QPost 1 "quux" "bar", QPost 2 "xyzzy" "baz"]

  context "GET requests" $
    it "get sends GET requests" $ do
      interpret $ do
        REST.get "foo"
        REST.get "bar"
        REST.get "baz"
     `shouldBe`
      [QGet 0 "foo", QGet 1 "bar", QGet 2 "baz"]


  describe "reload" $
    it "calls $jenkins_url/reload with POST query and then disconnects" $ do
      interpret $ do
        REST.reload
        REST.post_ "foo"
     `shouldBe`
      [QPost 0 "" "reload", QDisconnect]

  describe "restart" $
    it "calls $jenkins_url/safeRestart with POST query and then disconnects" $ do
      interpret $ do
        REST.restart
        REST.post_ "bar"
     `shouldBe`
      [QPost 0 "" "safeRestart", QDisconnect]

  describe "forceRestart" $
    it "calls $jenkins_url/restart with POST query and then disconnects" $ do
      interpret $ do
        REST.forceRestart
        REST.post_ "baz"
     `shouldBe`
      [QPost 0 "" "restart", QDisconnect]


data Query =
    QGet Int Strict.ByteString
  | QPost Int Lazy.ByteString Strict.ByteString
  | QDisconnect
    deriving (Show, Eq)

newtype Requests a = Requests [a]
  deriving (Show, Eq)

interpret :: REST.Jenkins a -> [Query]
interpret adt = evalState (iterJenkins go ([] <$ adt)) (Requests [0..]) where
  go :: JenkinsF (State (Requests Int) [Query]) -> State (Requests Int) [Query]
  go (Get m n) = do
    r <- render QGet m
    fmap (r :) (n mempty)
  go (Post m body n) = do
    r <- render (\x y -> QPost x body y) m
    fmap (r :) (n mempty)
  go Dcon =
    return [QDisconnect]

render :: (a -> Strict.ByteString -> Query) -> REST.Method f x -> State (Requests a) Query
render f m = do
  n <- next
  return $ f n (REST.render m)

next :: State (Requests a) a
next = do
  Requests (x:xs) <- get
  put (Requests xs)
  return x
