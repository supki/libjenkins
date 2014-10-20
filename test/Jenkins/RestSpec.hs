{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Jenkins.RestSpec (spec) where

import           Control.Applicative
import           Control.Monad.Trans.State (State, evalState, get, put)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Conduit as C
import           Data.Functor.Identity (Identity)
import           Data.Monoid (mempty)
import           Test.Hspec
import qualified Jenkins.Rest as Rest
import           Jenkins.Rest.Internal
import qualified Jenkins.Rest.Method.Internal as Method


spec :: Spec
spec = do
  context "POST requests" $ do
    it "post_ sends POST request with empty body" $ do
      interpret $ do
        Rest.post_ "foo"
        Rest.post_ "bar"
        Rest.post_ "baz"
     `shouldBe`
      [QPost 0 "" "foo", QPost 1 "" "bar", QPost 2 "" "baz"]

    it "post sends POST request with non-empty body" $ do
      interpret $ do
        Rest.post "foo" "qux"
        Rest.post "bar" "quux"
        Rest.post "baz" "xyzzy"
     `shouldBe`
      [QPost 0 "qux" "foo", QPost 1 "quux" "bar", QPost 2 "xyzzy" "baz"]

  context "GET requests" $
    it "get sends GET requests" $ do
      interpret $ do
        let getS :: (forall f. Rest.Method Rest.Complete f) -> JenkinsT Identity (ResourceT IO (C.ResumableSource (ResourceT IO) Strict.ByteString))
            getS = Rest.getS Rest.plain
        getS "foo"
        getS "bar"
        getS "baz"
     `shouldBe`
      [QGet 0 "foo", QGet 1 "bar", QGet 2 "baz"]


  describe "reload" $
    it "calls $jenkins_url/reload with POST query and then disconnects" $ do
      interpret $ do
        Rest.reload
        Rest.post_ "foo"
     `shouldBe`
      [QPost 0 "" "reload", QDisconnect]

  describe "restart" $
    it "calls $jenkins_url/safeRestart with POST query and then disconnects" $ do
      interpret $ do
        Rest.restart
        Rest.post_ "bar"
     `shouldBe`
      [QPost 0 "" "safeRestart", QDisconnect]

  describe "forceRestart" $
    it "calls $jenkins_url/restart with POST query and then disconnects" $ do
      interpret $ do
        Rest.forceRestart
        Rest.post_ "baz"
     `shouldBe`
      [QPost 0 "" "restart", QDisconnect]


data Query =
    QGet Int Strict.ByteString
  | QPost Int Lazy.ByteString Strict.ByteString
  | QDisconnect
    deriving (Show, Eq)

newtype Requests a = Requests [a]
  deriving (Show, Eq)

interpret :: Rest.JenkinsT Identity a -> [Query]
interpret adt = evalState (iterJenkins go ([] <$ adt)) (Requests [0..]) where
  go :: JenkinsF Identity (State (Requests Int) [Query]) -> State (Requests Int) [Query]
  go (Get m n) = do
    r <- render QGet m
    fmap (r :) (n (return (C.newResumableSource (C.yield mempty))))
  go (Post m body n) = do
    r <- render (\x y -> QPost x body y) m
    fmap (r :) n
  go Dcon =
    return [QDisconnect]

render :: (a -> Strict.ByteString -> Query) -> Rest.Method Method.Complete f -> State (Requests a) Query
render f m = do
  n <- next
  return $ f n (Method.render m)

next :: State (Requests a) a
next = do
  Requests (x:xs) <- get
  put (Requests xs)
  return x
