{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Jenkins.RestSpec (spec) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad.Trans.State (State, evalState, get, put)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor.Identity (Identity)
import           Test.Hspec
import qualified Jenkins.Rest as Jenkins
import           Jenkins.Rest.Internal
import qualified Jenkins.Rest.Method.Internal as Method


spec :: Spec
spec = do
  context "POST requests" $ do
    it "post_ sends POST request with empty body" $ do
      interpret $ do
        _ <- Jenkins.post_ "foo"
        _ <- Jenkins.post_ "bar"
        _ <- Jenkins.post_ "baz"
        return ()
     `shouldBe`
      [QPost 0 "" "foo", QPost 1 "" "bar", QPost 2 "" "baz"]

    it "post sends POST request with non-empty body" $ do
      interpret $ do
        _ <- Jenkins.post "foo" "qux"
        _ <- Jenkins.post "bar" "quux"
        _ <- Jenkins.post "baz" "xyzzy"
        return ()
     `shouldBe`
      [QPost 0 "qux" "foo", QPost 1 "quux" "bar", QPost 2 "xyzzy" "baz"]

  context "GET requests" $
    it "get sends GET requests" $ do
      interpret $ do
        _ <- Jenkins.get Jenkins.plain "foo"
        _ <- Jenkins.get Jenkins.plain "bar"
        _ <- Jenkins.get Jenkins.plain "baz"
        return ()
     `shouldBe`
      [QGet 0 "foo", QGet 1 "bar", QGet 2 "baz"]


data Query =
    QGet Int Strict.ByteString
  | QPost Int Lazy.ByteString Strict.ByteString
    deriving (Show, Eq)

newtype Requests a = Requests [a]
  deriving (Show, Eq)

interpret :: JenkinsT Identity a -> [Query]
interpret adt = evalState (iter go ([] <$ adt)) (Requests [0..]) where
  go :: JF Identity (State (Requests Int) [Query]) -> State (Requests Int) [Query]
  go (Get m n) = do
    r <- render QGet m
    fmap (r :) (n Lazy.empty)
  go (Post m body n) = do
    r <- render (\x y -> QPost x body y) m
    fmap (r :) (n Lazy.empty)
  go _ = error "unexpected term"

render :: (a -> Strict.ByteString -> Query) -> Jenkins.Method 'Method.Complete f -> State (Requests a) Query
render f m = do
  n <- next
  return $ f n (Method.render m)

next :: State (Requests a) a
next = do
  Requests (x:xs) <- get
  put (Requests xs)
  return x
