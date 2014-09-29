{-# LANGUAGE OverloadedStrings #-}
module Jenkins.Rest.InternalSpec (spec) where

import Control.Lens
import Control.Exception (throwIO)
import Control.Exception.Lens (throwingM, _IOException)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit (HttpException)
import Network.HTTP.Types (Status(..))
import Test.Hspec.Lens
import System.IO.Error
import System.IO.Error.Lens (errorType, _NoSuchThing)

import Jenkins.Rest.Internal
import Network.HTTP.Conduit.Lens (_StatusCodeException, _TooManyRetries)

_JenkinsException :: Iso' JenkinsException HttpException
_JenkinsException = iso (\(JenkinsHttpException e) -> e) JenkinsHttpException

spec :: Spec
spec = do
  let raiseHttp, raiseIO :: Jenkins a
      raiseHttp = liftIO (throwingM _TooManyRetries ())
      raiseIO   = liftIO (throwIO (mkIOError doesNotExistErrorType "foo" Nothing Nothing))

  describe "runJenkins" $ do
    it "catches 'HttpException' exceptions related to Jenkins queries" $
      runJenkins (defaultConnectInfo & jenkinsPort .~ 80) (liftJ (Get "hi" id))
     `shouldPerform`
      Status 404 ""
     `through`
      _Error._JenkinsException._StatusCodeException._1

    it "does not catch 'HttpException' exceptions not related to Jenkins queries" $
      runJenkins defaultConnectInfo raiseHttp `shouldThrow` _TooManyRetries

    it "does not catch 'IOException' exceptions" $
      runJenkins defaultConnectInfo raiseIO `shouldThrow` _IOException.errorType._NoSuchThing

  describe "runJenkinsThrowing" $ do
    it "does not catch 'HttpException' exceptions" $
      runJenkinsThrowing defaultConnectInfo raiseHttp `shouldThrow` _TooManyRetries

    it "does not catch 'IOException' exceptions" $
      runJenkinsThrowing defaultConnectInfo raiseIO `shouldThrow` _IOException.errorType._NoSuchThing
