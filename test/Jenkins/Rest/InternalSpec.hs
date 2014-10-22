{-# LANGUAGE OverloadedStrings #-}
module Jenkins.Rest.InternalSpec (spec) where

import           Control.Lens
import           Control.Exception (throwIO)
import           Control.Exception.Lens (throwingM, _IOException)
import           Network.HTTP.Conduit (HttpException)
import           Network.HTTP.Types (Status(..))
import           Test.Hspec.Lens
import           System.IO.Error
import           System.IO.Error.Lens (errorType, _NoSuchThing)

import           Jenkins.Rest
import           Jenkins.Rest.Internal
import           Network.HTTP.Conduit.Lens (_StatusCodeException, _InvalidUrlException, _TooManyRetries)

_JenkinsException :: Iso' JenkinsException HttpException
_JenkinsException = iso (\(JenkinsHttpException e) -> e) JenkinsHttpException

spec :: Spec
spec = do
  let raiseHttp, raiseIO :: JenkinsT IO a
      raiseHttp = liftIO (throwingM _TooManyRetries ())
      raiseIO   = liftIO (throwIO (mkIOError doesNotExistErrorType "foo" Nothing Nothing))

  describe "runJenkins" $ do
    it "wraps uncatched 'HttpException' exceptions from the queries in 'Error'" $
      runJenkins defaultMaster (get plain "hi")
     `shouldPerform`
      Status 404 ""
     `through`
      _Exception._JenkinsException._StatusCodeException._1

    it "wraps uncatched 'HttpException' exceptions from the URL parsing in 'Error'" $
      runJenkins (defaultMaster & jenkinsUrl .~ "foo") (get plain "hi")
     `shouldPerform`
      ("foo", "Invalid URL")
     `through`
      _Exception._JenkinsException._InvalidUrlException

    it "can catch 'HttpException' exceptions related from the queries" $
      runJenkins defaultMaster
        (liftJ (Or (get plain "hi" >> return 4) (return 7)))
     `shouldPerform`
      7
     `through`
      _Ok

    it "does not catch (and wrap) 'HttpException's not from the queries" $
      runJenkins defaultMaster raiseHttp `shouldThrow` _TooManyRetries

    it "does not catch (and wrap) 'IOException's" $
      runJenkins defaultMaster raiseIO `shouldThrow` _IOException.errorType._NoSuchThing
