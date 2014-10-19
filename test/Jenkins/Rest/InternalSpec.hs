{-# LANGUAGE OverloadedStrings #-}
module Jenkins.Rest.InternalSpec (spec) where

import           Control.Lens
import           Control.Exception (throwIO)
import           Control.Exception.Lens (throwingM, _IOException)
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Conduit (HttpException)
import           Network.HTTP.Types (Status(..))
import           Test.Hspec.Lens
import           System.IO.Error
import           System.IO.Error.Lens (errorType, _NoSuchThing)

import qualified Jenkins.Rest as Helper
import           Jenkins.Rest.Internal
import           Network.HTTP.Conduit.Lens (_StatusCodeException, _InvalidUrlException, _TooManyRetries)

_JenkinsException :: Iso' JenkinsException HttpException
_JenkinsException = iso (\(JenkinsHttpException e) -> e) JenkinsHttpException

spec :: Spec
spec = do
  let raiseHttp, raiseIO :: Jenkins a
      raiseHttp = liftIO (throwingM _TooManyRetries ())
      raiseIO   = liftIO (throwIO (mkIOError doesNotExistErrorType "foo" Nothing Nothing))

  describe "runJenkins" $ do
    it "wraps uncatched 'HttpException' exceptions from the queries in 'Error'" $
      runJenkins defaultConnectInfo (Helper.get Helper.plain "hi")
     `shouldPerform`
      Status 404 ""
     `through`
      _Error._JenkinsException._StatusCodeException._1

    it "wraps uncatched 'HttpException' exceptions from the URL parsing in 'Error'" $
      runJenkins (defaultConnectInfo & jenkinsUrl .~ "foo") (Helper.get Helper.plain "hi")
     `shouldPerform`
      ("foo", "Invalid URL")
     `through`
      _Error._JenkinsException._InvalidUrlException

    it "can catch 'HttpException' exceptions related from the queries" $
      runJenkins defaultConnectInfo
        (liftJ (Or (Helper.get Helper.plain "hi" >> return 4) (return 7)))
     `shouldPerform`
      7
     `through`
      _Result

    it "does not catch (and wrap) 'HttpException's not from the queries" $
      runJenkins defaultConnectInfo raiseHttp `shouldThrow` _TooManyRetries

    it "does not catch (and wrap) 'IOException's" $
      runJenkins defaultConnectInfo raiseIO `shouldThrow` _IOException.errorType._NoSuchThing
