{-# LANGUAGE OverloadedStrings #-}
module Jenkins.Rest.InternalSpec (spec) where

import           Control.Lens
import           Control.Exception (throwIO)
import           Control.Exception.Lens (throwingM, _IOException)
import           Network.HTTP.Client (HttpException)
import           Network.HTTP.Types (Status(..))
import           Test.Hspec.Lens
import           System.IO.Error
import           System.IO.Error.Lens (errorType, _NoSuchThing)

import           Jenkins.Rest (Jenkins, liftIO)
import qualified Jenkins.Rest as Jenkins
import           Jenkins.Rest.Internal
import           Network.HTTP.Client.Lens (_StatusCodeException, _InvalidUrlException, _TooManyRetries)

_JenkinsException :: Iso' JenkinsException HttpException
_JenkinsException = iso (\(JenkinsHttpException e) -> e) JenkinsHttpException

spec :: Spec
spec = do
  let raiseHttp, raiseIO :: Jenkins a
      raiseHttp = liftIO (throwingM _TooManyRetries ())
      raiseIO   = liftIO (throwIO (mkIOError doesNotExistErrorType "foo" Nothing Nothing))

  describe "runJenkins" $ do
    it "wraps uncatched 'HttpException' exceptions from the queries in 'Error'" $
      Jenkins.run Jenkins.defaultMaster (Jenkins.get Jenkins.plain "hi")
     `shouldPerform`
      Status 404 ""
     `through`
      _Left._JenkinsException._StatusCodeException._1

    it "wraps uncatched 'HttpException' exceptions from the URL parsing in 'Error'" $
      Jenkins.run (Jenkins.defaultMaster & Jenkins.url .~ "foo") (Jenkins.get Jenkins.plain "hi")
     `shouldPerform`
      ("foo", "Invalid URL")
     `through`
      _Left._JenkinsException._InvalidUrlException

    it "can catch 'HttpException' exceptions related from the queries" $
      Jenkins.run Jenkins.defaultMaster
        (liftJ (Or (Jenkins.get Jenkins.plain "hi" >> return 4) (\_ -> return 7)))
     `shouldPerform`
      7
     `through`
      _Right

    it "does not catch (and wrap) 'HttpException's not from the queries" $
      Jenkins.run Jenkins.defaultMaster raiseHttp `shouldThrow` _TooManyRetries

    it "does not catch (and wrap) 'IOException's" $
      Jenkins.run Jenkins.defaultMaster raiseIO `shouldThrow` _IOException.errorType._NoSuchThing
