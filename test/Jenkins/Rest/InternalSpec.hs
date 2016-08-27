{-# LANGUAGE OverloadedStrings #-}
module Jenkins.Rest.InternalSpec (spec) where

import           Control.Lens
import           Control.Exception (throwIO)
import           Control.Exception.Lens (_IOException)
import           Network.HTTP.Client (HttpException)
import qualified Network.HTTP.Client as Http
import           Test.Hspec.Lens
import           System.IO.Error
import           System.IO.Error.Lens (errorType, _NoSuchThing)

import           Jenkins.Rest (Jenkins, liftIO)
import qualified Jenkins.Rest as Jenkins
import           Jenkins.Rest.Internal
import           Network.HTTP.Client.Lens (_HttpException)

_JenkinsException :: Iso' JenkinsException HttpException
_JenkinsException = iso (\(JenkinsHttpException e) -> e) JenkinsHttpException

spec :: Spec
spec = do
  let raiseIO :: Jenkins a
      raiseIO = liftIO (throwIO (mkIOError doesNotExistErrorType "foo" Nothing Nothing))
      master = Jenkins.Master {
          Jenkins.url = "http://example.com/jenkins"
        , Jenkins.user = "jenkins"
        , Jenkins.apiToken = "secret"
        }

  describe "runJenkins" $ do
    it "wraps uncatched 'HttpException' exceptions from the queries in 'Error'" $ do
      r <- Jenkins.run master (Jenkins.get Jenkins.plain "hi")
      r `shouldHave` _Left._JenkinsException

    it "wraps uncatched 'HttpException' exceptions from the URL parsing in 'Error'" $ do
      r <- Jenkins.run (master { Jenkins.url = "foo" }) (Jenkins.get Jenkins.plain "hi")
      r `shouldHave` _Left._JenkinsException

    it "can catch 'HttpException' exceptions related from the queries" $ do
      r <- Jenkins.run master
        (liftJ (Or (Jenkins.get Jenkins.plain "hi" >> return 4) (\_ -> return 7)))
      r `shouldPreview` (7 :: Integer) `through` _Right

    it "does not catch (and wrap) 'HttpException's not from the queries" $
      Jenkins.run master (Http.parseUrlThrow "foo") `shouldThrow` _HttpException

    it "does not catch (and wrap) 'IOException's" $
      Jenkins.run master raiseIO `shouldThrow` _IOException.errorType._NoSuchThing
