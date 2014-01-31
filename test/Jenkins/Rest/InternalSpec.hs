module Jenkins.Rest.InternalSpec (spec) where

import Control.Exception (throwIO)
import Control.Exception.Lens (throwingM, _IOException)
import Control.Monad.IO.Class (liftIO)
import Test.Hspec.Lens
import System.IO.Error
import System.IO.Error.Lens (errorType, _NoSuchThing)

import Jenkins.Rest.Internal
import Network.HTTP.Conduit.Lens (_TooManyRetries)


spec :: Spec
spec = do
  let raiseHttp, raiseIO :: Jenkins a
      raiseHttp = liftIO (throwingM _TooManyRetries ())
      raiseIO   = liftIO (throwIO (mkIOError doesNotExistErrorType "foo" Nothing Nothing))

  describe "runJenkins" $ do
    it "catches 'HttpException' exceptions" $
      runJenkins defaultConnectInfo raiseHttp `shouldPerform` () `through` _Error._TooManyRetries

    it "does not catch 'IOException' exceptions" $
      runJenkins defaultConnectInfo raiseIO `shouldThrow` _IOException.errorType._NoSuchThing

  describe "runJenkinsThrowing" $ do
    it "does not catch 'HttpException' exceptions" $
      runJenkinsThrowing defaultConnectInfo raiseHttp `shouldThrow` _TooManyRetries

    it "does not catch 'IOException' exceptions" $
      runJenkinsThrowing defaultConnectInfo raiseIO `shouldThrow` _IOException.errorType._NoSuchThing
