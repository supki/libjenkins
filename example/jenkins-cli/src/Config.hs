{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Config
  ( readConfig
#ifdef TEST
  , Config(..)
#endif
  ) where

import           Control.Lens
import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Jenkins.Rest as Jenkins
import           System.Directory (getAppUserDataDirectory)
import           System.FilePath ((</>))


newtype Config = Config { _unConfig :: Jenkins.Master }
  deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object o) = do
    url   <- o .: "url"
    user  <- o .: "user"
    token <- o .: "api-token"
    return (Config (Jenkins.defaultMaster
      & set Jenkins.url url
      & set Jenkins.user user
      & set Jenkins.apiToken token))
  parseJSON _ = empty

readConfig :: IO Jenkins.Master
readConfig = do
  appData <- getAppUserDataDirectory "jenkins-cli"
  either error _unConfig . eitherDecode <$> ByteString.readFile (appData </> "conf.json")
