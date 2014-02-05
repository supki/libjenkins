{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Config
  ( readConfig
#ifdef TEST
  , Config(..)
#endif
  ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import           Jenkins.Rest
import           System.Directory (getAppUserDataDirectory)
import           System.FilePath ((</>))


newtype Config = Config { _unConfig :: ConnectInfo }
  deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object o) = do
    url   <- o .: "url"
    port  <- o .: "port"
    user  <- o .: "user"
    token <- o .: "api-token"
    return (Config (ConnectInfo url port user token))
  parseJSON _ = empty

readConfig :: IO ConnectInfo
readConfig = do
  appData <- getAppUserDataDirectory "jenkins-cli"
  either error _unConfig . eitherDecode <$> ByteString.readFile (appData </> "conf.json")
