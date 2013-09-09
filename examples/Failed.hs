{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Show jobs which last build has failed
module Main where

import           Control.Lens
import           Control.Lens.Aeson
import           Control.Monad (filterM)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO as T
import           Jenkins
import           Jenkins.REST.Method
import           Options.Applicative
import           System.Exit (exitFailure)

{-# ANN module ("HLint: Use camelCase" :: String) #-}


data Options = Options
  { url      :: String
  , port     :: Int
  , user     :: B.ByteString
  , password :: B.ByteString
  }


main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) options
  jobs <- find_failed opts
  case jobs of
    Right js -> mapM_ T.putStrLn js
    Left  e  -> do
      print e
      exitFailure
 where
  find_failed (Options { url, port, user, password }) = withJenkins url port user password $ do
    res <- get ("" `as` json)
    let jobs = res ^.. key "jobs"._Array.each.key "name"._String
    flip filterM jobs $ \job -> do
      res' <- get ("job" -/- text job `as` json)
      return $ elemOf (key "color") "red" res'

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
 where
  parser = Options
    <$> strOption (long "host" <> short 'h')
    <*> option (long "port" <> short 'p')
    <*> nullOption (reader (return . B.pack) <> long "user" <> short 'u')
    <*> nullOption (reader (return . B.pack) <> long "token" <> short 't')
