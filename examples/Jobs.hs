{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Show jobs status
module Main where

import           Control.Lens                         -- lens
import           Control.Lens.Aeson                   -- lens-aeson
import qualified Data.ByteString.Char8 as B           -- bytestring
import           Data.Text (Text)                     -- text
import qualified Data.Text.IO as T                    -- text
import           Jenkins.REST                         -- libjenkins
import           Jenkins.REST.Method hiding (render)  -- libjenkins
import           Options.Applicative                  -- optparse-applicative
import           System.Console.ANSI                  -- ansi-terminal
import           System.Exit (exitFailure)            -- base

{-# ANN module ("HLint: Use camelCase" :: String) #-}


-- | Job name and status
data Job = Job
  { name  :: Text
  , color :: Color
  }


main :: IO ()
main = do
  -- more useful help on error
  opts <- customExecParser (prefs showHelpOnError) options
  -- get all jobs (colored)
  jobs <- colorized_jobs opts
  case jobs of
    -- render them
    Right js -> mapM_ render js
    -- something bad happened, show it!
    Left  e  -> do
      print e
      exitFailure
 where
  -- get jobs names from jenkins "root" API
  colorized_jobs (Options { .. }) =
    jenkins url port user password $ do
      res <- get ("" `as` json -?- "tree" -=- "jobs[name]")
      let jobs = res ^.. key "jobs"._Array.each.key "name"._String
      concurrently (map colorize jobs)

  -- get jobs colors as they appear on web UI
  colorize :: Text -> Jenkins Job
  colorize name = do
    res <- get ("job" -/- text name `as` json -?- "tree" -=- "color")
    return . Job name $ case res ^? key "color" of
      -- but sane
      Just "red"  -> Red
      Just "blue" -> Green
      _           -> Yellow

  -- render colored job (assumes ANSI terminal)
  render :: Job -> IO ()
  render Job { .. } = do
    setSGR [SetColor Foreground Dull color]
    T.putStrLn name
    setSGR []


-- | Program options
data Options = Options
  { url      :: String
  , port     :: Int
  , user     :: B.ByteString
  , password :: B.ByteString
  }

-- | Quite a trivial options parser
options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
 where
  parser = Options
    <$> strOption (long "host" <> short 'h')
    <*> option (long "port" <> short 'p')
    <*> nullOption (reader (return . B.pack) <> long "user" <> short 'u')
    <*> nullOption (reader (return . B.pack) <> long "token" <> short 't')
