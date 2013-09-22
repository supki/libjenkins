{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Show jobs status
module Main where

import           Control.Lens                 -- lens
import           Control.Lens.Aeson           -- lens-aeson
import qualified Data.ByteString.Char8 as B   -- bytestring
import           Data.Text (Text)             -- text
import qualified Data.Text.IO as T            -- text
import           Jenkins.REST hiding (render) -- libjenkins
import           Options.Applicative          -- optparse-applicative
import           System.Console.ANSI          -- ansi-terminal
import           System.Exit (exitFailure)    -- base

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

-- get jobs names from jenkins "root" API
colorized_jobs :: Settings -> IO (Either SomeException [Job])
colorized_jobs settings = jenkins settings $ do
  res <- get (json -?- "tree" -=- "jobs[name]")
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
render Job { name, color } = do
  setSGR [SetColor Foreground Dull color]
  T.putStrLn name
  setSGR []


-- | Quite a trivial jenkins settings parser
options :: ParserInfo Settings
options = info (helper <*> parser) fullDesc
 where
  parser = Settings
    <$> strOption (long "host")
    <*> option (long "port")
    <*> nullOption (reader (return . B.pack) <> long "user")
    <*> nullOption (reader (return . B.pack) <> long "token")
