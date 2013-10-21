{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Rename jobs matching supplied pattern
module Main where

import           Control.Lens                      -- lens
import           Control.Lens.Aeson                -- lens-aeson
import           Control.Monad (when)              -- base
import qualified Data.ByteString.Char8 as B        -- bytestring
import           Data.Foldable (for_)              -- base
import           Data.Function (fix)               -- base
import           Data.Text (Text)                  -- text
import qualified Data.Text as T                    -- text
import qualified Data.Text.IO as T                 -- text
import           Jenkins.REST                      -- libjenkins
import           Options.Applicative               -- optparse-applicative
import           System.Exit (exitFailure)         -- base

{-# ANN module ("HLint: Use camelCase" :: String) #-}


main :: IO ()
main = do
  -- more useful help on error
  opts <- customExecParser (prefs showHelpOnError) options
  res  <- rename opts
  case res of
    Right () -> T.putStrLn "Done."
    -- something bad happened, show it!
    Left  e  -> do
      print e
      exitFailure

-- | Prompt to rename all jobs matching pattern
rename :: Options -> IO (Either Disconnect ())
rename (Options { settings, old, new }) = runJenkins settings $ do
  -- get jobs names from jenkins "root" API
  res <- get (json -?- "tree" -=- "jobs[name]")
  let jobs = res ^.. key "jobs"._Array.each.key "name"._String
  for_ jobs rename_job
 where
  rename_job :: Text -> Jenkins ()
  rename_job name = when (old `T.isInfixOf` name) $ do
    let name' = (old `T.replace` new) name
    -- prompt for every matching job
    yes <- prompt $ T.unwords ["Rename", name, "to", name', "? [y/n]"]
    when yes $
      -- if user agrees then voodoo comes
      post_ (job name -/- "doRename" -?- "newName" -=- name')

  -- asks user until she enters 'y' or 'n'
  prompt message = io . fix $ \loop -> do
    T.putStrLn message
    res <- T.getLine
    case T.toUpper res of
      "Y" -> return True
      "N" -> return False
      _   -> loop


-- | Program options
data Options = Options
  { settings :: Settings
  , old      :: Text
  , new      :: Text
  }

-- | Quite a trivial options parser
options :: ParserInfo Options
options = info (helper <*> parser) fullDesc
 where
  parser = Options
    <$> parse_settings
    <*> nullOption (reader (return . T.pack) <> long "old")
    <*> nullOption (reader (return . T.pack) <> long "new")

  parse_settings = Settings
    <$> (Host <$> strOption (long "host"))
    <*> (Port <$> option (long "port"))
    <*> (User . B.pack <$> strOption (long "user"))
    <*> (APIToken . B.pack <$> strOption (long "token" <> long "password"))
