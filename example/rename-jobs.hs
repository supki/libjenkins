{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Rename jobs matching supplied pattern
module Main (main) where

import           Control.Applicative           -- base
import           Control.Lens                  -- lens
import           Control.Monad (when)          -- base
import           Data.Aeson.Lens               -- lens-aeson
import           Data.Foldable (for_)          -- base
import           Data.Function (fix)           -- base
import           Data.String (fromString)      -- base
import           Data.Text (Text)              -- text
import qualified Data.Text as Text             -- text
import qualified Data.Text.IO as Text          -- text
import           Jenkins.Rest (Jenkins, (-?-), (-=-), (-/-), liftIO)
import qualified Jenkins.Rest as Jenkins       -- libjenkins
import           System.Environment (getArgs)  -- base
import           System.Exit (exitFailure)     -- base
import           System.IO (hPutStrLn, stderr) -- base

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- | Program options
data Options = Options
  { settings :: Jenkins.Master
  , old      :: Text
  , new      :: Text
  }


main :: IO ()
main = do
  -- more useful help on error
  url:user:token:o:n:_ <- getArgs
  let cinf = Jenkins.defaultMaster
        & Jenkins.url .~ url
        & Jenkins.user .~ fromString user
        & Jenkins.apiToken .~ fromString token
      opts = Options cinf (fromString o) (fromString n)
  res <- rename opts
  case res of
    Jenkins.Ok _ -> Text.putStrLn "Done."
    -- disconnected for some reason
    Jenkins.Disconnect -> die "disconnect!"
    -- something bad happened, show it!
    Jenkins.Exception e -> die (show e)
 where
  die message = do
    hPutStrLn stderr message
    exitFailure

-- | Prompt to rename all jobs matching pattern
rename :: Options -> IO (Jenkins.Result ())
rename (Options { settings, old, new }) = Jenkins.run settings $ do
  -- get jobs names from jenkins "root" API
  res <- Jenkins.get Jenkins.json ("/" -?- "tree" -=- "jobs[name]")
  let jobs = res ^.. key "jobs".values.key "name"._String
  for_ jobs rename_job
 where
  rename_job :: Text -> Jenkins ()
  rename_job name = when (old `Text.isInfixOf` name) $ do
    let name' = (old `Text.replace` new) name
    -- prompt for every matching job
    yes <- prompt $ Text.unwords ["Rename", name, "to", name', "? [y/n]"]
    when yes $
      -- if user agrees then voodoo comes
      () <$ Jenkins.post_ (Jenkins.job name -/- "doRename" -?- "newName" -=- name')

  -- asks user until she enters 'y' or 'n'
  prompt message = liftIO . fix $ \loop -> do
    Text.putStrLn message
    res <- Text.getLine
    case Text.toUpper res of
      "Y" -> return True
      "N" -> return False
      _   -> loop
