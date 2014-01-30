{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Rename jobs matching supplied pattern
module Main (main) where

import           Control.Lens                  -- lens
import           Control.Monad (when)          -- base
import           Data.Aeson.Lens               -- lens
import           Data.Foldable (for_)          -- base
import           Data.Function (fix)           -- base
import           Data.String (fromString)      -- base
import           Data.Text (Text)              -- text
import qualified Data.Text as Text             -- text
import qualified Data.Text.IO as Text          -- text
import           Jenkins.Rest                  -- libjenkins
import           System.Environment (getArgs)  -- base
import           System.Exit (exitFailure)     -- base
import           System.IO (hPutStrLn, stderr) -- base

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- | Program options
data Options = Options
  { settings :: ConnectInfo
  , old      :: Text
  , new      :: Text
  }


main :: IO ()
main = do
  -- more useful help on error
  host:port:user:pass:o:n:_ <- getArgs
  let opts = Options (ConnectInfo host (read port) (fromString user) (fromString pass)) (fromString o) (fromString n)
  res <- rename opts
  case res of
    Result _ -> Text.putStrLn "Done."
    -- disconnected for some reason
    Disconnect -> die "disconnect!"
    -- something bad happened, show it!
    Error e -> die (show e)
 where
  die message = do
    hPutStrLn stderr message
    exitFailure

-- | Prompt to rename all jobs matching pattern
rename :: Options -> IO (Result HttpException ())
rename (Options { settings, old, new }) = runJenkins settings $ do
  -- get jobs names from jenkins "root" API
  res <- get (json -?- "tree" -=- "jobs[name]")
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
      post_ (job name -/- "doRename" -?- "newName" -=- name')

  -- asks user until she enters 'y' or 'n'
  prompt message = io . fix $ \loop -> do
    Text.putStrLn message
    res <- Text.getLine
    case Text.toUpper res of
      "Y" -> return True
      "N" -> return False
      _   -> loop
