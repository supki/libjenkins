{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Rename jobs matching supplied pattern
module Main where

import           Control.Lens                  -- lens
import           Control.Lens.Aeson            -- lens-aeson
import           Control.Monad (when)          -- base
import qualified Data.ByteString.Char8 as B    -- bytestring
import           Data.Foldable (for_)          -- base
import           Data.Function (fix)           -- base
import           Data.Text (Text)              -- text
import qualified Data.Text as T                -- text
import qualified Data.Text.IO as T             -- text
import           Jenkins.REST                  -- libjenkins
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
  h:p:user:pass:o:n:_ <- getArgs
  let opts = Options (ConnectInfo h (read p) (B.pack user) (B.pack pass)) (T.pack o) (T.pack n)
  res <- rename opts
  case res of
    Right (Just ()) -> T.putStrLn "Done."
    -- disconnected for some reason
    Right Nothing -> die "disconnect!"
    -- something bad happened, show it!
    Left e -> die (show e)
 where
  die message = do
    hPutStrLn stderr message
    exitFailure

-- | Prompt to rename all jobs matching pattern
rename :: Options -> IO (Either HttpException (Maybe ()))
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
