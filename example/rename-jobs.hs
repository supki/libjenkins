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
import           Env                           -- envparse
import           Jenkins.Rest (Jenkins, JenkinsException, (-?-), (-=-), (-/-), liftIO)
import qualified Jenkins.Rest as Jenkins       -- libjenkins
import           System.Environment (getArgs)  -- base
import           System.Exit (exitFailure)     -- base
import qualified System.IO as IO               -- base

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


main :: IO ()
main = do
  o : n : _
       <- getArgs
  conf <- envConf
  res  <- rename conf (fromString o) (fromString n)
  case res of
    Right _ -> Text.putStrLn "Done."
    -- something bad happened, show it!
    Left e  -> die (show e)

die :: String -> IO a
die m = do IO.hPutStrLn IO.stderr m; exitFailure

envConf :: IO Jenkins.Master
envConf = Env.parse (desc "Rename jobs") $
  Jenkins.Master <$> var str "JENKINS_URL"       (help "Jenkins URL")
                 <*> var str "JENKINS_USERNAME"  (help "Jenkins username")
                 <*> var str "JENKINS_API_TOKEN" (help "Jenkins API token")

-- | Prompt to rename all jobs matching pattern
rename :: Jenkins.Master -> Text -> Text -> IO (Either JenkinsException ())
rename conf old new = Jenkins.run conf $ do
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
