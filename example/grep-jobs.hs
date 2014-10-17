{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Grep Jenkins instance jobs with Perl regex
module Main (main) where

import           Control.Lens                              -- lens
import           Control.Exception.Lens                    -- lens
import           Control.Monad                             -- base
import           Data.Aeson.Lens (key, values, _String)    -- lens-aeson
import           Data.String (fromString)                  -- base
import           Data.Text (Text)                          -- text
import qualified Data.Text as Text                         -- text
import qualified Data.Text.IO as Text                      -- text
import           Jenkins.Rest                              -- libjenkins
import           System.Environment (getArgs)              -- base
import           System.Exit.Lens                          -- lens
import           System.Process (readProcessWithExitCode)  -- process
import           Text.Printf (printf)                      -- base


main :: IO ()
main = do
  url:user:token:regex:_ <- getArgs
  jobs <- grep regex $ defaultConnectInfo
    & jenkinsUrl .~ url
    & jenkinsUser .~ fromString user
    & jenkinsApiToken .~ fromString token
  case jobs of
    [] -> throwingM _ExitFailure 1
    _  -> mapM_ Text.putStrLn jobs

-- | Filter matching job names
grep :: String -> ConnectInfo -> IO [Text]
grep regex conn = do
  jobs <- runJenkins conn $
    get (json -?- "tree" -=- "jobs[name]") <&> \res -> res ^.. key "jobs".values.key "name"._String
  filterM (match regex) (jobs ^.. _Result.folded)

-- | Match job name again Perl regex
match :: String -> Text -> IO Bool
match regex name = do
  (exitcode, _, _) <-
    readProcessWithExitCode "perl" ["-n", "-e", printf "/%s/ or die" regex] (Text.unpack name)
  return $ has _ExitSuccess exitcode
