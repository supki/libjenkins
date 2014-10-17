{-# LANGUAGE OverloadedStrings #-}
-- | Concurrency benchmark
--
-- The benchmark does the folllowing:
--
--   * it asks Jenkins instance for accessible jobs
--
--   * it sends queries to get jobs descriptions
--
--   * it prints all descriptions
--
-- The benchmark can be run sequentially or concurrently
--
-- See @bench/README.md@ for usage instructions
module Main (main) where

import           Control.Lens                  -- lens
import           Data.Aeson.Lens               -- lens-aeson
import           Data.Text (Text)              -- text
import qualified Data.Text as Text             -- text
import           Jenkins.Rest                  -- libjenkins
import           System.Environment (getArgs)  -- base
import           System.Exit (exitFailure)     -- base
import           System.IO (hPutStrLn, stderr) -- base


type Aggregate a b = (a -> Jenkins b) -> [a] -> Jenkins [b]

main :: IO ()
main = do
  m:url:user:token:_ <- getArgs
  ds <- descriptions (aggregate m) $
    defaultConnectInfo
    & jenkinsUrl .~ url
    & jenkinsUser .~ Text.pack user
    & jenkinsApiToken .~ Text.pack token
  case ds of
    Result ds' -> mapM_ print ds'
    Disconnect -> die "disconnect!"
    Error e    -> die (show e)
 where
  die message = do
    hPutStrLn stderr message
    exitFailure

  aggregate :: String -> Aggregate a b
  aggregate "concurrent" = traverseC
  aggregate "sequential" = mapM
  aggregate _ = error "Unknown mode"

descriptions
  :: Aggregate Text (Maybe Text)
  -> ConnectInfo
  -> IO (Result JenkinsException [Maybe Text])
descriptions aggregate settings = runJenkins settings $ do
  res <- get (json -?- "tree" -=- "jobs[name]")
  aggregate describe (res ^.. key "jobs".values.key "name"._String)

describe :: Text -> Jenkins (Maybe Text)
describe name = do
  desc <- get (job name `as` json -?- "tree" -=- "description")
  return (desc ^? key "description"._String)
