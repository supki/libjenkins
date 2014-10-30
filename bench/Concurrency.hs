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
import           Jenkins.Rest (Jenkins, (-?-), (-=-))
import qualified Jenkins.Rest as Jenkins       -- libjenkins
import           System.Environment (getArgs)  -- base
import           System.Exit (exitFailure)     -- base
import           System.IO (hPutStrLn, stderr) -- base


type Aggregate a b = (a -> Jenkins b) -> [a] -> Jenkins [b]

main :: IO ()
main = do
  m:url:user:token:_ <- getArgs
  ds <- descriptions (aggregate m) $
    Jenkins.defaultMaster
    & Jenkins.url .~ url
    & Jenkins.user .~ Text.pack user
    & Jenkins.apiToken .~ Text.pack token
  case ds of
    Jenkins.Ok ds'      -> mapM_ print ds'
    Jenkins.Disconnect  -> die "disconnect!"
    Jenkins.Exception e -> die (show e)
 where
  die message = do
    hPutStrLn stderr message
    exitFailure

  aggregate :: String -> Aggregate a b
  aggregate "concurrent" = Jenkins.traverse
  aggregate "sequential" = mapM
  aggregate _ = error "Unknown mode"

descriptions
  :: Aggregate Text (Maybe Text)
  -> Jenkins.Master
  -> IO (Jenkins.Result [Maybe Text])
descriptions aggregate settings = Jenkins.run settings $ do
  res <- Jenkins.get Jenkins.json ("" -?- "tree" -=- "jobs[name]")
  aggregate describe (res ^.. key "jobs".values.key "name"._String)

describe :: Text -> Jenkins (Maybe Text)
describe name = do
  desc <- Jenkins.get Jenkins.json (Jenkins.job name -?- "tree" -=- "description")
  return (desc ^? key "description"._String)
