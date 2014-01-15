{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
import           Control.Lens.Aeson            -- lens-aeson
import qualified Data.ByteString.Char8 as B    -- bytestring
import           Data.Text (Text)              -- text
import           Jenkins.Rest                  -- libjenkins
import           System.Environment (getArgs)  -- base
import           System.Exit (exitFailure)     -- base
import           System.IO (hPutStrLn, stderr) -- base


type Aggregate a b = (a -> Jenkins b) -> [a] -> Jenkins [b]

main :: IO ()
main = do
  m:host:port:user:pass:_ <- getArgs
  ds <- descriptions (aggregate m) $
    ConnectInfo host (read port) (B.pack user) (B.pack pass)
  case ds of
    Result ds  -> mapM_ print ds
    Disconnect -> die "disconnect!"
    Error e    -> die (show e)
 where
  die message = do
    hPutStrLn stderr message
    exitFailure

  aggregate :: String -> Aggregate a b
  aggregate "concurrent" = (concurrentlys .) . map
  aggregate "sequential" = mapM
  aggregate _ = error "Unknown mode"

descriptions
  :: Aggregate Text (Maybe Text)
  -> ConnectInfo
  -> IO (Result HttpException [Maybe Text])
descriptions aggregate settings = runJenkins settings $ do
  res <- get (json -?- "tree" -=- "jobs[name]")
  aggregate describe (res ^.. key "jobs".values.key "name"._String)

describe :: Text -> Jenkins (Maybe Text)
describe name = do
  desc <- get (job name `as` json -?- "tree" -=- "description")
  return (desc ^? key "description"._String)
