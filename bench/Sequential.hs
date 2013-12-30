{-# LANGUAGE OverloadedStrings #-}
-- | Show jobs descriptions
--
-- Does all jobs API calls sequentially, otherwise is exactly like Concurrent.hs
module Main (main) where

import           Control.Lens                 -- lens
import           Control.Lens.Aeson           -- lens-aeson
import qualified Data.ByteString.Char8 as B   -- bytestring
import           Data.Text (Text)             -- text
import           Jenkins.REST                 -- libjenkins
import           System.Environment (getArgs) -- base
import           System.Exit (exitFailure)    -- base


main :: IO ()
main = do
  h:p:user:pass:_ <- getArgs
  ds <- descriptions $
    Settings (Host h) (Port (read p)) (User (B.pack user)) (APIToken (B.pack pass))
  case ds of
    Right ds' -> mapM_ print ds'
    Left  e  -> do
      print e
      exitFailure

descriptions :: Settings -> IO (Either Disconnect [Maybe Text])
descriptions settings = runJenkins settings $ do
  res <- get (json -?- "tree" -=- "jobs[name]")
  let jobs = res ^.. key "jobs"._Array.each.key "name"._String
  mapM describe jobs

describe :: Text -> Jenkins (Maybe Text)
describe name = do
  desc <- get (job name `as` json -?- "tree" -=- "description")
  return (desc ^? key "description"._String)
