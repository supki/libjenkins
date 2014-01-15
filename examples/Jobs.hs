{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Show jobs status
module Main where

import           Control.Lens                  -- lens
import           Control.Lens.Aeson            -- lens-aeson
import qualified Data.ByteString.Char8 as B    -- bytestring
import           Data.Text (Text)              -- text
import qualified Data.Text.IO as T             -- text
import           Jenkins.REST hiding (render)  -- libjenkins
import           System.Console.ANSI           -- ansi-terminal
import           System.Environment (getArgs)  -- base
import           System.Exit (exitFailure)     -- base
import           System.IO (hPutStrLn, stderr) -- base

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- | Job name and status
data Job = Job
  { name  :: Text
  , color :: Color
  }


main :: IO ()
main = do
  -- more useful help on error
  h:p:user:pass:_ <- getArgs
  let conn = ConnectInfo h (read p) (B.pack user) (B.pack pass)
  -- get all jobs (colored)
  jobs <- colorized_jobs conn
  case jobs of
    -- render them
    Result js -> mapM_ render js
    -- disconnected for some reason
    Disconnect -> die "disconnect!"
    -- something bad happened, show it!
    Error e -> die (show e)
 where
  die message = do
    hPutStrLn stderr message
    exitFailure

-- get jobs names from jenkins "root" API
colorized_jobs :: ConnectInfo -> IO (Result HttpException [Job])
colorized_jobs conn = runJenkins conn $ do
  res <- get (json -?- "tree" -=- "jobs[name]")
  let jobs = res ^.. key "jobs".values.key "name"._String
  concurrentlys (map colorize jobs)

-- get jobs colors as they appear on web UI
colorize :: Text -> Jenkins Job
colorize name = do
  res <- get (job name `as` json -?- "tree" -=- "color")
  return . Job name $ case res ^? key "color" of
    -- but sane
    Just "red"  -> Red
    Just "blue" -> Green
    _           -> Yellow

-- render colored job (assumes ANSI terminal)
render :: Job -> IO ()
render Job { name, color } = do
  setSGR [SetColor Foreground Dull color]
  T.putStrLn name
  setSGR []
