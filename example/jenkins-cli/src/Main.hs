{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad (filterM)
import           Data.Aeson (Value)
import           Data.Aeson.Lens
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as Lazy
import           Jenkins.Rest
import           Options.Applicative (customExecParser, prefs, showHelpOnError)
import           System.Exit (exitFailure)
import           System.Exit.Lens
import           System.IO (Handle, hPutStrLn, stdin, stderr)
import           System.Process (readProcessWithExitCode)
import           Text.Printf (printf)
import qualified Text.XML as XML

import           Config
import           Options (Command(..), Greppable(..), options)


main :: IO ()
main = do
  comm <- customExecParser (prefs showHelpOnError) options
  jenk <- readConfig
  resp <- runJenkins jenk $ case comm of
    Grep greppables -> grepJobs greppables
    Get jobs        -> withJobsOrHandle getJob stdin jobs
    Enable jobs     -> withJobsOrHandle enableJob stdin jobs
    Disable jobs    -> withJobsOrHandle disableJob stdin jobs
    Build jobs      -> withJobsOrHandle buildJob stdin jobs
    Delete jobs     -> withJobsOrHandle deleteJob stdin jobs
    Rename old new  -> withJobs (renameJob old new)
    Queue           -> waitJobs
  case resp of
    Error e -> die (show e)
    _       -> return ()

die :: String -> IO a
die message = do
  hPutStrLn stderr message
  exitFailure

withJobs :: (Text -> Jenkins a) -> Jenkins ()
withJobs j = getJobs >>= traverse_ j

getJobs :: Jenkins [Text]
getJobs = do
  res <- get (json -?- "tree" -=- "jobs[name]")
  return $ res ^.. key "jobs".values.key "name"._String

grepJobs :: [Greppable] -> Jenkins ()
grepJobs greppables = do
  json_root <- get (json -?- "tree" -=- "jobs[name,description,color]")
  io $ do
    let jobs = json_root ^.. key "jobs".values
    filtered_jobs <- applyFilters (map grep greppables) jobs
    mapM_ Text.putStrLn (filtered_jobs^..folded.key "name"._String)

applyFilters:: Monad m => [a -> m a] -> a -> m a
applyFilters []       a = return a
applyFilters (f : fs) a = f a >>= applyFilters fs

grep :: Greppable -> [Value] -> IO [Value]
grep greppable = filterM (match (pattern greppable) . property greppable)

property :: Greppable -> Value -> Text
property (Name        _) json_value = json_value^.singular (key "name"._String)
property (Description _) json_value = json_value^.singular (key "description"._String)
property (Color       _) json_value = json_value^.singular (key "color"._String)

withJobsOrHandle :: (Text -> Jenkins a) -> Handle -> [Text] -> Jenkins ()
withJobsOrHandle doThing handle [] =
  io (Text.hGetContents handle) >>= traverse_ doThing . Text.words
withJobsOrHandle doThing _      xs =
  traverse_ doThing xs

getJob :: Text -> Jenkins ()
getJob name = do
  config <- XML.parseLBS_ XML.def <$> get (job name -/-  "config.xml")
  io (Lazy.putStrLn (XML.renderText XML.def { XML.rsPretty = True } config))

enableJob :: Text -> Jenkins ()
enableJob = withJob "enable"

disableJob :: Text -> Jenkins ()
disableJob = withJob "disable"

buildJob :: Text -> Jenkins ()
buildJob = withJob "build"

deleteJob :: Text -> Jenkins ()
deleteJob = withJob "doDelete"

waitJobs :: Jenkins ()
waitJobs = get (queue `as` json) >>= io . printJobs
 where printJobs info = mapM_ Text.putStrLn (info ^.. key "items".values.key "task".key "name"._String)

withJob :: (forall f. Method Complete f) -> Text -> Jenkins ()
withJob doThing name = post_ (job name -/- doThing)

renameJob :: String -> String -> Text -> Jenkins ()
renameJob old new name = substitute old new name >>=
  traverse_ (\name' -> post_ (job name -/- "doRename" -?- "newName" -=- name'))

substitute :: String -> String -> Text -> Jenkins (Maybe Text)
substitute old new name = do
  (exitcode, stdout, _) <- io $
    readProcessWithExitCode "perl" ["-n", "-e", printf "print if s/%s/%s/ or die" old new] (Text.unpack name)
  return $ exitcode ^? _ExitSuccess.to (\_ -> Text.pack stdout)

match :: String -> Text -> IO Bool
match regex name = do
  (exitcode, _, _) <-
    readProcessWithExitCode "perl" ["-n", "-e", printf "print if /%s/ or die" regex] (Text.unpack name)
  return $ has _ExitSuccess exitcode
