{-# LANGUAGE OverloadedStrings #-}
-- | Count running jobs on Jenkins instance
--
-- Uses an awful hack, that is inspecting the job ball color. Jenkins sets
-- it to "blue_anime", meaning "animated blue ball" if job is running
module Main (main) where

import           Control.Lens                      -- lens
import           Data.Aeson.Lens                   -- lens-aeson
import           Data.ByteString.Lazy (ByteString) -- bytestring
import           Env                               -- envparse
import           Jenkins.Rest (Jenkins, (-?-), (-=-))
import qualified Jenkins.Rest as Jenkins           -- libjenkins
import           Text.Printf (printf)              -- base


main :: IO ()
main = do
  conf <- envConf
  jobs <- Jenkins.run conf getJobs
  printf "Running jobs count: %d\n" (lengthOf (_Right.running) jobs)

envConf :: IO Jenkins.Master
envConf = Env.parse (desc "Get running jobs count") $
  Jenkins.Master <$> var str "JENKINS_URL"       (help "Jenkins URL")
                 <*> var str "JENKINS_USERNAME"  (help "Jenkins username")
                 <*> var str "JENKINS_API_TOKEN" (help "Jenkins API token")

getJobs :: Jenkins ByteString
getJobs = Jenkins.get Jenkins.json ("" -?- "tree" -=- "jobs[color]")

running :: Fold ByteString ()
running = key "jobs".values.key "color"._String.only "blue_anime"
