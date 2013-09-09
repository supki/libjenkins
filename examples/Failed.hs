{-# LANGUAGE OverloadedStrings #-}
-- | Show jobs which last build has failed
module Main where

import Control.Lens
import Control.Lens.Aeson
import Control.Monad (filterM)
import Data.Text (Text)
import Jenkins
import Jenkins.REST.Method
import Network.HTTP.Conduit (HttpException)


failed :: IO (Either HttpException [Text])
failed = do
  let user     = "__USER__"
      password = "__PASSWORD__"
  withJenkins "__JENKINS_URL__" __PORT__ user password $ do
    res <- get ("" `as` json)
    let jobs = res ^.. key "jobs"._Array.each.key "name"._String
    flip filterM jobs $ \job -> do
      res' <- get ("job" -/- text job `as` json)
      return $ elemOf (key "color") "red" res'
