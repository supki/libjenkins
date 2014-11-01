{-# LANGUAGE OverloadedStrings #-}
-- | Discover Jenkins server on the network
module Main (main) where

import           Data.Monoid ((<>), mempty) -- base
import           Data.Text (Text)           -- text
import qualified Data.Text as Text          -- text
import qualified Data.Text.IO as Text       -- text
import           Jenkins.Discover           -- libjenkins
import           System.Exit (exitFailure)  -- base


main :: IO ()
main = do
  -- send discover broadcast with 100ms timeout
  discoveries <- discover 100000
  case discoveries of
    -- no Jenkins responded
    [] -> exitFailure
    -- pretty print responses
    _  -> mapM_ (Text.putStrLn . pretty) discoveries

-- | Pretty print Jenkins discovery responses
pretty :: Discover -> Text
pretty x = Text.unwords $
  "Jenkins" : version x : maybe mempty (return . between "(" ")") (serverId x) ++ ["at", url x]
 where
  between l r t = l <> t <> r

