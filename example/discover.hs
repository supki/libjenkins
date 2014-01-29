{-# LANGUAGE OverloadedStrings #-}
-- | Discover Jenkins server on the network
module Main (main) where

import           Data.Monoid ((<>), mempty) -- base
import           Data.Text (Text)           -- text
import qualified Data.Text as T             -- text
import qualified Data.Text.IO as T          -- text
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
    _  -> mapM_ (T.putStrLn . pretty) discoveries

-- | Pretty print Jenkins discovery responses
pretty :: Discover -> Text
pretty x = T.unwords $
  ["Jenkins", version x] ++ maybe mempty (return . between "(" ")") (server_id x) ++ ["at", url x]
 where
  between l r t = l <> t <> r

