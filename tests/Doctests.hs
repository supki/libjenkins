module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM, liftM)
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Test.DocTest (doctest)


main :: IO ()
main = do
  files <- sources "src"
  doctest (sandbox:files)
 where
  sandbox = "-package-db=.cabal-sandbox/x86_64-linux-ghc-7.6.2-packages.conf.d"

sources :: FilePath -> IO [FilePath]
sources root = do
  files <- contents root
  liftM concat . forM files $ \file -> do
    let path = root </> file
    is_dir <- doesDirectoryExist path
    case is_dir of
      True              -> sources path
      _ | isSource path -> return [path]
        | otherwise     -> return []
 where
  isSource path = any (`isSuffixOf` path) [".hs", ".lhs"]

contents :: FilePath -> IO [FilePath]
contents dir = filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
