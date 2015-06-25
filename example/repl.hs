{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A simple Groovy REPL (type :q to quit)
--
-- @
-- >>> println "Hello, World!"
-- Hello, World!
-- >>> println 4 * 7
-- 28
-- @
module Main (main) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative       -- base
#endif
import           Data.Text.Lazy (Text)     -- text
import qualified Data.Text.Lazy.IO as Text -- text
import           Env                       -- envparse
import           Jenkins.Rest as Jenkins   -- libjenkins
import qualified System.IO as IO           -- base


main :: IO ()
main = do
  conf <- envConf
  Right ()
       <- Jenkins.run conf loop
  return ()

envConf :: IO Jenkins.Master
envConf = Env.parse (desc "A simple Groovy REPL") $
  Jenkins.Master <$> var str "JENKINS_URL"       (help "Jenkins URL")
                 <*> var str "JENKINS_USERNAME"  (help "Jenkins username")
                 <*> var str "JENKINS_API_TOKEN" (help "Jenkins API token")

loop :: Jenkins ()
loop = do
  reply <- prompt
  case reply of
    ":q" -> return ()
    _    -> do
      res <- groovy reply
      liftIO $ do Text.putStr res; flush;
      loop

prompt :: Jenkins Text
prompt = liftIO $ do Text.putStr ">>> "; flush; Text.getLine

flush :: IO ()
flush = IO.hFlush IO.stdout
