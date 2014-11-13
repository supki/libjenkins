{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | A simple Groovy REPL (type :q to quit)
--
-- @
-- >>> println "Hello, World!"
-- Hello, World!
-- >>> println 4 * 7
-- 28
-- @
module Main (main) where

import           Control.Applicative                  -- base
import           Data.Functor.Identity (Identity(..)) -- transformers
import           Data.Text.Lazy (Text)                -- text
import qualified Data.Text.Lazy.IO as Text            -- text
import           Env                                  -- envparse
import           Jenkins.Rest as Jenkins              -- libjenkins
import qualified System.IO as IO                      -- base


main :: IO ()
main = do
  conf <- envConf
  Disconnect
       <- Jenkins.run conf loop
  return ()

envConf :: IO Jenkins.Master
envConf = Env.parse (desc "A simple Groovy REPL") $
  pure Jenkins.defaultMaster
    <**> le Jenkins.url      (var str "JENKINS_URL"       (help "Jenkins URL"))
    <**> le Jenkins.user     (var str "JENKINS_USERNAME"  (help "Jenkins username"))
    <**> le Jenkins.apiToken (var str "JENKINS_API_TOKEN" (help "Jenkins API token"))

loop :: Jenkins ()
loop = do
  reply <- prompt
  case reply of
    ":q" -> disconnect
    _    -> do
      res <- groovy reply
      liftIO $ do Text.putStr res; flush;
      loop

prompt :: Jenkins Text
prompt = liftIO $ do Text.putStr ">>> "; flush; Text.getLine

flush :: IO ()
flush = IO.hFlush IO.stdout

-- My Little Lens

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

set :: Lens' s a -> a -> s -> s
set l x = runIdentity . l (Identity . const x)

le :: Functor f => Lens' s a -> f a -> f (s -> s)
le = fmap . set
