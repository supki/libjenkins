module Options where

import Control.Lens hiding (argument)
import Data.Monoid (mconcat)
import Data.Foldable (asum)
import Data.Text (Text, pack)
import Options.Applicative hiding (subparser)
import Options.Applicative.Builder.Internal
import Options.Applicative.Types


data Command =
    Grep [Greppable]
  | Get [Text]
  | Enable [Text]
  | Disable [Text]
  | Build [Text]
  | Delete [Text]
  | Rename String String
  | Queue
    deriving (Show, Eq)

data Greppable =
    Name        { pattern :: String }
  | Description { pattern :: String }
  | Color       { pattern :: String }
    deriving (Show, Eq)

_Grep :: Prism' Command [Greppable]
_Grep = prism' Grep (\x -> case x of Grep p -> Just p; _ -> Nothing)
{-# INLINE _Grep #-}

_Get :: Prism' Command [Text]
_Get  = prism' Get (\x -> case x of Get p -> Just p; _ -> Nothing)
{-# INLINE _Get #-}

_Enable :: Prism' Command [Text]
_Enable = prism' Enable (\x -> case x of Enable p -> Just p; _ -> Nothing)
{-# INLINE _Enable #-}

_Disable :: Prism' Command [Text]
_Disable = prism' Disable (\x -> case x of Disable p -> Just p; _ -> Nothing)
{-# INLINE _Disable #-}

_Build :: Prism' Command [Text]
_Build = prism' Build (\x -> case x of Build p -> Just p; _ -> Nothing)
{-# INLINE _Build #-}

_Delete :: Prism' Command [Text]
_Delete = prism' Delete (\x -> case x of Delete p -> Just p; _ -> Nothing)
{-# INLINE _Delete #-}

_Rename :: Prism' Command (String, String)
_Rename = prism' (uncurry Rename) (\x -> case x of Rename a b -> Just (a, b); _ -> Nothing)
{-# INLINE _Rename #-}

_Queue :: Prism' Command ()
_Queue = prism' (const Queue) (\x -> case x of Queue -> Just (); _ -> Nothing)
{-# INLINE _Queue #-}

options :: ParserInfo Command
options = info (helper <*> parser) fullDesc
 where
  parser = subparser subcommands

subparser :: Mod CommandFields a -> Parser a
subparser m = mkParser d g rdr
 where
  Mod _ d g = metavar "COMMAND" <> m
  rdr = uncurry CmdReader (mkCommand m)

subcommands :: Mod CommandFields Command
subcommands = mconcat
  [ command "grep"    $ helpInfo (Grep <$> grep)           (progDesc "Grep jobs")
  , command "get"     $ helpInfo (Get <$> jobs)            (progDesc "Get jobs configurations")
  , command "enable"  $ helpInfo (Enable <$> jobs)         (progDesc "Enable jobs")
  , command "disable" $ helpInfo (Disable <$> jobs)        (progDesc "Disable jobs")
  , command "build"   $ helpInfo (Build <$> jobs)          (progDesc "Build jobs")
  , command "delete"  $ helpInfo (Delete <$> jobs)         (progDesc "Delete jobs")
  , command "rename"  $ helpInfo (uncurry Rename <$> args) (progDesc "Rename matched jobs")
  , command "queue"   $ helpInfo (pure Queue)              (progDesc "Get waiting jobs")
  ]
 where
  args = (,)
    <$> strOption (long "from" <> short 'f' <> perlPattern)
    <*> strOption (long "to"   <> short 't' <> perlPattern)

  grep = some $ asum
    [ Name        <$> strOption (long "name"        <> short 'n' <> help "grep job name")
    , Description <$> strOption (long "description" <> short 'd' <> help "grep job description")
    , Color       <$> strOption (long "color"       <> short 'c' <> help "grep job color")
    ]

  jobs = many (argument (fmap pack str) (help "Job name" <> metavar "JOB"))

  helpInfo parser = info (helper <*> parser)

  perlPattern = help "Perl regex" <> metavar "PATTERN"
