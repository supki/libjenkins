{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module OptionsSpec where

import Control.Lens
import Data.Monoid (mempty)
import Data.String (IsString)
import Options.Applicative
import Test.Hspec
import Test.Hspec.Lens

import Options
  ( Command
  , Greppable(..)
  , subcommands
  , _Grep, _Get, _Enable, _Disable, _Build, _Delete, _Rename, _Queue
  )

instance Show ParserFailure where
  show _ = "<ParserFailure>"

spec :: Spec
spec = do
  describe "grep option" $ do
    it "mandates at least one pattern argument" $
      grep mempty `shouldNotHave` _Right

    it "matches job names if --name flag is provided" $
      grep ["--name", foo] `shouldPreview` Name foo `through` _Right._Grep.folded

    it "matches job descriptions if --description flag is provided" $
      grep ["--description", foo] `shouldPreview` Description foo `through` _Right._Grep.folded

    it "matches job colors if --color flag is provided" $
      grep ["--color", foo] `shouldPreview` Color foo `through` _Right._Grep.folded

    it "matches multiple patterns" $
      grep ["--name", foo, "--name", bar, "--description", baz]
        `shouldList` [Name foo, Name bar, Description baz] `through` _Right._Grep.folded


  describe "get command" $ do
    it "does not have mandatory arguments" $
      get mempty `shouldHave` _Right._Get

    it "parses multiple arguments" $
      get [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Right._Get.folded

  describe "enable command" $ do
    it "does not have mandatory arguments" $
      enable mempty `shouldHave` _Right._Enable

    it "parses multiple arguments" $
      enable [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Right._Enable.folded

  describe "disable command" $ do
    it "does not have mandatory arguments" $
      disable mempty `shouldHave` _Right._Disable

    it "parses multiple argument" $
      disable [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Right._Disable.folded

  describe "build command" $ do
    it "does not have mandatory arguments" $
      build mempty `shouldHave` _Right._Build

    it "parses multiple argument" $
      build [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Right._Build.folded

  describe "delete command" $ do
    it "does not have mandatory arguments" $
      delete mempty `shouldHave` _Right._Delete

    it "parses multiple argument" $
      delete [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Right._Delete.folded


  describe "rename command" $ do
    it "mandates a --from pattern argument" $
      rename ["--from", foo] `shouldNotHave` _Right

    it "mandates a --to pattern argument" $
      rename ["--to", bar] `shouldNotHave` _Right

    it "parses --from and --to pattern arguments" $
      rename ["--from", foo, "--to", bar] `shouldPreview` (foo, bar) `through` _Right._Rename

  describe "queue command" $
    it "does not mandate an argument" $
      queue mempty `shouldHave` _Right._Queue
 where
  [grep, get, enable, disable, build, delete, rename, queue] = map parse
    ["grep", "get", "enable", "disable", "build", "delete", "rename", "queue"]

parse :: String -> [String] -> Either ParserFailure Options.Command
parse c = execParserPure (prefs mempty) (info (subparser subcommands) fullDesc) . (c:)

foo, bar, baz :: IsString s => s
foo = "foo"
bar = "bar"
baz = "baz"
