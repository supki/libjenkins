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

spec :: Spec
spec = do
  describe "grep option" $ do
    it "mandates at least one pattern argument" $
      grep mempty `shouldNotHave` _Just

    it "matches job names if --name flag is provided" $
      grep ["--name", foo] `shouldPreview` Name foo `through` _Just._Grep.folded

    it "matches job descriptions if --description flag is provided" $
      grep ["--description", foo] `shouldPreview` Description foo `through` _Just._Grep.folded

    it "matches job colors if --color flag is provided" $
      grep ["--color", foo] `shouldPreview` Color foo `through` _Just._Grep.folded

    it "matches multiple patterns" $
      grep ["--name", foo, "--name", bar, "--description", baz]
        `shouldList` [Name foo, Name bar, Description baz] `through` _Just._Grep.folded


  describe "get command" $ do
    it "does not have mandatory arguments" $
      get mempty `shouldHave` _Just._Get

    it "parses multiple arguments" $
      get [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Just._Get.folded

  describe "enable command" $ do
    it "does not have mandatory arguments" $
      enable mempty `shouldHave` _Just._Enable

    it "parses multiple arguments" $
      enable [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Just._Enable.folded

  describe "disable command" $ do
    it "does not have mandatory arguments" $
      disable mempty `shouldHave` _Just._Disable

    it "parses multiple argument" $
      disable [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Just._Disable.folded

  describe "build command" $ do
    it "does not have mandatory arguments" $
      build mempty `shouldHave` _Just._Build

    it "parses multiple argument" $
      build [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Just._Build.folded

  describe "delete command" $ do
    it "does not have mandatory arguments" $
      delete mempty `shouldHave` _Just._Delete

    it "parses multiple argument" $
      delete [foo, bar, baz] `shouldList` [foo, bar, baz] `through` _Just._Delete.folded


  describe "rename command" $ do
    it "mandates a --from pattern argument" $
      rename ["--from", foo] `shouldNotHave` _Just

    it "mandates a --to pattern argument" $
      rename ["--to", bar] `shouldNotHave` _Just

    it "parses --from and --to pattern arguments" $
      rename ["--from", foo, "--to", bar] `shouldPreview` (foo, bar) `through` _Just._Rename

  describe "queue command" $
    it "does not mandate an argument" $
      queue mempty `shouldHave` _Just._Queue
 where
  [grep, get, enable, disable, build, delete, rename, queue] = map parse
    ["grep", "get", "enable", "disable", "build", "delete", "rename", "queue"]

parse :: String -> [String] -> Maybe Options.Command
parse c = getParseResult . execParserPure (prefs mempty) (info (subparser subcommands) fullDesc) . (c:)

foo, bar, baz :: IsString s => s
foo = "foo"
bar = "bar"
baz = "baz"
