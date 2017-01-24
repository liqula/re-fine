{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.DocRepo.HtmlFileTreeSpec where

import Data.String.Conversions
import Data.Text.IO as ST
import System.Directory
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Refine.Backend.DocRepo.HtmlFileTree
import Refine.Backend.Test.Util (withTempCurrentDirectory)
import Refine.Common.Test.Arbitrary ()
import Refine.Common.Types


instance Arbitrary FileTree where
  arbitrary = oneof
    [ File <$> arbitrary <*> arbitrary
    , Directory <$> arbitrary <*> scale (`div` 2) arbitrary
    ]

  shrink (File fp c) = File <$> shrink fp <*> shrink c
  shrink (Directory dp c) = Directory <$> shrink dp <*> shrink c


spec :: Spec
spec = do
  describe "htmlToFileTree" $ do
    it "creates files for text nodes." $
      htmlToFileTree (VDocVersion "phoo")
        `shouldBe` File "00001" "phoo"

    it "creates files for comment nodes." $
      htmlToFileTree (VDocVersion "<!-- phoo -->")
        `shouldBe` File "00001" "<!-- phoo -->"

    it "creates directories for tag nodes and stores tag names in file TAGNAME." $
      htmlToFileTree (VDocVersion "<div></div>")
        `shouldBe` Directory "00001" [File "TAGNAME" "div"]

    it "stores attribute key-value pairs in file ATTRS." $
      htmlToFileTree (VDocVersion "<div a=\"3\" bw=\"phoo\"></div>")
        `shouldBe` Directory "00001" [File "ATTRS" "a=\"3\"\nbw=\"phoo\"", File "TAGNAME" "div"]

    it "is inverse of htmlFromFileTree." . property $ \vers ->
      htmlFromFileTree (htmlToFileTree vers) `shouldBe` vers

  describe "writeHtml, readHtml" $ do
    it "are inverses." $ do
      -- (we've covered property checking of html to/from file tree above.  but we should run these
      -- two functions once anyway, just to make sure.)
      let vers = VDocVersion "<div a=\"3\" bw=\"phoo\">whee<p>bla</p>furgh<span>3</span></div>"
      withTempCurrentDirectory (writeHtml "." vers >>= readHtml)
        `shouldReturn` vers

  describe "readFileTree" $ do
    it "reads top-level file correctly." $ do
      fileTree <- withTempCurrentDirectory $ do
        ST.writeFile "name" "contents"
        readFileTree =<< getCurrentDirectory
      fileTree `shouldBe` (File "name" "contents")

    it "reads directory correctly." $ do
      fileTree <- withTempCurrentDirectory $ do
        createDirectory "name"
        readFileTree =<< getCurrentDirectory
      fileTree `shouldBe` (Directory "name" [])

    it "reads nested file correctly." $ do
      fileTree <- withTempCurrentDirectory $ do
        createDirectory "dir"
        ST.writeFile "./dir/file" "content"
        readFileTree "."
      fileTree `shouldBe` (Directory "dir" [File "file" "content"])

    it "reads multiple files in lexicographical order." $ do
      fileTree <- withTempCurrentDirectory $ do
        createDirectory "dir"
        ST.writeFile "./dir/a" "a"
        ST.writeFile "./dir/c" "c"
        ST.writeFile "./dir/b" "b"
        ST.writeFile "./dir/2" "2"
        ST.writeFile "./dir/10" "10"
        readFileTree =<< getCurrentDirectory
      fileTree `shouldBe` (Directory "dir" ((\s -> File (cs s) s) <$> ["10", "2", "a", "b", "c"]))

  describe "writeFileTree" $ do
    it "is inverse of readFileTree." . property $ \ft ->
      withTempCurrentDirectory (writeFileTree "." ft >> readFileTree ".") `shouldReturn` ft
