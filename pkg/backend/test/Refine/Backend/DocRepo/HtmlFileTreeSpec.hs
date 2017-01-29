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

import Control.Exception (evaluate)
import Data.Function (on)
import Data.List (nubBy, sort)
import Data.String.Conversions
import Data.Text as ST
import Data.Text.IO as ST
import System.Directory
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Text.HTML.Parser (Token(..), Attr(..), parseTokens, renderTokens)
import Text.HTML.Tree (nonClosing)

import Refine.Backend.DocRepo.HtmlFileTree
import Refine.Backend.Test.Util (withTempCurrentDirectory)
import Refine.Common.Test.Arbitrary ()
import Refine.Common.Types
import Refine.Common.VDoc.HTML


instance Arbitrary FileTree where
  arbitrary = oneof
    [ File <$> arbitraryFileName <*> arbitrary
    , Directory <$> arbitraryFileName <*> (nubBy ((==) `on` _fileName) . sort <$> scale (`div` 2) arbitrary)
    ]

  shrink (File fp c) = File fp <$> shrink c
  shrink (Directory dp c) = Directory dp <$> shrink c

arbitraryFileName :: Gen FilePath
arbitraryFileName = elements $ show <$> [(1 :: Int)..10]

-- | FIXME: move this to a better place
--
-- If a tag that *can* be closed normally is closed with the short-hand, change that.  In some
-- functions this happens implicitly, so in order to compare the input of those function against the
-- output, we have to fake it.  Look at the places where we call this and remove the call for a
-- better understanding.
normalizeSelfClosingTags :: VDocVersion a -> VDocVersion a
normalizeSelfClosingTags = VDocVersion . cs . renderTokens . smoothen . parseTokens . cs . _unVDocVersion
  where
    smoothen [] = []
    smoothen (TagSelfClose n attrs : ts) = if n `elem` nonClosing
      then TagOpen n attrs : smoothen ts
      else TagOpen n attrs : TagClose n : smoothen ts
    smoothen (t : ts) = t : smoothen ts


spec :: Spec
spec = do
  describe "htmlToFileTree" $ do
    it "creates files for text nodes." $ do
      let vers = VDocVersion "phoo"
      htmlToFileForest vers
        `shouldBe` [File "SIBLINGORDER" "s1\n", File "s1" "phoo"]

    it "throws an exception if any tag has no @data-uid@ attribute." $ do
      pendingWith "#16"
      let vers = VDocVersion "<div></div>"
      evaluate (Prelude.length . show $ htmlToFileForest vers)
        `shouldThrow` anyException

    it "creates directories for tags; stores tag names in file TAGNAME; attrs in ATTRS; uses data-uid as filename." $ do
      let vers = VDocVersion "<div data-uid=\"234\" something=\"other\"></div>"
      htmlToFileForest vers
        `shouldBe` [ File "SIBLINGORDER" "u234\n"
                   , Directory "u234" [ File "TAGNAME" "div"
                                      , File "ATTRS" $ showAttrs [Attr "something" "other", Attr "data-uid" "234"]
                                      ]
                   ]

    it "is inverse of htmlFromFileTree." . property $ \vers -> do
      htmlFromFileForest (htmlToFileForest vers) `shouldBe` normalizeSelfClosingTags vers

    it "slightly bigger example." $ do
      let Right vers = canonicalizeVDocVersion . VDocVersion . ST.unlines $
            [ "<div>"
            , "  <a href=\"/link1\">link1 text</a>"
            , "  <a href=\"/link2\">link2 text</a>"
            , "  <div>"
            , "    <br/>"
            , "  </div>"
            , "</div>"
            ]

          -- this has been stolen from hspec output and superficially sanity-checked during writing
          -- of this test.  but even if it's still wrong, it gives us a good reference to discuss
          -- that.
          expected = [ File "SIBLINGORDER" "u1\nu6\n"
                     , Directory "u1"
                         [ File "TAGNAME" "div"
                         , File "ATTRS" $ showAttrs [Attr "data-uid" "1"]
                         , File "SIBLINGORDER" "s1\nu2\ns3\nu3\ns5\nu4\ns7\n"
                         , File "s1" "\n\n"
                         , Directory "u2"
                             [ File "TAGNAME" "a"
                             , File "ATTRS" $ showAttrs [Attr "data-uid" "2", Attr "href" "/link1"]
                             , File "SIBLINGORDER" "s1\n"
                             , File "s1" "link1\ntext"
                             ]
                         , File "s3" "\n\n"
                         , Directory "u3"
                             [ File "TAGNAME" "a"
                             , File "ATTRS" $ showAttrs [Attr "data-uid" "3", Attr "href" "/link2"]
                             , File "SIBLINGORDER" "s1\n"
                             , File "s1" "link2\ntext"
                             ]
                         , File "s5" "\n\n"
                         , Directory "u4"
                             [ File "TAGNAME" "div"
                             , File "ATTRS" $ showAttrs [Attr "data-uid" "4"]
                             , File "SIBLINGORDER" "s1\nu5\ns3\n"
                             , File "s1" "\n\n"
                             , Directory "u5"
                                 [ File "TAGNAME" "br"
                                 , File "ATTRS" $ showAttrs [Attr "data-uid" "5"]
                                 ]
                             , File "s3" "\n\n"
                             ]
                         , File "s7" "\n\n"
                         ]
                     , Directory "u6"
                         [ File "TAGNAME" "span"
                         , File "ATTRS" "[\n    [\n        \"data-uid\",\n        \"6\"\n    ]\n]"
                         , File "SIBLINGORDER" "s1\n"
                         , File "s1" "\n\n"
                         ]
                     ]

      htmlFromFileForest (htmlToFileForest vers) `shouldBe` normalizeSelfClosingTags vers
      htmlToFileForest vers `shouldBe` expected

  describe "writeHtml, readHtml" $ do
    it "are inverses." $ do
      -- (we've covered property checking of html to/from file tree above.  but we should run these
      -- two functions once anyway, just to make sure.)
      let Right vers = canonicalizeVDocVersion
                     $ VDocVersion "<div a=\"3\" bw=\"phoo\">whee<p>bla</p>furgh<span>3</span></div>"
      withTempCurrentDirectory (writeHtml "." vers >>= readHtml)
        `shouldReturn` vers

  describe "readFileTree" $ do
    it "reads top-level file correctly." $ do
      fileTree <- withTempCurrentDirectory $ do
        ST.writeFile "name" "contents"
        readFileTree "."
      fileTree `shouldBe` Directory "." [File "name" "contents"]

    it "reads directory correctly." $ do
      fileTree <- withTempCurrentDirectory $ do
        createDirectory "name"
        readFileTree "."
      fileTree `shouldBe` Directory "." [Directory "name" []]

    it "reads nested file correctly." $ do
      fileTree <- withTempCurrentDirectory $ do
        createDirectory "dir"
        ST.writeFile "./dir/file" "content"
        readFileTree "."
      fileTree `shouldBe` Directory "." [Directory "dir" [File "file" "content"]]

    it "reads multiple files in lexicographical order." $ do
      fileTree <- withTempCurrentDirectory $ do
        createDirectory "dir"
        ST.writeFile "./dir/a" "a"
        ST.writeFile "./dir/c" "c"
        ST.writeFile "./dir/b" "b"
        ST.writeFile "./dir/2" "2"
        ST.writeFile "./dir/10" "10"
        readFileTree "."
      fileTree `shouldBe` Directory "." [Directory "dir" . sort $ ((\s -> File (cs s) s) <$> ["10", "2", "a", "b", "c"])]

  describe "writeFileTree" $ do
    it "is inverse of readFileTree." . property $ \ft ->
      withTempCurrentDirectory (writeFileTree "." ft >> readFileTree ".") `shouldReturn` Directory "." [ft]
