{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.HTML.EnhanceSpec where

import           Data.List (foldl')
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Renderer.Utf8 as Blaze (renderMarkup)
import           Text.HTML.Parser
import qualified Text.HTML.QQ as QQ
import           Text.HTML.Tree

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Canonicalize (canonicalizeAttrsForest)
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Enhance
import Refine.Common.VDoc.HTML.Splice
import Refine.Common.VDoc.HTML.SpliceSpec (shouldBeLikeVDocVersion)

openTagWithUID :: Token
openTagWithUID = TagOpen "tag" [Attr "data-uid" "77"]

openTagWithOtherUID :: Token
openTagWithOtherUID = TagOpen "tag" [Attr "data-uid" "13"]

openTagWithoutUID :: Token
openTagWithoutUID = TagOpen "tag" []

openMarkTag :: Token
openMarkTag = TagOpen "mark" []


toTrimmedTokenForest :: QQ.Document -> Forest Token
toTrimmedTokenForest
    = (\(Right v) -> v) . tokensToForest
    . canonicalizeTokens
    . filter notDocType
    . fmap trim
    . parseTokens . cs
    . Blaze.renderMarkup
    . Blaze.toMarkup
  where
    trim (ContentText s) = ContentText (ST.strip s)
    trim t = t

    notDocType (Doctype _) = False
    notDocType _ = True

stripAttrForest :: ST -> Forest Token -> Forest Token
stripAttrForest k = fmap (stripAttrTree k)

stripAttrTree :: ST -> Tree Token -> Tree Token
stripAttrTree k (Node tok children) = Node (stripAttrToken k tok) (stripAttrForest k children)

stripAttrToken :: ST -> Token -> Token
stripAttrToken k (TagOpen n attrs) = TagOpen n (stripAttr k attrs)
stripAttrToken k (TagSelfClose n attrs) = TagSelfClose n (stripAttr k attrs)
stripAttrToken _ t = t

stripAttr :: ST -> [Attr] -> [Attr]
stripAttr k (a@(Attr k' _) : as) = if k == k' then as else a : stripAttr k as
stripAttr _ [] = []


testAddOffsetsToForest :: QQ.Document -> Expectation
testAddOffsetsToForest doc =
  let forest = toTrimmedTokenForest doc
  in canonicalizeAttrsForest (addOffsetsToForest (stripAttrForest "data-offset" forest)) `shouldBe` canonicalizeAttrsForest forest


spec :: Spec
spec = parallel $ do
    describe "### [test qq -- should be moved further down in this module]" $ do
      it "works" $ do
        let forest = toTrimmedTokenForest
              [QQ.html|<span>
                         ab c
                       </span>
                      |]
        forest `shouldBe` [Node (TagOpen "span" []) [Node (ContentText "ab c") []]]

      it "also works" $ do
        let forest = toTrimmedTokenForest [QQ.html|
              <div data-uid="1">
                1234
                <mark data-uid="1" data-offset="4">
                  asd
                  <mark data-uid="1" data-offset="7">
                    q
                  </mark>
                  wf
                  <mark data-uid="1" data-offset="10">
                    987
                  </mark>
                </mark>
              </div>
              |]

        stripAttrForest "data-offset" forest `shouldNotBe` forest

      it "also works" $ do
        testAddOffsetsToForest [QQ.html|
              <div data-uid="1" data-offset="0">
                1234
                <mark data-uid="1" data-offset="4">
                  asd
                </mark>
              </div>
              |]


    describe "addUIInfoToVDocVersion" $ do
      it "is idempotent" . property $ do
        \(insertMarks ([] :: [ChunkRange Note]) -> vers) ->
          addUIInfoToVDocVersion vers `shouldBe` addUIInfoToVDocVersion (addUIInfoToVDocVersion vers)

      let interleaveProp :: VersWithRanges -> Expectation
          interleaveProp (VersWithRanges vers rs) = do
              runOnce `shouldBeLikeVDocVersion` runMany
            where
              runOnce = addUIInfoToVDocVersion $ insertMarks rs vers
              runMany = addUIInfoToVDocVersion $ foldl' go (insertMarks ([] :: [ChunkRange Note]) vers) rs
                where
                  go :: VDocVersion 'HTMLWithMarks -> ChunkRange Edit -> VDocVersion 'HTMLWithMarks
                  go v r = insertMoreMarks [r] $ addUIInfoToVDocVersion v

      it "can be interleaved with `insertMoreMarks`." . property $
        interleaveProp


      let hasAttrProp :: ST -> VersWithRanges -> Expectation
          hasAttrProp key (VersWithRanges vers rs) = do
              violators runOnce `shouldBe` []
              violators runMany `shouldBe` []
            where
              violators :: Forest Token -> [Token]
              violators = filter f . mconcat . fmap flatten
                where
                  hasKey (Attr key' _ : attrs') = key' == key || hasKey attrs'
                  hasKey [] = False

                  f (TagOpen _ attrs)  = not $ hasKey attrs
                  f (TagSelfClose _ _) = False  -- (doesn't need the data-uid, actually, so we don't care.)
                  f _                  = False

              VDocVersion runOnce = addUIInfoToVDocVersion $ insertMarks rs vers
              VDocVersion runMany = addUIInfoToVDocVersion $ foldl' go (insertMarks ([] :: [ChunkRange Note]) vers) rs
                where
                  go :: VDocVersion 'HTMLWithMarks -> ChunkRange Edit -> VDocVersion 'HTMLWithMarks
                  go v r = insertMoreMarks [r] $ addUIInfoToVDocVersion v

      it "leaves no tag without data-uid attribute." . property $
        hasAttrProp "data-uid"

      it "leaves no tag without data-offset attribute." . property $
        hasAttrProp "data-offset"


    describe "addOffsetsToForest" $ do
      it "### for every mark tag, sets offset to the length of the text bewteen it and first non-mark open tag." . property $ do
        \(VersWithRanges vers rs) -> do
          let forest = addOffsetsToForest . _unVDocVersion $ insertMarks rs vers

              check :: Int -> Forest Token -> [String]
              check off (tree@(Node tok@(TagOpen "mark" attrs) children) : siblings) =
                  check off children <>
                  checkMarkNode attrs <>
                  check (off + treeTextLength tree) siblings
                where
                  checkMarkNode (Attr "data-offset" off' : _) =
                    ["bad data-offset in mark token: " <> show (off, tok) | off' /= cs (show off)]
                  checkMarkNode (_ : attrs') =
                    checkMarkNode attrs'
                  checkMarkNode [] =
                    ["no data-offset in mark token: " <> show (off, tok)]

              check off (tree@(Node tok@(TagOpen _ attrs) children) : siblings) =  -- assumed to have data-uid.
                  check 0 children <>
                  checkNonMarkNode attrs <>
                  check (off + treeTextLength tree) siblings
                where
                  checkNonMarkNode (Attr "data-offset" off' : _) =
                    ["bad data-offset in non-mark token (should be 0): " <> show (off, tok) | off' /= "0"]
                  checkNonMarkNode (_ : attrs') =
                    checkNonMarkNode attrs'
                  checkNonMarkNode [] =
                    ["no data-offset in non-mark token: " <> show (off, tok)]

              check off (tree@(Node _ []) : siblings) =
                  check (off + treeTextLength tree) siblings

              check _ [] = []
              check badoff badforest = ["just bad: " <> show (badoff, badforest)]

          check 0 forest `shouldBe` []

      it "inserts the offset of a child mark node" $ do
        testAddOffsetsToForest [QQ.html|
                                  <p data-offset="0" data-uid="1">
                                    texta
                                    <mark data-offset="5" data-uid="1">
                                      textb
                                    </mark>
                                    <strong data-offset="0" data-uid="2">
                                        textc
                                    </strong>
                                  </p>
                               |]

      it "inserts the offset of multiple cascading mark nodes" $ do
        testAddOffsetsToForest [QQ.html|
                                <p data-uid="1" data-offset="0">
                                  a text
                                  <mark data-uid="1" data-offset="6">
                                    another text
                                    <mark data-uid="1" data-offset="18">
                                    </mark>
                                  </mark>
                                </p>
                               |]

      it "resets the addition when encountering a non-mark tag" $ do
        testAddOffsetsToForest [QQ.html|
                                <p data-uid="1" data-offset="0">
                                  a text
                                  <mark data-uid="1" data-offset="6">
                                    another text
                                    <div data-uid="2" data-offset="0">
                                      more text
                                      <mark data-uid="2" data-offset="9">
                                      </mark>
                                    </div>
                                  </mark>
                                </p>
                               |]

      it "carries over the offset from the children to the parent and into the next tree of the forest" $ do
        testAddOffsetsToForest [QQ.html|
                              <p data-uid="1" data-offset="0">
                                <div data-uid="2" data-offset="0">
                                  text a                    <!-- length  6 -->
                                  <mark data-uid="2" data-offset="6">
                                    marked text             <!-- length 11 -->
                                  </mark>
                                  unmarked text             <!-- length 13 -->
                                </div>
                                some text                   <!-- length 9 -->
                                <mark data-uid="1" data-offset="39">
                                  some text in mark         <!-- length 17 -->
                                  <mark data-uid="1" data-offset="56">
                                    more text in mark       <!-- length 17 -->
                                  </mark>
                                </mark>
                                another text                <!-- length 12 -->
                                <mark data-uid="1" data-offset="85">
                                </mark>
                               |]

      it "inserts the offset of a child mark node (and its mark children) that occur after some other node" $ do
        testAddOffsetsToForest [QQ.html|
                                  <p data-offset="0" data-uid="1">
                                    texta
                                    <strong data-offset="0" data-uid="2">
                                        textc
                                    </strong>
                                    <mark data-offset="10" data-uid="1">
                                      <mark data-offset="10" data-uid="1">
                                        texte
                                      </mark>
                                    </mark>
                                  </p>
                               |]

      it "inserts the offset of a mark node that occurs inside some non-mark node" $ do
        testAddOffsetsToForest [QQ.html|
                                  <p data-offset="0" data-uid="1">
                                    texta
                                    textb
                                    <strong data-offset="0" data-uid="2">
                                      te
                                      <mark data-offset="2" data-uid="2">
                                        xtc
                                      </mark>
                                    </strong>
                                  </p>
                               |]



    describe "addDataUidsToTree" $ do
      it "keeps the passed uid when there is already one" $ do
        addDataUidsToTree (Just "1") (Node openTagWithUID []) `shouldBe` Node openTagWithUID []

      it "does not alter other tokens" $ do
        addDataUidsToTree (Just "1") (Node (TagClose "tag")     []) `shouldBe` Node (TagClose "tag") []
        addDataUidsToTree (Just "1") (Node (ContentText "text") []) `shouldBe` Node (ContentText "text") []
        addDataUidsToTree (Just "1") (Node (ContentChar 'x')    []) `shouldBe` Node (ContentChar 'x') []
        addDataUidsToTree (Just "1") (Node (Comment "secret")   []) `shouldBe` Node (Comment "secret") []
        addDataUidsToTree (Just "1") (Node (Doctype "type")     []) `shouldBe` Node (Doctype "type") []

      it "passes the present uid to all children that do not already have one" $ do
        addDataUidsToTree (Just "1") (Node openTagWithUID [ Node openMarkTag []
                                                          , Node openTagWithOtherUID []])
          `shouldBe` Node openTagWithUID [ Node (TagOpen "mark" [Attr "data-uid" "77"]) []
                                         , Node openTagWithOtherUID []]

    describe "addOffsetsToTree" $ do
      it "sets an offset of 0 if we are not on a mark tag" $ do
        -- the second case is impossible if input is canonicalized.
        addOffsetsToTree 50 (Node openTagWithUID [])    `shouldBe` (Node (TagOpen "tag" [Attr "data-offset" "0", Attr "data-uid" "77"]) [], 0)
        addOffsetsToTree 50 (Node openTagWithoutUID []) `shouldBe` (Node (TagOpen "tag" [Attr "data-offset" "0"]) [], 0)

      it "sets the passed offset if we are on a mark tag" $ do
        addOffsetsToTree 50 (Node openMarkTag []) `shouldBe` (Node (TagOpen "mark" [Attr "data-offset" "50"]) [], 0)

      it "returns the length of the node" $ do
        addOffsetsToTree 50 (Node openMarkTag [Node (ContentText "some text") []]) `shouldBe` (Node (TagOpen "mark" [Attr "data-offset" "50"]) [Node (ContentText "some text") []], 9)
        addOffsetsToTree 50 (Node openTagWithoutUID [Node (ContentText "some text") []]) `shouldBe` (Node (TagOpen "tag" [Attr "data-offset" "0"]) [Node (ContentText "some text") []], 9)

-- TODO we need this test somewhere: Generated marks will only contain text and marks. They will never be wrapped around
-- other DOM elements. (i.e. they are only inserted at the leaves of the tree)
