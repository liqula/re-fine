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
import           Data.String.Conversions (ST)
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Text.HTML.Parser

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Enhance
import Refine.Common.VDoc.HTML.Splice
import Refine.Common.VDoc.HTML.SpliceSpec (shouldBeLikeVDocVersion)



openTagWithUID :: Token
openTagWithUID = TagOpen "tag" [Attr "data-uid" "77"]

openTagWithOtherUID :: Token
openTagWithOtherUID = TagOpen "tag" [Attr "data-uid" "13"]

openTagWithoutUID :: Token
openTagWithoutUID = openMarkTag

openMarkTag :: Token
openMarkTag = TagOpen "mark" []

spec :: Spec
spec = parallel $ do
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


    describe "addDataUidsToTree" $ do
      it "adds the passed uid when there is none" $ do
        addDataUidsToTree (Just "1") (Node openTagWithoutUID []) `shouldBe` Node (TagOpen "mark" [Attr "data-uid" "1"]) []

      it "ignores the passed uid when there is already one" $ do
        addDataUidsToTree (Just "1") (Node openTagWithUID []) `shouldBe` Node openTagWithUID []

      it "does not alter other tokens" $ do
        addDataUidsToTree (Just "1") (Node (TagClose "tag")     []) `shouldBe` Node (TagClose "tag") []
        addDataUidsToTree (Just "1") (Node (ContentText "text") []) `shouldBe` Node (ContentText "text") []
        addDataUidsToTree (Just "1") (Node (ContentChar 'x')    []) `shouldBe` Node (ContentChar 'x') []
        addDataUidsToTree (Just "1") (Node (Comment "secret")   []) `shouldBe` Node (Comment "secret") []
        addDataUidsToTree (Just "1") (Node (Doctype "type")     []) `shouldBe` Node (Doctype "type") []

      it "passes the present uid to all children that do not already have one" $ do
        addDataUidsToTree (Just "1") (Node openTagWithUID [ Node openTagWithoutUID []
                                                          , Node openTagWithOtherUID []])
          `shouldBe` Node openTagWithUID [ Node (TagOpen "mark" [Attr "data-uid" "77"]) []
                                         , Node openTagWithOtherUID []]

      it "passes the passed uid to all children that do not already have one if none is present" $ do
        addDataUidsToTree (Just "1") (Node openTagWithoutUID [ Node openTagWithoutUID []
                                                             , Node openTagWithOtherUID []])
          `shouldBe` Node (TagOpen "mark" [Attr "data-uid" "1"]) [ Node (TagOpen "mark" [Attr "data-uid" "1"]) []
                                                                 , Node openTagWithOtherUID []]

    describe "addOffsetsToTree" $ do
      it "sets an offset of 0 if we are not on a mark tag" $ do
        -- the second case is impossible if input is canonicalized.
        addOffsetsToTree 50 (Node openTagWithUID [])    `shouldBe` (Node (TagOpen "tag" [Attr "data-offset" "0", Attr "data-uid" "77"]) [], 0)
        addOffsetsToTree 50 (Node openTagWithoutUID []) `shouldBe` (Node (TagOpen "mark" [Attr "data-offset" "50"]) [], 50)

      it "sets the passed offset if we are on a mark tag" $ do
        addOffsetsToTree 50 (Node openMarkTag []) `shouldBe` (Node (TagOpen "mark" [Attr "data-offset" "50"]) [], 50)

      it "adds up the offset of subsequent texts" $ do
        -- offset is relative to non-mark nodes, because those are the ones that have no data-uid
        -- (that is not inherited via 'addDataUidsToTree').  therefore, sibling marks have
        -- accumulating offset values because all selections into their children have to refer to
        -- the first data-uid-carrying ancestor with their offsets.
        addOffsetsToForest_ 50 [ Node (ContentText "a text") []
                               , Node (TagOpen "mark" []) []
                               , Node (ContentText "another text") []
                               , Node (TagOpen "mark" []) []] `shouldBe`
          ([ Node (ContentText "a text") []
           , Node (TagOpen "mark" [Attr "data-offset" "56"]) []
           , Node (ContentText "another text") []
           , Node (TagOpen "mark" [Attr "data-offset" "68"]) []
           ], 68)

      it "resets the addition when encountering a non-mark tag" $ do
        addOffsetsToForest_ 50
          [ Node (ContentText "a text") []
          , Node (TagOpen "mark" []) []
          , Node (ContentText "another text") []
          , Node (TagOpen "div" []) []
          , Node (ContentText "more text") []
          , Node (TagOpen "mark" []) []
          ] `shouldBe`
          ([ Node (ContentText "a text") []
           , Node (TagOpen "mark" [Attr "data-offset" "56"]) []
           , Node (ContentText "another text") []
           , Node (TagOpen "div" [Attr "data-offset" "0"]) []
           , Node (ContentText "more text") []
           , Node (TagOpen "mark" [Attr "data-offset" "9"]) []
           ], 9)

      it "carries over the offset from the children to the parent and into the next tree of the forest" $ do
        addOffsetsToForest_ 50
          [ Node (TagOpen "div" []) [ Node (ContentText "text a") []
                                    , Node (TagOpen "mark" []) []
                                    , Node (ContentText "marked text") []
                                    , Node (TagClose "mark") []
                                    , Node (ContentText "unmarked text") []]
          , Node (ContentText "some text") []
          , Node (TagOpen "mark" []) [ Node (ContentText "some text in mark") []
                                     , Node (TagOpen "mark" []) []
                                     , Node (ContentText "more text in mark") []
                                     , Node (TagClose "mark") []
                                     ]
          , Node (ContentText "another text") []
          , Node (TagOpen "mark" []) []
          ] `shouldBe`
          ([ Node (TagOpen "div" [Attr "data-offset" "0"])
                                    [ Node (ContentText "text a") [] -- length 6
                                    , Node (TagOpen "mark" [Attr "data-offset" "6"]) []
                                    , Node (ContentText "marked text") [] -- length 11
                                    , Node (TagClose "mark") []
                                    , Node (ContentText "unmarked text") []] -- length 13
          , Node (ContentText "some text") [] -- length 9
          , Node (TagOpen "mark" [Attr "data-offset" "39"])
                                      [ Node (ContentText "some text in mark") [] -- length 17
                                      , Node (TagOpen "mark" [Attr "data-offset" "56"]) []
                                      , Node (ContentText "more text in mark") [] -- length 17
                                      , Node (TagClose "mark") []
                                      ]
          , Node (ContentText "another text") [] -- length 12
          , Node (TagOpen "mark" [Attr "data-offset" "85"]) []
          ], 85)
