{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Refine.Common.VDoc.DraftSpec where
#include "import_common.hs"

import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Aeson.GenericSpecs
import Test.Hspec
import Test.QuickCheck

import Refine.Common.Test.Arbitrary ()
import Refine.Common.Test.Samples ()  -- (just importing it so we know it compiles.)
import Refine.Common.Types
import Refine.Common.VDoc.Draft


spec :: Spec
spec = do
  describe "RawContent" $ do
    roundtripSpecs (Proxy @RawContent)

    it "rawContentFromCompositeVDoc regression test" $ do
      pendingWith "TODO"
      -- TODO: 'compositeVDocRegression1' may very well be inconsistent, but in that case we'd like
      -- a useful error message.  currently we get an unlocated "Prelude.!!: index too large.".
      (length . show $ rawContentFromCompositeVDoc compositeVDocRegression1) `shouldNotBe` 0

  describe "separateStyles, joinStyles" $ do
    it "joinStyles . separateStyles == id" . property $ \(rawContent :: RawContent) -> do
      joinStyles (separateStyles rawContent) `shouldBe` rawContent
    it "separateStyles . joinStyles . separateStyles == separateStyles" . property $ \(rawContent :: RawContent) -> do
      let ss = separateStyles rawContent
      separateStyles (joinStyles ss) `shouldBe` ss

  describe "mkSomeSegments" $ do
    it "works" $ do
      mkSomeSegments fst snd [(EntityRange 1 3, 'o'), (EntityRange 2 4, 'x')]
        `shouldBe` [(1, Set.empty), (1, Set.fromList "o"), (2, Set.fromList "ox"), (2, Set.fromList "x")]

  describe "getLeafSelectors" $ do
    let cid0 = MarkContribution (ContribIDDiscussion True (ID 13)) 0
        cid1 = MarkContribution (ContribIDDiscussion True (ID 35)) 0
        block0 = BlockIndex 0 $ BlockKey "b0"
        block1 = BlockIndex 1 $ BlockKey "b1"

    it "works (no contribs)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = []
          want       = Map.fromList []
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning the entire block)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [(cid0, Range (Position block0 0) (Position block0 4))]
          want       = Map.fromList [(cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 0)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 0))])]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning part of the block)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [(cid0, Range (Position block0 2) (Position block0 4))]
          want       = Map.fromList [(cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 1))])]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (single contrib spanning the entire block, with an extra block style flying around)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890" & blockStyles .~ [(EntityRange 1 2, Bold)]]
          marks      = [(cid0, Range (Position block0 0) (Position block0 4))]
          want       = Map.fromList [(cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 0)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 2))])]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (one contrib in two parts)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 3))
                       , (cid0, Range (Position block0 4) (Position block0 7))
                       ]
          want       = Map.fromList [(cid0, RangesInner
                       [ RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 1))
                       , RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 3)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 3))
                       ])]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (one contrib in two parts spanning two blocks)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890", mkBlock "asdf"]
          marks      = [ (cid0, Range (Position block0 2) (Position block1 1))
                       , (cid0, Range (Position block1 2) (Position block1 3))
                       ]
          want       = Map.fromList [(cid0, RangesInner
                       [ RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block1 ^. blockIndexKey) (SpanIndex 0 0))
                       , RangeInner (Position (block1 ^. blockIndexKey) (SpanIndex 0 2)) (Position (block1 ^. blockIndexKey) (SpanIndex 0 2))
                       ])]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 4))
                       , (cid1, Range (Position block0 3) (Position block0 7))
                       ]
          want       = Map.fromList
                       [ (cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 2))])
                       , (cid1, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 2)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 3))])
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs spanning two blocks)" $ do
      let rawContent = mkRawContent $ NEL.fromList [mkBlock "1234567890", mkBlock "asdf"]
          marks      = [ (cid0, Range (Position block0 2) (Position block1 3))
                       , (cid1, Range (Position block1 1) (Position block1 2))
                       ]
          want       = Map.fromList
                       [ (cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block1 ^. blockIndexKey) (SpanIndex 0 2))])
                       , (cid1, RangesInner [RangeInner (Position (block1 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block1 ^. blockIndexKey) (SpanIndex 0 1))])
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs beginning in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 3))
                       , (cid1, Range (Position block0 2) (Position block0 4))
                       ]
          want       = Map.fromList
                       [ (cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 1))])
                       , (cid1, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 2))])
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs ending in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 3) (Position block0 4))
                       , (cid1, Range (Position block0 2) (Position block0 4))
                       ]
          want       = Map.fromList
                       [ (cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 2)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 2))])
                       , (cid1, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 2))])
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (two overlapping contribs beginning and ending in the same point)" $ do
      let rawContent = mkRawContent $ mkBlock "1234567890" :| []
          marks      = [ (cid0, Range (Position block0 2) (Position block0 4))
                       , (cid1, Range (Position block0 2) (Position block0 4))
                       ]
          want       = Map.fromList
                       [ (cid0, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 1))])
                       , (cid1, RangesInner [RangeInner (Position (block0 ^. blockIndexKey) (SpanIndex 0 1)) (Position (block0 ^. blockIndexKey) (SpanIndex 0 1))])
                       ]
      getLeafSelectors (addMarksToRawContent marks rawContent) `shouldBe` want

    it "works (with entity)" $ do
      pendingWith "do they also have spans?  can we somehow distinguish them away in the css selector?"


-- * data for regression tests

compositeVDocRegression1 :: CompositeVDoc
Just [compositeVDocRegression1] = decode "[{\"_compositeVDoc\":{\"_vdocHeadEdit\":2,\"_vdocGroup\":2,\"_vdocImage\":null,\"_vdocMetaID\":{\"_miID\":1,\"_miMeta\":{\"_metaCreatedAt\":\"2017-09-28T16:51:36.968455142Z\",\"_metaChangedAt\":\"2017-09-28T16:51:41.503050894Z\",\"_metaChangedBy\":\"Anonymous\",\"_metaCreatedBy\":\"Anonymous\"}},\"_vdocTitle\":\"[no title]\",\"_vdocStats\":{\"_editStatsComments\":0,\"_editStatsEdits\":1,\"_editStatsUsers\":1},\"_vdocAbstract\":\"[no abstract]\"},\"_compositeVDocThisEdit\":{\"_editDiscussions'\":[],\"_editVDocVersion\":{\"entityMap\":{},\"blocks\":[{\"key\":\"b0\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wef\"}]},\"_editVDoc\":1,\"_editChildren\":[3],\"_editKind\":\"Initial\",\"_editMetaID\":{\"_miID\":2,\"_miMeta\":{\"_metaCreatedAt\":\"2017-09-28T16:51:41.486756961Z\",\"_metaChangedAt\":\"2017-09-28T16:51:41.486756961Z\",\"_metaChangedBy\":\"Anonymous\",\"_metaCreatedBy\":\"Anonymous\"}},\"_editSource\":[[[{\"unERawContent\":[{\"unENonEmpty\":{\"tag\":\"EditItem\",\"contents\":[0,[{\"tag\":\"EditSecond\",\"contents\":{\"tag\":\"SegmentListEdit\",\"contents\":{\"tag\":\"InsertItem\",\"contents\":[0,[[null,[]],\"wef\"]]}}}]]}}]}],1]],\"_editDesc\":\"initial content\",\"_editVotes\":[]},\"_compositeVDocApplicableEdits\":[[3,{\"_editDiscussions'\":[],\"_editVDocVersion\":{\"entityMap\":{},\"blocks\":[{\"key\":\"b0\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wef\"},{\"key\":\"3u6kf\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"\"},{\"key\":\"n3ur\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wef\"},{\"key\":\"amdte\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wef\"},{\"key\":\"1q4l1\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wef\"},{\"key\":\"1trhn\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wef\"},{\"key\":\"4prv8\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"we\"},{\"key\":\"ad01o\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"fw\"},{\"key\":\"cbg14\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"ef\"},{\"key\":\"di0ni\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wef\"},{\"key\":\"4lqfo\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wefw\"},{\"key\":\"7sh3p\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"ef\"},{\"key\":\"alcoj\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"we\"},{\"key\":\"8hut6\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"fw\"},{\"key\":\"4o3n9\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"ef\"},{\"key\":\"a0of2\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"we\"},{\"key\":\"sbgd\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"g23g2\"},{\"key\":\"cbnu6\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"3g\"},{\"key\":\"1pob1\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"23\"},{\"key\":\"4eic4\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"g23\"},{\"key\":\"6tl7v\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"g2\"},{\"key\":\"ajm66\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"3g\"},{\"key\":\"7ah81\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"2g3\"},{\"key\":\"7hr6h\",\"depth\":0,\"type\":\"ordered-list-item\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"\"}]},\"_editVDoc\":1,\"_editChildren\":[],\"_editKind\":\"Phrasing\",\"_editMetaID\":{\"_miID\":3,\"_miMeta\":{\"_metaCreatedAt\":\"2017-09-28T17:58:37.400026723Z\",\"_metaChangedAt\":\"2017-09-28T17:58:37.400026723Z\",\"_metaChangedBy\":{\"UserID\":1},\"_metaCreatedBy\":{\"UserID\":1}}},\"_editSource\":[[[{\"unERawContent\":[{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[1,[[[\"unstyled\",0],\"3u6kf\"],[]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[2,[[[\"unstyled\",0],\"n3ur\"],[[[null,[]],\"wef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[3,[[[\"unstyled\",0],\"amdte\"],[[[null,[]],\"wef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[4,[[[\"unstyled\",0],\"1q4l1\"],[[[null,[]],\"wef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[5,[[[\"unstyled\",0],\"1trhn\"],[[[null,[]],\"wef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[6,[[[\"unstyled\",0],\"4prv8\"],[[[null,[]],\"we\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[7,[[[\"unstyled\",0],\"ad01o\"],[[[null,[]],\"fw\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[8,[[[\"unstyled\",0],\"cbg14\"],[[[null,[]],\"ef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[9,[[[\"unstyled\",0],\"di0ni\"],[[[null,[]],\"wef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[10,[[[\"unstyled\",0],\"4lqfo\"],[[[null,[]],\"wefw\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[11,[[[\"unstyled\",0],\"7sh3p\"],[[[null,[]],\"ef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[12,[[[\"unstyled\",0],\"alcoj\"],[[[null,[]],\"we\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[13,[[[\"unstyled\",0],\"8hut6\"],[[[null,[]],\"fw\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[14,[[[\"unstyled\",0],\"4o3n9\"],[[[null,[]],\"ef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[15,[[[\"unstyled\",0],\"a0of2\"],[[[null,[]],\"we\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[16,[[[\"unstyled\",0],\"sbgd\"],[[[null,[]],\"g23g2\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[17,[[[\"unstyled\",0],\"cbnu6\"],[[[null,[]],\"3g\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[18,[[[\"unstyled\",0],\"1pob1\"],[[[null,[]],\"23\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[19,[[[\"unstyled\",0],\"4eic4\"],[[[null,[]],\"g23\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[20,[[[\"unstyled\",0],\"6tl7v\"],[[[null,[]],\"g2\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[21,[[[\"unstyled\",0],\"ajm66\"],[[[null,[]],\"3g\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[22,[[[\"unstyled\",0],\"7ah81\"],[[[null,[]],\"2g3\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[23,[[[\"ordered-list-item\",0],\"7hr6h\"],[]]]}}]}],2]],\"_editDesc\":\"initial contentwef\",\"_editVotes\":[]}]],\"_compositeVDocApplicableDiscussions\":[]}]"
