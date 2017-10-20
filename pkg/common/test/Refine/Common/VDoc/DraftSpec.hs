{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Refine.Common.VDoc.DraftSpec where
#include "import_common.hs"

import Control.Exception.Base (evaluate)
import Test.Aeson.GenericSpecs
import Test.Hspec
import Test.QuickCheck

import Refine.Common.OT
import Refine.Common.Test.Arbitrary ()
import Refine.Common.Test.Samples
import Refine.Common.Types
import Refine.Common.VDoc.Draft
import Refine.Common.VDoc.OT


spec :: Spec
spec = do
  describe "RawContent" $ do
    roundtripSpecs (Proxy @RawContent)

    it "rawContentFromCompositeVDoc regression test" $ do
      let errmsg = "toStylePosition: position outside content: " <> show (rc, pos)
          rc = RawContent (Block' { _blockText' = "wef"
                                  , _blockEntityRanges' = mempty
                                  , _blockStyles' = mempty
                                  , _blockType' = NormalText
                                  , _blockDepth' = 0
                                  , _blockKey' = BlockKey {_unBlockKey = "b0"}
                                  } :| [])
                          mempty
          pos = Position { _blockIndex = BlockIndex { _blockIndexIndex = 2 :: Int
                                                    , _blockIndexKey = BlockKey {_unBlockKey = "n3ur"}}
                         , _columnIndex = 0 :: Int
                         }
          thrower = show . length . show $ rawContentFromCompositeVDoc compositeVDocRegression1

      evaluate thrower `catch` (\(ErrorCall errmsg') -> pure errmsg')
        `shouldReturn` errmsg

  describe "rangeText" $ do
    it "works" $ do
      let rc = rawContentFromST "sdf\nwef"
          pos :: Int -> Int -> Position
          pos row = Position (BlockIndex row bkey)
            where
              Just bkey = bkeys `atMay` row
              bkeys = NEL.toList $ (^. blockKey) <$> (rc ^. rawContentBlocks)

       in do
        -- putStrLn `mapM_` ["        rangeText BlockBoundaryIsNewline rc " <>
        --                  "(Range (pos " <> show y1 <> " " <> show x1 <> ") (pos " <> show y2 <> " " <> show x2 <>")) " <>
        --                  "`shouldBe` \"\""
        --                  | y1 <- [0..1], x1 <- [0..3], y2 <- [0..1], x2 <- [0..3], y1 <= y2 && (x1 <= x2 || y1 < y2)]

        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 0 0)) `shouldBe` ""
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 0 1)) `shouldBe` "s"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 0 2)) `shouldBe` "sd"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 0 3)) `shouldBe` "sdf"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 1 0)) `shouldBe` "sdf\n"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 1 1)) `shouldBe` "sdf\nw"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 1 2)) `shouldBe` "sdf\nwe"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 0) (pos 1 3)) `shouldBe` "sdf\nwef"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 1) (pos 0 1)) `shouldBe` ""
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 1) (pos 0 2)) `shouldBe` "d"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 1) (pos 0 3)) `shouldBe` "df"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 1) (pos 1 0)) `shouldBe` "df\n"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 1) (pos 1 1)) `shouldBe` "df\nw"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 1) (pos 1 2)) `shouldBe` "df\nwe"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 1) (pos 1 3)) `shouldBe` "df\nwef"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 2) (pos 0 2)) `shouldBe` ""
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 2) (pos 0 3)) `shouldBe` "f"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 2) (pos 1 0)) `shouldBe` "f\n"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 2) (pos 1 1)) `shouldBe` "f\nw"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 2) (pos 1 2)) `shouldBe` "f\nwe"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 2) (pos 1 3)) `shouldBe` "f\nwef"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 3) (pos 0 3)) `shouldBe` ""
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 3) (pos 1 0)) `shouldBe` "\n"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 3) (pos 1 1)) `shouldBe` "\nw"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 3) (pos 1 2)) `shouldBe` "\nwe"
        rangeText BlockBoundaryIsNewline rc (Range (pos 0 3) (pos 1 3)) `shouldBe` "\nwef"
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 0) (pos 1 0)) `shouldBe` ""
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 0) (pos 1 1)) `shouldBe` "w"
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 0) (pos 1 2)) `shouldBe` "we"
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 0) (pos 1 3)) `shouldBe` "wef"
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 1) (pos 1 1)) `shouldBe` ""
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 1) (pos 1 2)) `shouldBe` "e"
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 1) (pos 1 3)) `shouldBe` "ef"
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 2) (pos 1 2)) `shouldBe` ""
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 2) (pos 1 3)) `shouldBe` "f"
        rangeText BlockBoundaryIsNewline rc (Range (pos 1 3) (pos 1 3)) `shouldBe` ""

  describe "showEditAsRawContentWithMarks" $ do
    let rc0   = rawContentFromST "sdxyf\nwef"
        rc1 r = rc0 & rawContentBlocks %~ (\(h :| t) -> (h & blockStyles' %~ Set.insert r) :| t)

        check_ r = do
          pendingWith "#461"
          let Right d = rc0 `diff` rc1 (r, Bold)
              rc' = showEditAsRawContentWithMarks d rc0
          NEL.head (rc' ^.rawContentBlocks) ^. blockStyles'
            `shouldBe` Set.fromList [(r, StyleAdded)]

    it "Changing style on 'dxy' shows the location of the edit at 'dxy'." $ check_ (RangeInner 2 4)
    it "Changing style on 'dxyf' shows the location of the edit at 'dxyf'." $ check_ (RangeInner 2 5)

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
compositeVDocRegression1 = CompositeVDoc {_compositeVDoc = VDoc {_vdocMetaID = MetaID {_miID = ID 1
  , _miMeta = MetaInfo {_metaCreatedBy = Anonymous
  , _metaCreatedAt = sampleTime
  , _metaChangedBy = Anonymous
  , _metaChangedAt = sampleTime}}
  , _vdocTitle = Title {_unTitle = "[no title]"}
  , _vdocAbstract = Abstract {_unAbstract = "[no abstract]"}
  , _vdocHeadEdit = ID 2
  , _vdocGroup = ID 2
  , _vdocStats = EditStats {_editStatsUsers = 1
  , _editStatsEdits = 1
  , _editStatsComments = 0}
  , _vdocImage = Nothing}
  , _compositeVDocThisEdit = Edit {_editMetaID = MetaID {_miID = ID 2
  , _miMeta = MetaInfo {_metaCreatedBy = Anonymous
  , _metaCreatedAt = sampleTime
  , _metaChangedBy = Anonymous
  , _metaChangedAt = sampleTime}}
  , _editDesc = "initial content"
  , _editKind = Initial
  , _editSource = EditSource {_unEditSource = [([ERawContent {unERawContent = [ENonEmpty {unENonEmpty = EditItem 0 [EditSecond (SegmentListEdit (InsertItem 0 ((Atom {unAtom = Nothing}
  ,Set.fromList [])
  ,NonEmptyST {unNonEmptyST = "wef"})))]}]}]
  ,ID 1)]}
  , _editVDoc = ID 1
  , _editVDocVersion = RawContent {_rawContentBlocks = Block' {_blockText' = "wef"
  , _blockEntityRanges' = Set.fromList []
  , _blockStyles' = Set.fromList []
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "b0"}} :| []
  , _rawContentEntityMap = mempty}
  , _editVotes = Map.fromList []
  , _editChildren = Set.fromList [ID 3]
  , _editDiscussions' = Map.fromList []}
  , _compositeVDocApplicableEdits = Map.fromList [(ID 3
  ,Edit {_editMetaID = MetaID {_miID = ID 3
  , _miMeta = MetaInfo {_metaCreatedBy = UserID (ID 1)
  , _metaCreatedAt = sampleTime
  , _metaChangedBy = UserID (ID 1)
  , _metaChangedAt = sampleTime}}
  , _editDesc = "initial contentwef"
  , _editKind = Phrasing
  , _editSource = EditSource {_unEditSource = [([ERawContent {unERawContent = [ENonEmpty {unENonEmpty = InsertItem 1 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "3u6kf"}})
  ,Segments [])}
  ,ENonEmpty {unENonEmpty = InsertItem 2 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "n3ur"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "wef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 3 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "amdte"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "wef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 4 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "1q4l1"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "wef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 5 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "1trhn"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "wef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 6 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "4prv8"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "we"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 7 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "ad01o"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "fw"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 8 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "cbg14"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "ef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 9 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "di0ni"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "wef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 10 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "4lqfo"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "wefw"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 11 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "7sh3p"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "ef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 12 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "alcoj"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "we"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 13 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "8hut6"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "fw"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 14 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "4o3n9"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "ef"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 15 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "a0of2"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "we"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 16 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "sbgd"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "g23g2"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 17 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "cbnu6"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "3g"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 18 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "1pob1"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "23"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 19 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "4eic4"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "g23"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 20 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "6tl7v"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "g2"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 21 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "ajm66"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "3g"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 22 (((Atom {unAtom = NormalText}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "7ah81"}})
  ,Segments [((Atom {unAtom = Nothing}
  ,mempty)
  ,NonEmptyST {unNonEmptyST = "2g3"})])}
  ,ENonEmpty {unENonEmpty = InsertItem 23 (((Atom {unAtom = EnumPoint}
  ,Atom {unAtom = BlockDepth {unBlockDepth = 0}})
  ,NonEditable {unNonEditable = BlockKey {_unBlockKey = "7hr6h"}})
  ,Segments [])}]}]
  ,ID 2)]}
  , _editVDoc = ID 1
  , _editVDocVersion = RawContent {_rawContentBlocks = Block' {_blockText' = "wef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "b0"}} :| [Block' {_blockText' = ""
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "3u6kf"}}
  ,Block' {_blockText' = "wef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "n3ur"}}
  ,Block' {_blockText' = "wef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "amdte"}}
  ,Block' {_blockText' = "wef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "1q4l1"}}
  ,Block' {_blockText' = "wef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "1trhn"}}
  ,Block' {_blockText' = "we"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "4prv8"}}
  ,Block' {_blockText' = "fw"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "ad01o"}}
  ,Block' {_blockText' = "ef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "cbg14"}}
  ,Block' {_blockText' = "wef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "di0ni"}}
  ,Block' {_blockText' = "wefw"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "4lqfo"}}
  ,Block' {_blockText' = "ef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "7sh3p"}}
  ,Block' {_blockText' = "we"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "alcoj"}}
  ,Block' {_blockText' = "fw"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "8hut6"}}
  ,Block' {_blockText' = "ef"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "4o3n9"}}
  ,Block' {_blockText' = "we"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "a0of2"}}
  ,Block' {_blockText' = "g23g2"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "sbgd"}}
  ,Block' {_blockText' = "3g"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "cbnu6"}}
  ,Block' {_blockText' = "23"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "1pob1"}}
  ,Block' {_blockText' = "g23"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "4eic4"}}
  ,Block' {_blockText' = "g2"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "6tl7v"}}
  ,Block' {_blockText' = "3g"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "ajm66"}}
  ,Block' {_blockText' = "2g3"
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = NormalText
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "7ah81"}}
  ,Block' {_blockText' = ""
  , _blockEntityRanges' = mempty
  , _blockStyles' = mempty
  , _blockType' = EnumPoint
  , _blockDepth' = 0
  , _blockKey' = BlockKey {_unBlockKey = "7hr6h"}}]
  , _rawContentEntityMap = mempty}
  , _editVotes = mempty
  , _editChildren = mempty
  , _editDiscussions' = mempty})]
  , _compositeVDocApplicableDiscussions = mempty}
