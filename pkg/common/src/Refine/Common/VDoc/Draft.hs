{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.Draft
where

import Refine.Common.Prelude

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST

import Refine.Common.Types.Core
import Refine.Common.Types.Chunk
import Refine.Common.Types.Contribution
import Refine.Common.Types.Comment


-- * functions

emptyRawContent :: RawContent
emptyRawContent = mkRawContent $ emptyBlock :| []

-- | Two points worth nothing here:
--
-- (1) 'SelectionState' is always defined, even if nothing is selected.  If `window.getSelection()`
-- yields nothing, the selection state value in the editor state contains the empty selection (start
-- point == end point).
--
-- (2) Since blocks can be empty, empty selections can range over many lines.
selectionIsEmpty :: RawContent -> SelectionState -> Bool
selectionIsEmpty (RawContent bs _) ss@(SelectionState _ s e) = s == e || multiLineCase
  where
    multiLineCase = case selectedBlocks ss (NEL.toList bs) of
      []        -> True
      [_]       -> assert (s /= e) False
      (b : bs') -> and [ ST.length (b ^. blockText) == (s ^. selectionOffset)
                       , e ^. selectionOffset == 0
                       , all (ST.null . view blockText) (init bs')
                       ]

-- | alternative implementation (it would be interesting to benchmark both):
--
-- >>> takeWhile1 ((/= Just ek) . (^. blockKey)) . dropWhile ((/= Just sk) . (^. blockKey))
-- >>>
-- >>> takeWhileAndOneMore p [] = []
-- >>> takeWhileAndOneMore p (x : xs) = x : if p x then takeWhile1 p xs else []
selectedBlocks :: SelectionState -> [Block EntityKey BlockKey] -> [Block EntityKey BlockKey]
selectedBlocks (SelectionState _ (SelectionPoint sk _) (SelectionPoint ek _)) = f
  where
    f [] = []
    f (b:bs) = if b ^. blockKey == sk then b : g bs else f bs

    g [] = []
    g (b:bs) = if b ^. blockKey == ek then [b] else b : g bs

-- | Like 'selectionIsEmpty', but much simpler!
entityRangeIsEmpty :: EntityRange -> Bool
entityRangeIsEmpty (_, j) = j == 0

selectEverything :: RawContent -> SelectionState
selectEverything (RawContent bs _) = SelectionState False (SelectionPoint sb so) (SelectionPoint eb eo)
  where
    sb = NEL.head bs ^. blockKey
    so = 0
    eb = NEL.last bs ^. blockKey
    eo = NEL.last bs ^. blockText . to ST.length


-- * vdoc

-- | The 'DataUID' values are actually block numbers (yes, this is cheating, but it works for the
-- backend :-).  The 'RawContent' is needed to convert block keys to block numbers and back.
chunkRangeToSelectionState :: RawContent -> ChunkRange -> SelectionState
chunkRangeToSelectionState (RawContent bs _) (ChunkRange s e) = SelectionState False (maybe top trans s) (maybe bottom trans e)
  where
    trans (ChunkPoint (DataUID blocknum) offset) = SelectionPoint ((bs NEL.!! blocknum) ^. blockKey) offset

    top :: SelectionPoint
    top = SelectionPoint (NEL.head bs ^. blockKey) 0

    bottom :: SelectionPoint
    bottom = SelectionPoint (NEL.last bs ^. blockKey) (ST.length $ NEL.last bs ^. blockText)


-- | See 'chunkRangeToSelectionState'.
selectionStateToChunkRange :: RawContent -> SelectionState -> ChunkRange
selectionStateToChunkRange (RawContent (NEL.toList -> bs) _) (SelectionState _ s e) = ChunkRange (trans s) (trans e)
  where
    trans (SelectionPoint bk offset) = Just (ChunkPoint (DataUID blocknum) offset)
      where
        [(blocknum, _)] = filter (\(_, b) -> b ^. blockKey == bk) $ zip [0..] bs


rawContentFromCompositeVDoc :: CompositeVDoc -> RawContent
rawContentFromCompositeVDoc (CompositeVDoc _ _ vers edits notes discussions) =
  addMarksToRawContent marks rawContent
  where
    rawContent = rawContentFromVDocVersion vers
    convertHack l (k, v) = (contribID k, chunkRangeToSelectionState rawContent $ v ^. l)

    marks :: [(ContributionID, SelectionState)]
    marks = [(contribID k, s) | (k, e) <- Map.toList edits, s <- NEL.toList $ e ^. editRanges]
         <> (convertHack noteRange                               <$> Map.toList notes)
         <> (convertHack (compositeDiscussion . discussionRange) <$> Map.toList discussions)

rawContentFromVDocVersion :: VDocVersion -> RawContent
rawContentFromVDocVersion (VDocVersion st) = case eitherDecode $ cs st of
  Right v -> v
  Left msg -> error $ "rawContentFromVDocVersion: " <> show (msg, st)

rawContentToVDocVersion :: RawContent -> VDocVersion
rawContentToVDocVersion = VDocVersion . cs . encode


-- * marks

deleteMarksFromRawContent :: RawContent -> RawContent
deleteMarksFromRawContent = deleteMarksFromRawContentIf (const True)

deleteMarksFromRawContentIf :: (ContributionID -> Bool) -> RawContent -> RawContent
deleteMarksFromRawContentIf p = rawContentBlocks %~ fmap (deleteMarksFromBlockIf p)

deleteMarksFromBlock :: Block EntityKey BlockKey -> Block EntityKey BlockKey
deleteMarksFromBlock = deleteMarksFromBlockIf (const True)

deleteMarksFromBlockIf :: (ContributionID -> Bool) -> Block EntityKey BlockKey -> Block EntityKey BlockKey
deleteMarksFromBlockIf p = blockStyles %~ List.filter (p' . snd)
  where
    p' (Mark cid) = not (p cid)
    p' _          = True


addMarksToRawContent :: [(ContributionID, SelectionState)] -> RawContent -> RawContent
addMarksToRawContent marks = rawContentBlocks %~ addMarksToBlocks marks

addMarksToBlocks :: [(ContributionID, SelectionState)] -> NonEmpty (Block EntityKey BlockKey) -> NonEmpty (Block EntityKey BlockKey)
addMarksToBlocks m bs = case (addMarksToBlock (warmupSelectionStates m) `mapM` bs) `runState` [] of
  (bs', []) -> bs'
  bad -> error $ "addMarksToBlocks: impossible: " <> show bad

type AddMarksState = State [SoloSelectionPoint]

-- | 'SelectionPoint' that carries extra information needed for 'addMarksToBlocks'.
data SoloSelectionPoint = SoloSelectionPoint
  { soloSelectionPointPoint   :: SelectionPoint
  , soloSelectionPointID      :: ContributionID
  , soloSelectionPointIsStart :: Bool
  }
  deriving (Eq, Show)

warmupSelectionStates :: [(ContributionID, SelectionState)] -> Map BlockKey [SoloSelectionPoint]
warmupSelectionStates = aggr . mconcat . fmap trans
  where
    aggr :: (Ord k) => [(k, v)] -> Map k [v]
    aggr = Map.fromListWith (<>) . map (second pure)

    trans :: (ContributionID, SelectionState) -> [(BlockKey, SoloSelectionPoint)]
    trans (i, SelectionState _ p1 p2) =
      [ (p1 ^. selectionBlock, SoloSelectionPoint p1 i True)
      , (p2 ^. selectionBlock, SoloSelectionPoint p2 i False)
      ]

-- | 'Refine.Common.VDoc.OT.mkBlock' solves a similar problem.  Basically the ranges are sorted by
-- starting point, then I go through this sorted list and a stack of active ranges is maintained
-- during it. If I would have more time I would think of the performance of these two alternative
-- approaches or how these could be tuned if necessary.  [divipp]
addMarksToBlock :: Map BlockKey [SoloSelectionPoint] -> Block EntityKey BlockKey -> AddMarksState (Block EntityKey BlockKey)
addMarksToBlock pointmap block = f (fromMaybe [] $ Map.lookup (block ^. blockKey) pointmap) block
  where
    f :: [SoloSelectionPoint] -> Block EntityKey BlockKey -> AddMarksState (Block EntityKey BlockKey)
    f pointlist blk = do
      previousOpenPoints :: [SoloSelectionPoint] <- get
      let (newOpenPoints, newClosePoints) = List.partition soloSelectionPointIsStart pointlist

          blocklen :: Int
          blocklen = blk ^. blockText . to ST.length

          inlineStyles :: [(EntityRange, Style)]
          inlineStyles = List.filter (not . entityRangeIsEmpty . fst)
                       $ (addMarkToBlock blocklen True  newClosePoints <$> previousOpenPoints)
                      <> (addMarkToBlock blocklen False newClosePoints <$> newOpenPoints)

      modify ( List.filter ((`notElem` (soloSelectionPointID <$> newClosePoints)) . soloSelectionPointID)
             . (newOpenPoints <>)
             )
      blk & blockStyles %~ (inlineStyles <>) & pure

addMarkToBlock :: Int -> Bool -> [SoloSelectionPoint] -> SoloSelectionPoint -> (EntityRange, Style)
addMarkToBlock blocklen openedInOtherBlock newClosePoints thisPoint = assert (start >= 0 && end >= 0) ((start, end), style)
  where
    style = Mark . soloSelectionPointID $ thisPoint

    start = if openedInOtherBlock
      then 0
      else soloSelectionPointPoint thisPoint ^. selectionOffset

    end = case List.filter ((== soloSelectionPointID thisPoint) . soloSelectionPointID) newClosePoints of
      sps -> minimum $ blocklen : [e | sp <- sps, let e = soloSelectionPointPoint sp ^. selectionOffset - start, e >= 0]


-- | See 'mkSomeSegments' (@payload@ is 'Style').  (See 'rawContentToDoc' for another use of 'mkSomeSegments'.)
mkInlineStyleSegments :: [(EntityRange, Style)] -> [(Int, Set Style)]
mkInlineStyleSegments = mkSomeSegments fst snd

data BlockId = BlockId Int BlockKey
    deriving (Eq, Ord)

data SegId = SegId Int Bool{-last segment-}
    deriving (Eq, Ord)

getMarkSelectors :: RawContent -> [(ContributionID, MarkSelector, MarkSelector)]
getMarkSelectors
    = concatMap createRanges
    . List.groupBy ((==) `on` fst) . sort
    . mconcat
    . zipWith collectBlock [0..]
    . NEL.toList . view rawContentBlocks
  where
    collectBlock :: Int -> Block EntityKey BlockKey -> [(ContributionID, ((BlockId, SegId), (BlockId, SegId)))]
    collectBlock bix block
      = [ (cid, ((bid, six), (bid, six)))
        | (six, (_, styles)) <- addSegIds . mkInlineStyleSegments $ block ^. blockStyles
        , Mark cid <- Set.toList styles
        -- (note that this case keeps track of 'ContribIDHighlightMark' positions, even though
        -- that's not needed for anything.)
        , let bid = BlockId bix $ block ^. blockKey
        ]

    addSegIds :: [a] -> [(SegId, a)]
    addSegIds as = zip (zipWith SegId [0..] $ replicate (length as - 1) False <> [True]) as

    createRanges :: [(ContributionID, ((BlockId, SegId), (BlockId, SegId)))] -> [(ContributionID, MarkSelector, MarkSelector)]
    createRanges [] = error "impossible"
    createRanges arg@((cid, _) : _) =
        [ (cid, MarkSelector MarkSelectorTop k1 o1, MarkSelector MarkSelectorBottom k2 o2)
        | ((BlockId _ k1, SegId o1 _), (BlockId _ k2, SegId o2 _)) <- foldr addSelector [] $ snd <$> arg]

    addSelector (b1, e1) ((b2, e2): ss) | e1 `nextTo` b2 = (b1, e2): ss
    addSelector s ss = s: ss

    nextTo :: (BlockId, SegId) -> (BlockId, SegId) -> Bool
    nextTo (BlockId i _, SegId j isend) (BlockId i' _, SegId j' _)
        =  i == i' && j+1 == j'
        || i+1 == i' && isend && j' == 0
