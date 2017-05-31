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
import           Data.List.NonEmpty (NonEmpty)
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
emptyRawContent = mkRawContent [emptyBlock]

resetBlockKeys :: RawContent -> RawContent
resetBlockKeys (RawContent bs es) = RawContent (set blockKey Nothing <$> bs) es

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
selectedBlocks :: SelectionState -> [Block EntityKey] -> [Block EntityKey]
selectedBlocks (SelectionState _ (SelectionPoint sk _) (SelectionPoint ek _)) = f
  where
    f [] = []
    f (b:bs) = if b ^. blockKey == Just sk then b : g bs else f bs

    g [] = []
    g (b:bs) = if b ^. blockKey == Just ek then [b] else b : g bs

-- | Like 'selectionIsEmpty', but much simpler!
entityRangeIsEmpty :: EntityRange -> Bool
entityRangeIsEmpty (_, j) = j == 0

-- | This is a work-around for until we change 'SelectionState' to carry @Maybe 'SelectionPoint'@s,
-- then 'Nothing' means beginning / end of document, resp.
selectEverything :: RawContent -> SelectionState
selectEverything (RawContent bs _) = SelectionState False (SelectionPoint sb so) (SelectionPoint eb eo)
  where
    sb = NEL.head bs ^?! blockKey . _Just
    so = 0
    eb = NEL.last bs ^?! blockKey . _Just
    eo = NEL.last bs ^. blockText . to ST.length


-- * vdoc

-- | The 'DataUID' values are actually block numbers (yes, this is cheating, but it works for the
-- backend :-).  The 'RawContent' is needed to convert block keys to block numbers and back.
chunkRangeToSelectionState :: RawContent -> ChunkRange -> SelectionState
chunkRangeToSelectionState (RawContent bs _) (ChunkRange s e) = SelectionState False (maybe top trans s) (maybe bottom trans e)
  where
    trans (ChunkPoint (DataUID blocknum) offset) = SelectionPoint blockkey offset
      where
        Just blockkey = (bs NEL.!! blocknum) ^. blockKey

    top :: SelectionPoint
    top = SelectionPoint (fromJust $ NEL.head bs ^. blockKey) 0

    bottom :: SelectionPoint
    bottom = SelectionPoint (fromJust $ NEL.last bs ^. blockKey) (ST.length $ NEL.last bs ^. blockText)


-- | See 'chunkRangeToSelectionState'.
selectionStateToChunkRange :: RawContent -> SelectionState -> ChunkRange
selectionStateToChunkRange (RawContent (NEL.toList -> bs) _) (SelectionState _ s e) = ChunkRange (trans s) (trans e)
  where
    trans (SelectionPoint blockkey offset) = Just (ChunkPoint (DataUID blocknum) offset)
      where
        [(blocknum, _)] = filter (\(_, b) -> b ^. blockKey == Just blockkey) $ zip [0..] bs


rawContentFromCompositeVDoc :: CompositeVDoc -> RawContent
rawContentFromCompositeVDoc (CompositeVDoc _ _ vers edits notes discussions) =
  addMarksToRawContent marks rawContent
  where
    rawContent = rawContentFromVDocVersion vers
    convertHack l (k, v) = (contribID k, chunkRangeToSelectionState rawContent $ v ^. l)

    marks :: [(ContributionID, SelectionState)]
    marks = (convertHack editRange                               <$> Map.toList edits)
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

deleteMarksFromBlock :: Block EntityKey -> Block EntityKey
deleteMarksFromBlock = deleteMarksFromBlockIf (const True)

deleteMarksFromBlockIf :: (ContributionID -> Bool) -> Block EntityKey -> Block EntityKey
deleteMarksFromBlockIf p = blockStyles %~ List.filter (p' . snd)
  where
    p' (Mark cid) = not (p cid)
    p' _          = True


addMarksToRawContent :: [(ContributionID, SelectionState)] -> RawContent -> RawContent
addMarksToRawContent marks = rawContentBlocks %~ addMarksToBlocks marks

addMarksToBlocks :: [(ContributionID, SelectionState)] -> NonEmpty (Block EntityKey) -> NonEmpty (Block EntityKey)
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
addMarksToBlock :: Map BlockKey [SoloSelectionPoint] -> Block EntityKey -> AddMarksState (Block EntityKey)
addMarksToBlock pointmap block = f (fromMaybe [] $ Map.lookup (block ^?! blockKey . _Just) pointmap) block
  where
    f :: [SoloSelectionPoint] -> Block EntityKey -> AddMarksState (Block EntityKey)
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
      []   -> blocklen
      [sp] -> soloSelectionPointPoint sp ^. selectionOffset - start
      bad  -> error $ "addMarkToBlock: impossible: " <> show bad


-- | See 'mkSomeSegments' (@payload@ is 'Style').  (See 'rawContentToDoc' for another use of 'mkSomeSegments'.)
mkInlineStyleSegments :: [(EntityRange, Style)] -> [(Int, Set Style)]
mkInlineStyleSegments = mkSomeSegments fst snd

getMarkSelectors :: RawContent -> [(ContributionID, MarkSelector, MarkSelector)]
getMarkSelectors = findSides . mconcat . fmap collectBlock . zip [0..] . NEL.toList . view rawContentBlocks
  where
    collectBlock :: (Int, Block EntityKey) -> [(ContributionID, ((Int, Int), MarkSelector))]
    collectBlock (bix, block) = catMaybes $ collectSegment <$> warmupSegments (mkInlineStyleSegments $ block ^. blockStyles)
      where
        warmupSegments :: [(Int, Set Style)] -> [(Int, Style)]
        warmupSegments segs
          = [ (six, style)
            | (six, (_, styles)) <- zip [0..] segs
            , style <- Set.toList styles
            ]

        collectSegment :: (Int, Style) -> Maybe (ContributionID, ((Int, Int), MarkSelector))
        collectSegment (six, Mark cid)
          = Just (cid, ((bix, six), MarkSelector MarkSelectorUnknownSide (block ^?! blockKey . _Just) six))
            -- (note that this case keeps track of 'ContribIDHighlightMark' positions, even though
            -- that's not needed for anything.)
        collectSegment _
          = Nothing

    findSides :: [(ContributionID, ((Int, Int), MarkSelector))] -> [(ContributionID, MarkSelector, MarkSelector)]
    findSides = fmap pickMinMax . List.groupBy ((==) `on` fst) . sortBy (compare `on` fst)

    pickMinMax :: [(ContributionID, ((Int, Int), MarkSelector))] -> (ContributionID, MarkSelector, MarkSelector)
    pickMinMax [] = error "impossible"
    pickMinMax arg@((cid, _) : _) = (cid, top, bot)
      where
        marks = Map.fromList $ snd <$> arg
        top   = case Map.findMin marks of (_, MarkSelector _ k o) -> MarkSelector MarkSelectorTop    k o
        bot   = case Map.findMax marks of (_, MarkSelector _ k o) -> MarkSelector MarkSelectorBottom k o
