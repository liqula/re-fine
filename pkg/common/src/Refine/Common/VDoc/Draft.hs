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
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

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
import Refine.Common.Types.Contribution
import Refine.Common.Types.Comment
import Refine.Common.VDoc.OT (docRanges, docEditRanges)

-- * functions

emptyRawContent :: RawContent
emptyRawContent = mkRawContent $ emptyBlock :| []

-- | Collect all characters (non-newline whitespace is treates no different from letters) between
-- two selection points.
--
-- Two points worth nothing here:
--
-- (1) 'SelectionState' is always defined, even if nothing is selected.  If `window.getSelection()`
-- yields nothing, the selection state value in the editor state contains the empty selection (start
-- point == end point).
--
-- (2) Since blocks can be empty, empty selections can range over many lines.
rangeText :: BlockBoundary -> RawContent -> Range Position -> ST
rangeText blockBoundary (RawContent bs _) ss@(Range s e) = case selectedBlocks ss (NEL.toList bs) of
      []        -> ""
      [b]       -> ST.drop (s ^. columnIndex) . ST.take (e ^. columnIndex) $ b ^. blockText
      (b : bs') -> combine $
                         ST.drop (s ^. columnIndex) (b ^. blockText)
                       : (((^. blockText) <$> init bs') <> [ST.take (e ^. columnIndex) (last bs' ^. blockText)])
  where
    combine = case blockBoundary of
      BlockBoundaryIsNewline -> ST.intercalate "\n"
      BlockBoundaryIsEmpty   -> ST.concat

data BlockBoundary = BlockBoundaryIsNewline | BlockBoundaryIsEmpty
  deriving (Eq, Show)

selectedBlocks :: Range Position -> [Block EntityKey BlockKey] -> [Block EntityKey BlockKey]
selectedBlocks (Range a b)
    = drop (a ^. rowIndex) . take ((b ^. rowIndex) + 1)

maximumRange :: RawContent -> Range Position
maximumRange (RawContent bs _) = RangeInner (Position sb so) (Position eb eo)
  where
    sb = BlockIndex 0 $ NEL.head bs ^. blockKey
    so = 0
    eb = BlockIndex (NEL.length bs - 1) $ NEL.last bs ^. blockKey
    eo = NEL.last bs ^. blockText . to ST.length

minimumRange :: RawContent -> Range Position
minimumRange (RawContent bs _) = RangeInner (Position sb so) (Position sb so)
  where
    sb = BlockIndex 0 $ NEL.head bs ^. blockKey
    so = 0

-- | Check if 'rangeText' yields nothing (block bounderiers are treated as empty).
rangeIsEmpty :: RawContent -> Range Position -> Bool
rangeIsEmpty rc = isEmptyRange . fmap (toStylePosition rc)


-- * vdoc

rawContentFromCompositeVDoc :: CompositeVDoc -> RawContent
rawContentFromCompositeVDoc (CompositeVDoc _ base edits notes discussions) =
  addMarksToRawContent marks rawContent
  where
    rawContent = base ^. editVDocVersion

    convertHack l (k, v) = (MarkContribution (contribID k) 0, extendRange $ v ^. l)

    extendRange r
      | x == y    = fromStyleRange rawContent
                  . uncurry Range . (next *** next) $ surroundingStylePositions rawContent x
      | otherwise = r
      where
        Range x y = toStylePosition rawContent <$> r

        next (_: z: _) = z
        next zs = head zs

    marks :: [(MarkID, Range Position)]
    marks = [ (MarkContribution (contribID k) i, s)
            | (k, e) <- Map.toList edits
            , (diff, b) <- e ^. editSource . unEditSource
            , b == base ^. editID
            , let rs = unRanges $ docEditRanges diff rawContent
            , (i, s) <- zip (numberRanges rs) rs
            ]
         <> (convertHack noteRange       <$> Map.toList notes)
         <> (convertHack discussionRange <$> Map.toList discussions)

    numberRanges :: [Range Position] -> [Int]
    numberRanges
      = concatMap (\(i, rs) -> replicate (length rs) i)
      . zip [0..] . List.groupBy ((==) `on` (^. rangeBegin . rowIndex))


-- * marks

deleteMarksFromRawContent :: RawContent -> RawContent
deleteMarksFromRawContent = deleteMarksFromRawContentIf (const True)

deleteMarksFromRawContentIf :: (MarkID -> Bool) -> RawContent -> RawContent
deleteMarksFromRawContentIf p = rawContentBlocks %~ fmap (deleteMarksFromBlockIf p)

deleteMarksFromBlock :: Block EntityKey BlockKey -> Block EntityKey BlockKey
deleteMarksFromBlock = deleteMarksFromBlockIf (const True)

deleteMarksFromBlockIf :: (MarkID -> Bool) -> Block EntityKey BlockKey -> Block EntityKey BlockKey
deleteMarksFromBlockIf p = blockStyles %~ List.filter (p' . snd)
  where
    p' (Mark m) = not (p m)
    p' _ = True

-- FIXME: change type :: Map ContributionID (Ranges StylePosition) -> RawContent -> RawContent
addMarksToRawContent :: [(MarkID, Range Position)] -> RawContent -> RawContent
addMarksToRawContent marks rc = joinStyles . RawContentSeparateStyles txts $ foldl' addStyle stys marks
  where
    RawContentSeparateStyles txts stys = separateStyles rc

    addStyle sty (cid, range) = case Map.lookup key sty of
        Nothing -> Map.insert key r sty
        Just rs -> Map.insert key (r <> rs) sty
      where
        key = Right $ Mark cid
        r = rangesFromRange False $ toStylePosition rc <$> range

getLeafSelectors :: RawContent -> Map MarkID (Ranges LeafSelector)
getLeafSelectors rc
    = fmap (RangesInner . map (styleRangeToLeafSelectors rc) . unRanges)
    . mapMaybeKey f
    $ documentMarks (separateStyles rc)
  where
    f (Right (Mark m)) = Just m
    f _ = Nothing

-- the function should be strictly monotonic on Just values
mapMaybeKey :: (HasCallStack, Ord k, Ord l) => (k -> Maybe l) -> Map k a -> Map l a
mapMaybeKey f = Map.mapKeysMonotonic (fromJust . f) . Map.filterWithKey (\k _ -> isJust $ f k)


---------------------- separate style representation

data RawContentSeparateStyles = RawContentSeparateStyles
    { documentText  :: NonEmpty SeparateBlock
    , documentMarks :: Map (Either Entity Style) (Ranges StylePosition)
    }
    deriving (Eq, Show)

data SeparateBlock = SeparateBlock BlockKey BlockType BlockDepth ST
    deriving (Eq, Show)

separateStyles :: RawContent -> RawContentSeparateStyles
separateStyles rc = RawContentSeparateStyles
    { documentText  = mkSeparateBlock <$> (rc ^. rawContentBlocks)
    , documentMarks = toStyleRanges rc <$> Map.fromList (docRanges False lineElemLength f rc)
    }
  where
    mkSeparateBlock :: Block EntityKey BlockKey -> SeparateBlock
    mkSeparateBlock b = SeparateBlock (b ^. blockKey) (b ^. blockType) (BlockDepth $ b ^. blockDepth) (b ^. blockText)
    f ((Atom e, ss), _) = [Left x | x <- maybeToList e] <> [Right s | Atom s <- Set.toList ss]

joinStyles :: RawContentSeparateStyles -> RawContent
joinStyles (RawContentSeparateStyles txts sty)
    = mkRawContentInternal . NEL.fromList . zipWith f [0..] $ NEL.toList txts
  where
    f i (SeparateBlock key ty (BlockDepth d) txt)
        = Block txt [(e, r) | (r, Left e) <- ranges] [(r, s) | (r, Right s) <- ranges] ty d key
      where
        ranges = sort [(r, s) | (s, rs) <- Map.toList sty, r <- concatMap cutRange $ unRanges rs]

        cutRange :: Range StylePosition -> [EntityRange]
        cutRange (Range (StylePosition (Position (BlockIndex i1 _) c1) m1) (StylePosition (Position (BlockIndex i2 _) c2) _m2))
            = [EntityRange beg (end - beg) | i1 + m1 <= i, i <= i2, end > beg]
          where
            beg | i == i1   = c1
                | otherwise = 0
            end | i == i2   = c2
                | otherwise = ST.length txt
