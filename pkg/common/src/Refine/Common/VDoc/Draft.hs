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

import           Control.Arrow (second)
import           Control.Exception (assert)
import           Control.Lens (makeLenses, view, set, (^.), (&), (%~), _Just, to, (^?!))
import           Control.Monad (foldM)
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Foldable (toList)
import           Data.Functor.Infix ((<$$>))
import qualified Data.HashMap.Lazy as HashMap
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.List (nub)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.String.Conversions
import qualified Data.Text as ST
import           Data.Typeable (Typeable)
import           GHC.Generics hiding (to)
import           Web.HttpApiData (toUrlPiece, parseUrlPiece)

import Refine.Common.Types
import Refine.Prelude.TH hiding (typeOf)


-- * data types

-- | Haskell representation of the javascript @RawDraftContentState@.
-- https://draftjs.org/docs/api-reference-data-conversion.html#content
data RawContent = RawContent
  { _rawContentBlocks    :: [Block EntityKey]  -- ^ FIXME: use Data.List.NonEmpty from semigroups.
  , _rawContentEntityMap :: IntMap Entity  -- ^ for performance, do not use @Map EntityKey Entity@ here.
  }
  deriving (Eq, Show, Generic)

-- | typical rangekey values are 'Int' and 'Entity'
data Block rangeKey = Block
  { _blockText         :: ST
  , _blockEntityRanges :: [(rangeKey, EntityRange)]
  , _blockStyles       :: [(EntityRange, Style)]
  , _blockType         :: BlockType
  , _blockDepth        :: Int
  , _blockKey          :: Maybe BlockKey  -- ^ FIXME: many function rely on this being defined.
                                          -- make this an index and either store '()' or 'BlockKey'.
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

-- | `key` attribute of the 'Block'.  'SelectionState' uses this to refer to blocks.  If in doubt
-- leave it 'Nothing'.
newtype BlockKey = BlockKey ST
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic)

-- | key into 'rawContentEntityMap'.
newtype EntityKey = EntityKey { _unEntityKey :: Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic)

type EntityRange = (Int, Int)

-- | an entity's range may span across multiple blocks
newtype Entity =
    EntityLink ST  -- ^ url
--  | ...
  deriving (Show, Eq, Ord, Generic)

-- | a style's range should fit into a single block.
--
-- NOTE: 'Mark' could be an entity if it weren't for the fact that we need overlaps (see
-- https://github.com/facebook/draft-js/issues/212).
data Style =
    Bold
  | Italic
  | Underline
  | Code
    -- custom styles
  | Mark ContributionID
  deriving (Show, Eq, Ord, Generic)

-- | each block has a unique blocktype
data BlockType =
    NormalText
  | Header1
  | Header2
  | Header3
  | BulletPoint
  | EnumPoint
  deriving (Show, Eq, Generic, Bounded, Enum)


-- | https://draftjs.org/docs/api-reference-selection-state.html
data SelectionState
  = SelectionState
      { _selectionIsBackward :: Bool
      , _selectionStart      :: SelectionPoint
      , _selectionEnd        :: SelectionPoint
      }
  deriving (Eq, Ord, Show, Generic)

data SelectionPoint
  = SelectionPoint
      { _selectionBlock  :: BlockKey
      , _selectionOffset :: Int
      }
  deriving (Eq, Ord, Show, Generic)


-- * instances

makeLenses ''RawContent
makeLenses ''Block
makeLenses ''BlockKey
makeLenses ''EntityKey
makeLenses ''Entity
makeLenses ''Style
makeLenses ''BlockType

makeSOPGeneric ''RawContent
makeSOPGeneric ''Block
makeSOPGeneric ''BlockKey
makeSOPGeneric ''EntityKey
makeSOPGeneric ''Entity
makeSOPGeneric ''Style
makeSOPGeneric ''BlockType

makeNFData ''RawContent
makeNFData ''Block
makeNFData ''BlockKey
makeNFData ''EntityKey
makeNFData ''Entity
makeNFData ''Style
makeNFData ''BlockType

instance ToJSON RawContent where
  toJSON (RawContent blocks entitymap) = object
    [ "blocks"    .= blocks
    , "entityMap" .= renderEntityMap entitymap
    ]
    where
      renderEntityMap m = object [ cs (show a) .= b | (a, b) <- IntMap.toList m ]

instance FromJSON RawContent where
  parseJSON = withObject "RawContent" $ \obj -> RawContent
    <$> obj .: "blocks"
    <*> (parseEntityMap =<< obj .: "entityMap")
    where
      parseEntityMap = withObject "parseEntityMap" $ foldM f mempty . HashMap.toList
        where
          f :: IntMap Entity -> (ST, Value) -> Parser (IntMap Entity)
          f m (read . cs -> k, v) = (\e -> IntMap.insert k e m) <$> parseJSON v

instance ToJSON (Block EntityKey) where
  toJSON (Block content ranges styles ty depth key) = object $
    [ "text"              .= content
    , "entityRanges"      .= (renderRange <$> ranges)
    , "inlineStyleRanges" .= (renderStyle <$> styles)
    , "depth"             .= depth  -- ^ (if certain BlockType values force this field to be 0, move this field there.)
    , "type"              .= ty
    ] <>
    [ "key" .= k | k <- maybeToList key ]
    where
      renderRange (k, (o, l)) = object ["key"   .= k, "length" .= l, "offset" .= o]
      renderStyle ((o, l), s) = object ["style" .= s, "length" .= l, "offset" .= o]

instance FromJSON (Block EntityKey) where
  parseJSON = withObject "Block EntityKey" $ \obj -> Block
    <$> obj .: "text"
    <*> (mapM parseRange =<< (obj .: "entityRanges"))
    <*> (mapM parseStyle =<< (obj .: "inlineStyleRanges"))
    <*> obj .: "type"
    <*> (round <$> (obj .: "depth" :: Parser Double))
    <*> obj .:? "key"
    where
      parseRange = withObject "Block EntityKey: entityRanges" $ \obj -> do
        k <- obj .: "key"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure (k, (o, l))
      parseStyle = withObject "Block EntityKey: inlineStyleRanges" $ \obj -> do
        s <- obj .: "style"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure ((o, l), s)

instance ToJSON BlockType where
  toJSON NormalText  = "unstyled"
  toJSON Header1     = "header-one"
  toJSON Header2     = "header-two"
  toJSON Header3     = "header-three"
  toJSON BulletPoint = "unordered-list-item"
  toJSON EnumPoint   = "ordered-list-item"

instance FromJSON BlockType where
  parseJSON (String "unstyled")            = pure NormalText
  parseJSON (String "header-one")          = pure Header1
  parseJSON (String "header-two")          = pure Header2
  parseJSON (String "header-three")        = pure Header3
  parseJSON (String "unordered-list-item") = pure BulletPoint
  parseJSON (String "ordered-list-item")   = pure EnumPoint
  parseJSON bad = fail $ "BlockType: no parse for " <> show bad

instance ToJSON Entity where
  toJSON (EntityLink url) = object
    [ "type"            .= ("LINK" :: ST)
    , "mutability"      .= ("MUTABLE" :: ST)
    , "data"            .= object ["url" .= url]
    ]

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \obj -> do
    ty :: ST <- obj .: "type"
    case ty of
      "LINK" -> let parseData = withObject "LINK data" (.: "url")
                in EntityLink <$> (parseData =<< obj .: "data")
      bad -> fail $ "Entity: no parse for " <> show bad

instance ToJSON Style where
  toJSON Bold         = "BOLD"
  toJSON Italic       = "ITALIC"
  toJSON Underline    = "UNDERLINE"
  toJSON Code         = "CODE"
  toJSON (Mark cid)   = String $ "MARK__" <> toUrlPiece cid

instance FromJSON Style where
  parseJSON (String "BOLD")                 = pure Bold
  parseJSON (String "ITALIC")               = pure Italic
  parseJSON (String "UNDERLINE")            = pure Underline
  parseJSON (String "CODE")                 = pure Code
  parseJSON (String (ST.splitAt 6 -> ("MARK__", parseUrlPiece -> Right cid)))
                                            = pure $ Mark cid
  parseJSON bad = fail $ "Style: no parse for " <> show bad

makeRefineType ''SelectionState
makeRefineType ''SelectionPoint


-- * functions

-- | Note: empty block list is illegal.  For once it will make draft crash in 'stateFromContent'.
mkRawContent :: [Block Entity] -> RawContent
mkRawContent [] = mkRawContent [emptyBlock]
mkRawContent bs = RawContent (index <$$> bs) (IntMap.fromList entities)
  where
    -- FUTUREWORK: it is possible to do just one traversal to collect and index entities
    -- https://www.reddit.com/r/haskell/comments/610sa1/applicative_sorting/
    entities = zip [0..] . nub $ concatMap toList bs

    index :: Entity -> EntityKey
    index e = EntityKey . fromMaybe (error "mkRawContent: impossible") $ Map.lookup e em

    em = Map.fromList $ (\(a, b) -> (b, a)) <$> entities

emptyRawContent :: RawContent
emptyRawContent = mkRawContent [emptyBlock]

mkBlock :: ST -> Block rangeKey
mkBlock t = Block t [] [] NormalText 0 Nothing

emptyBlock :: Block rangeKey
emptyBlock = mkBlock mempty

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
    multiLineCase = case selectedBlocks ss bs of
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


-- * vdoc

-- | The 'DataUID' values are actually block numbers (yes, this is cheating, but it works for the
-- backend :-).  The 'RawContent' is needed to convert block keys to block numbers and back.  This
-- function isn't total, but all undefined values are internal errors`.
chunkRangeToSelectionState :: RawContent -> ChunkRange -> SelectionState
chunkRangeToSelectionState (RawContent bs _) (ChunkRange s e) = SelectionState False (trans s) (trans e)
  where
    trans (Just (ChunkPoint (DataUID blocknum) offset)) = SelectionPoint blockkey offset
      where
        Just blockkey = (bs !! blocknum) ^. blockKey
    trans bad = error $ "chunkRangeToSelectionState: impossibel: " <> show bad

-- | See 'chunkRangeToSelectionState'.
selectionStateToChunkRange :: RawContent -> SelectionState -> ChunkRange
selectionStateToChunkRange (RawContent bs _) (SelectionState _ s e) = ChunkRange (trans s) (trans e)
  where
    trans (SelectionPoint blockkey offset) = Just (ChunkPoint (DataUID blocknum) offset)
      where
        [(blocknum, _)] = filter (\(_, b) -> b ^. blockKey == Just blockkey) $ zip [0..] bs


rawContentFromCompositeVDoc :: CompositeVDoc -> RawContent
rawContentFromCompositeVDoc (CompositeVDoc _ _ _ vers edits notes discussions) =
  rawContentFromVDocVersion vers & rawContentBlocks %~ f
  where
    -- 'convertHack' will go away when 'ChunkRange' goes away.
    convertHack = chunkRangeToSelectionState (rawContentFromVDocVersion vers)
    f = addMarksToBlocks (convertHack . view editRange <$> edits)
      . addMarksToBlocks (convertHack . view noteRange <$> notes)
      . addMarksToBlocks (convertHack . view (compositeDiscussion . discussionRange) <$> discussions)
      . fmap deleteMarksFromBlock  -- (this shouldn't be doing anything, the raw content value has
                                   -- just been created.  but it may be necessary somewhere else,
                                   -- after vdoc version has been refactored away, so we keep it as
                                   -- a reminder.)


rawContentFromVDocVersion :: VDocVersion -> RawContent
rawContentFromVDocVersion (VDocVersion st) = case eitherDecode $ cs st of
  Right v -> v
  Left msg -> error $ "rawContentFromVDocVersion: " <> show (msg, st)

rawContentToVDocVersion :: RawContent -> VDocVersion
rawContentToVDocVersion = VDocVersion . cs . encode


deleteMarksFromBlock :: Block EntityKey -> Block EntityKey
deleteMarksFromBlock = blockStyles %~ List.filter ((`elem` [Bold, Italic, Underline, Code]) . snd)

addMarksToBlocks :: forall a. (Typeable a, IsContribution a) => Map (ID a) SelectionState -> [Block EntityKey] -> [Block EntityKey]
addMarksToBlocks m bs = case (addMarksToBlock (warmupSelectionStates m) `mapM` bs) `runState` [] of
  (bs', []) -> bs'
  bad -> error $ "addMarksToBlocks: impossible: " <> show bad

type AddMarksState a = State [SoloSelectionPoint a]

-- | 'SelectionPoint' that carries extra information needed for 'addMarksToBlocks'.
data SoloSelectionPoint a = SoloSelectionPoint
  { soloSelectionPointPoint   :: SelectionPoint
  , soloSelectionPointID      :: ID a
  , soloSelectionPointIsStart :: Bool
  }
  deriving (Eq, Show)

warmupSelectionStates :: forall a. (Typeable a) => Map (ID a) SelectionState -> Map BlockKey [SoloSelectionPoint a]
warmupSelectionStates = aggr . mconcat . fmap trans . Map.toList
  where
    aggr :: (Ord k) => [(k, v)] -> Map k [v]
    aggr = Map.fromListWith (<>) . map (second pure)

    trans :: (ID a, SelectionState) -> [(BlockKey, SoloSelectionPoint a)]
    trans (i, SelectionState _ p1 p2) =
      [ (p1 ^. selectionBlock, SoloSelectionPoint p1 i True)
      , (p2 ^. selectionBlock, SoloSelectionPoint p2 i False)
      ]

-- | 'Refine.Common.VDoc.OT.mkBlock' solves a similar problem.  Basically the ranges are sorted by
-- starting point, then I go through this sorted list and a stack of active ranges is maintained
-- during it. If I would have more time I would think of the performance of these two alternative
-- approaches or how these could be tuned if necessary.  [divipp]
addMarksToBlock :: forall a. (Typeable a, IsContribution a)
                => Map BlockKey [SoloSelectionPoint a] -> Block EntityKey -> AddMarksState a (Block EntityKey)
addMarksToBlock pointmap block = f (fromMaybe [] $ Map.lookup (block ^?! blockKey . _Just) pointmap) block
  where
    f :: [SoloSelectionPoint a] -> Block EntityKey -> AddMarksState a (Block EntityKey)
    f pointlist blk = do
      previousOpenPoints :: [SoloSelectionPoint a] <- get
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

addMarkToBlock :: forall a. (Typeable a, IsContribution a)
               => Int -> Bool -> [SoloSelectionPoint a] -> SoloSelectionPoint a -> (EntityRange, Style)
addMarkToBlock blocklen openedInOtherBlock newClosePoints thisPoint = assert (start >= 0 && end >= 0) ((start, end), style)
  where
    style = Mark . contribID . soloSelectionPointID $ thisPoint

    start = if openedInOtherBlock
      then 0
      else soloSelectionPointPoint thisPoint ^. selectionOffset

    end = case List.filter ((== soloSelectionPointID thisPoint) . soloSelectionPointID) newClosePoints of
      []   -> blocklen
      [sp] -> soloSelectionPointPoint sp ^. selectionOffset - start
      bad  -> error $ "addMarkToBlock: impossible: " <> show bad
