{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.VDoc where

import           Control.DeepSeq
import           Control.Exception (assert)
import           Control.Lens (makeLenses, makePrisms, Lens', view, (&), (%~), (^?!), _Just, (^.), to)
import           Control.Monad.State
import           Data.Aeson
import           Data.List as List
import           Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.String.Conversions (ST, cs, (<>))
import qualified Data.Text           as ST
import           Data.Typeable
import qualified Generics.SOP        as SOP
import qualified Generics.SOP.JSON   as SOP
import qualified Generics.SOP.NFData as SOP
import           GHC.Generics (Generic)

import Refine.Common.Orphans ()
import Refine.Common.Types.Chunk
import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Common.VDoc.Draft
import Refine.Prelude
import Refine.Prelude.TH (makeRefineType)


data VDoc = VDoc
  { _vdocMetaID   :: MetaID VDoc
  , _vdocTitle    :: Title
  , _vdocAbstract :: Abstract
  , _vdocRepo     :: ID VDocRepo
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- the name clashes in the record selectors are really annoying...
-- makes me understand why people were so fond of OO when they invented it
data CreateVDoc = CreateVDoc
  { _createVDocTitle       :: Title
  , _createVDocAbstract    :: Abstract
  , _createVDocInitVersion :: VDocVersion
  }
  deriving (Eq, Ord, Show, Generic)

newtype Title = Title { _unTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Abstract = Abstract { _unAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocVersion = VDocVersion { _unVDocVersion :: ST }
  deriving (Eq, Ord, Show, Generic, Monoid)

-- | TODO: this should not be needed anywhere except perhaps in tests
vdocVersionFromSTSafe :: ST -> Either String VDocVersion
vdocVersionFromSTSafe = Right . VDocVersion

-- | TODO: this should not be needed anywhere except perhaps in tests
vdocVersionFromST :: ST -> VDocVersion
vdocVersionFromST = VDocVersion

data VDocRepo = VDocRepo
  { _vdocRepoID    :: ID VDocRepo
  , _vdocHeadEdit  :: ID Edit
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Edit = Edit
  { _editMetaID :: MetaID Edit
  , _editDesc   :: ST
  , _editRange  :: ChunkRange
  , _editKind   :: EditKind
  , _editMotiv  :: ST  -- (list of paragraphs)
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateEdit = CreateEdit
  { _createEditDesc  :: ST
  , _createEditRange :: ChunkRange
  , _createEditVDoc  :: VDocVersion
  , _createEditKind  :: EditKind
  , _createEditMotiv :: ST
  }
  deriving (Eq, Ord, Show, Generic)

data EditKind = Grammar | Phrasing | Meaning | Initial
  deriving (Eq, Ord, Show, Read, Generic)

data ConflictResolution = ConflictResolution
  deriving (Eq, Ord, Show, Read, Generic)


-- * create types, instances

type instance Create VDoc  = CreateVDoc
type instance Create Edit = CreateEdit


-- * refine types

makeRefineType ''VDoc
makeRefineType ''CreateVDoc
makeRefineType ''VDocRepo
makeRefineType ''Edit
makeRefineType ''CreateEdit
makeRefineType ''EditKind
makeRefineType ''ConflictResolution
makeRefineType ''Title
makeRefineType ''Abstract

-- ('makeRefineType' doesn't support parametric types.)
instance SOP.Generic VDocVersion
instance SOP.HasDatatypeInfo VDocVersion
instance NFData VDocVersion where rnf = SOP.grnf
instance SOP.ToJSON VDocVersion where toJSON = gtoJSONDef
instance SOP.FromJSON VDocVersion where parseJSON = gparseJSONDef
-- TODO: aeson-encode phantom type in json for cross-network type safety
makeLenses ''VDocVersion
makePrisms ''VDocVersion


-- * composites

-- | Packaged vdoc ready for use by client.
--
-- - morally we have three phases in working on a document: (1) add comments and edits, (2) merge a
--   bunch of edits and (3) create a new version.
--
-- - what follows from this:
--     - there are no edits on edits that we need to display
--     - it's ok to only display edits on head, not on any other version
--     - same for comments: comments collect on head, then then are discarded in (2), (3).
--
-- - if we try to consider comments, edits, ... on other versions than head, we are in trouble.
data CompositeVDoc = CompositeVDoc
  { _compositeVDoc            :: VDoc
  , _compositeVDocRepo        :: VDocRepo
  , _compositeVDocEditID      :: ID Edit
  , _compositeVDocVersion     :: VDocVersion
  , _compositeVDocEdits       :: Map (ID Edit) Edit
  , _compositeVDocNotes       :: Map (ID Note) Note
  -- , _compositeVDocQuestions   :: [Question]  -- will be due in #99
  , _compositeVDocDiscussions :: Map (ID Discussion) CompositeDiscussion
  }
  deriving (Eq, Show, Generic)

makeRefineType ''CompositeVDoc

vdocID :: Lens' VDoc (ID VDoc)
vdocID = vdocMetaID . miID

editID :: Lens' Edit (ID Edit)
editID = editMetaID . miID


-- * draft

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
  Left msg -> error $ "vdocVersionToRawContent: " <> show (msg, st)

rawContentToVDocVersion :: RawContent -> VDocVersion
rawContentToVDocVersion = VDocVersion . cs . encode


deleteMarksFromBlock :: Block EntityKey -> Block EntityKey
deleteMarksFromBlock = blockStyles %~ List.filter ((`elem` [Bold, Italic, Underline, Code]) . snd)

addMarksToBlocks :: forall a. (Typeable a) => Map (ID a) SelectionState -> [Block EntityKey] -> [Block EntityKey]
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
    aggr = List.foldl' (\m (k, v) -> Map.alter (Just . maybe [v] (v:)) k m) mempty

    trans :: (ID a, SelectionState) -> [(BlockKey, SoloSelectionPoint a)]
    trans (i, SelectionState _ p1 p2) =
      [ (p1 ^. selectionBlock, SoloSelectionPoint p1 i True)
      , (p2 ^. selectionBlock, SoloSelectionPoint p2 i False)
      ]

addMarksToBlock :: forall a. (Typeable a) => Map BlockKey [SoloSelectionPoint a] -> Block EntityKey -> AddMarksState a (Block EntityKey)
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

addMarkToBlock :: forall a. (Typeable a) => Int -> Bool -> [SoloSelectionPoint a] -> SoloSelectionPoint a -> (EntityRange, Style)
addMarkToBlock blocklen openedInOtherBlock newClosePoints thisPoint = assert (start >= 0 && end >= 0) ((start, end), style)
  where
    style = if
      | typeOf (Proxy :: Proxy a) == typeOf (Proxy :: Proxy Edit) -> RangeEdit
      | otherwise                                                 -> RangeComment

    start = if openedInOtherBlock
      then 0
      else soloSelectionPointPoint thisPoint ^. selectionOffset

    end = case List.filter ((== soloSelectionPointID thisPoint) . soloSelectionPointID) newClosePoints of
      []   -> blocklen
      [sp] -> soloSelectionPointPoint sp ^. selectionOffset - start
      bad  -> error $ "addMarkToBlock: impossible: " <> show bad
