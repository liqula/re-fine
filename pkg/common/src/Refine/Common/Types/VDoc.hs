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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.VDoc where

import           Control.DeepSeq
import           Control.Lens (makeLenses, makePrisms, Lens')
import           Data.Map
import           Data.String.Conversions (ST, cs)
import           Data.Tree (Forest)
import qualified Generics.SOP        as SOP
import qualified Generics.SOP.JSON   as SOP
import qualified Generics.SOP.NFData as SOP
import           GHC.Generics (Generic)
import           Text.HTML.Parser (Token, renderTokens, parseTokens)
import           Text.HTML.Tree (tokensToForest, tokensFromForest)

import Refine.Common.Orphans ()
import Refine.Common.Types.Chunk
import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Prelude
import Refine.Prelude.TH


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
  , _createVDocInitVersion :: VDocVersion 'HTMLRaw
  }
  deriving (Eq, Ord, Show, Generic)

newtype Title = Title { _unTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Abstract = Abstract { _unAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

data HTMLState = HTMLRaw | HTMLCanonical | HTMLWithMarks

newtype VDocVersion (state :: HTMLState) = VDocVersion { _unVDocVersion :: Forest Token }
  deriving (Eq, Ord, Show, Generic)

vdocVersionFromSTSafe :: ST -> Either String (VDocVersion b)
vdocVersionFromSTSafe = either (Left . show) (Right . VDocVersion) . tokensToForest . parseTokens

vdocVersionFromST :: ST -> VDocVersion b
vdocVersionFromST = (\(Right v) -> v) . vdocVersionFromSTSafe

vdocVersionToST :: VDocVersion b -> ST
vdocVersionToST = cs . renderTokens . tokensFromForest . _unVDocVersion

emptyVDocVersion :: VDocVersion b
emptyVDocVersion = vdocVersionFromST ""

data VDocRepo = VDocRepo
  { _vdocRepoID    :: ID VDocRepo
  , _vdocHeadEdit  :: ID Edit
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Edit = Edit
  { _editID    :: ID Edit
  , _editDesc  :: ST
  , _editRange :: ChunkRange
  , _editKind  :: EditKind
  , _editMotiv :: ST  -- (list of paragraphs)
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateEdit = CreateEdit
  { _createEditDesc  :: ST
  , _createEditRange :: ChunkRange
  , _createEditVDoc  :: VDocVersion 'HTMLRaw
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
instance SOP.Generic (VDocVersion a)
instance SOP.HasDatatypeInfo (VDocVersion a)
instance NFData (VDocVersion a) where rnf = SOP.grnf
instance SOP.ToJSON (VDocVersion a) where toJSON = gtoJSONDef
instance SOP.FromJSON (VDocVersion a) where parseJSON = gparseJSONDef
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
  , _compositeVDocVersion     :: VDocVersion 'HTMLWithMarks
  , _compositeVDocEdits       :: Map (ID Edit) Edit
  , _compositeVDocNotes       :: Map (ID Note) Note
  -- , _compositeVDocQuestions   :: [Question]  -- will be due in #99
  , _compositeVDocDiscussions :: Map (ID Discussion) CompositeDiscussion
  }
  deriving (Eq, Show, Generic)

makeRefineType ''CompositeVDoc

-- *

vdocID :: Lens' VDoc (ID VDoc)
vdocID = vdocMetaID . miID
