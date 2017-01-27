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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.VDoc where

import           Control.Lens (makeLenses, makePrisms)
import           Data.Map
import           Data.String.Conversions (ST)
import           GHC.Generics (Generic)
import           Control.DeepSeq
import qualified Generics.SOP        as SOP
import qualified Generics.SOP.JSON   as SOP
import qualified Generics.SOP.NFData as SOP
import           Refine.Prelude

import Refine.Common.Orphans ()
import Refine.Common.Types.Chunk
import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Prelude.TH


data VDoc = VDoc
  { _vdocID       :: ID VDoc
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
  deriving (Eq, Ord, Show, Read, Generic)

newtype Title = Title { _unTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Abstract = Abstract { _unAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

data HTMLState = HTMLRaw | HTMLCanonical | HTMLWithMarks

-- | TODO: `newtype VDocVersion = VDocVersion { _unVDocVersion :: Forest Token }` would be better.
newtype VDocVersion (state :: HTMLState) = VDocVersion { _unVDocVersion :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

data VDocRepo = VDocRepo
  { _vdocRepoID    :: ID VDocRepo
  , _vdocHeadEdit  :: ID Edit
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Edit = Edit
  { _editID    :: ID Edit
  , _editDesc  :: ST
  , _editRange :: ChunkRange Edit
  }
  deriving (Eq, Ord, Show, Read, Generic)

data CreateEdit = CreateEdit
  { _createEditDesc  :: ST
  , _createEditRange :: CreateChunkRange
  , _createEditVDoc  :: VDocVersion 'HTMLRaw
  }
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
  , _compositeVDocVersion     :: VDocVersion 'HTMLWithMarks
  , _compositeVDocEdits       :: Map (ID Edit) Edit
  , _compositeVDocNotes       :: Map (ID Note) Note
  -- , _compositeVDocQuestions   :: [Question]  -- will be due in #99
  , _compositeVDocDiscussions :: Map (ID Discussion) CompositeDiscussion
  }
  deriving (Eq, Show, Read, Generic)

makeRefineType ''CompositeVDoc
