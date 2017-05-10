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

import           Control.Lens (Lens')
import           Data.Map as Map
import           Data.String.Conversions (ST)
import           GHC.Generics (Generic)

import Refine.Common.Orphans ()
import Refine.Common.Types.Chunk
import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Prelude.TH (makeRefineType)


data VDoc = VDoc
  { _vdocMetaID   :: MetaID VDoc
  , _vdocTitle    :: Title
  , _vdocAbstract :: Abstract
  , _vdocHeadEdit :: ID Edit
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

data Edit = Edit
  { _editMetaID :: MetaID Edit
  , _editDesc   :: ST
  , _editRange  :: ChunkRange
  , _editKind   :: EditKind
  , _editMotiv  :: ST  -- (list of paragraphs)
  }
  deriving (Eq, Ord, Show, Read, Generic)

data EditSource a =
    InitialEdit
  | EditOfEdit (){-FIXME: OT.Edit VDocVersion-} a
  | MergeOfEdits a a
  deriving (Eq, Ord, Show, Read, Generic, Functor)

data CreateEdit = CreateEdit
  { _createEditDesc  :: ST
  , _createEditRange :: ChunkRange
  , _createEditVDoc  :: VDocVersion
-- FIXME: add:  , _createEditSource :: EditSource (ID Edit)
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
makeRefineType ''Edit
makeRefineType ''CreateEdit
makeRefineType ''EditKind
makeRefineType ''ConflictResolution
makeRefineType ''Title
makeRefineType ''Abstract
makeRefineType ''VDocVersion

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
