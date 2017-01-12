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
import           Data.String.Conversions (ST)
import           GHC.Generics (Generic)
import           Control.DeepSeq
import qualified Generics.SOP        as SOP
import qualified Generics.SOP.JSON   as SOP
import qualified Generics.SOP.NFData as SOP
import           Refine.Prelude

import Refine.Common.Orphans ()
import Refine.Common.Types.Prelude
import Refine.Prelude.TH


data VDoc = VDoc
  { _vdocId       :: ID VDoc
  , _vdocTitle    :: Title
  , _vdocAbstract :: Abstract
  , _vdocRepo     :: ID VDocRepo
  }
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoVDoc = ProtoVDoc
  { _protoVDocTitle       :: Title
  , _protoVDocAbstract    :: Abstract
  , _protoVDocInitVersion :: VDocVersion 'HTMLCanonical
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
  , _vdocHeadPatch :: ID Patch
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Patch = Patch
  { _patchId   :: ID Patch
  , _patchDesc :: ST
  }
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoPatch = ProtoPatch
  deriving (Eq, Ord, Show, Read, Generic)

data ConflictResolution = ConflictResolution
  deriving (Eq, Ord, Show, Read, Generic)


-- * protos, instances

type instance Proto VDoc  = ProtoVDoc
type instance Proto Patch = ProtoPatch

-- * refine types

makeRefineType ''VDoc
makeRefineType ''ProtoVDoc
makeRefineType ''VDocRepo
makeRefineType ''Patch
makeRefineType ''ProtoPatch
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
