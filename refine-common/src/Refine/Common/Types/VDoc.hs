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

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

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
  , _protoVDocInitVersion :: VDocVersion
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Title = Title { _unTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Abstract = Abstract { _unAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

-- TODO: this change would be nice, but it breaks makeRefineType:
-- >> data HTMLState = HTMLRaw | HTMLCanonical | HTMLWithMarks
-- >> newtype VDocVersion (state :: HTMLState) = ...

-- TODO: `newtype VDocVersion = VDocVersion { _unVDocVersion :: Forest Token }` would be better than
-- with `ST`.

newtype VDocVersion = VDocVersion { _unVDocVersion :: ST }
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
makeRefineType ''VDocVersion
