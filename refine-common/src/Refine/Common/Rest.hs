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

module Refine.Common.Rest where

import Servant.API hiding (Patch)
import GHC.Generics (Generic)

import Refine.Common.Types
import Refine.Prelude.TH


-- | The 'S' prefix in the handlers stands for "server" (see 'refineApi' for an explanation).
type RefineAPI =
       SListVDocs
  :<|> SGetVDoc
  :<|> SCreateVDoc
  :<|> SAddComment
  :<|> SAddPatch


type SListVDocs
  = "r" :> "vdocs"
    :> Get '[JSON] [ID VDoc]

type SGetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] HeavyVDoc

type SCreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Create VDoc)
    :> Post '[JSON] HeavyVDoc

type SAddComment
  = "r" :> "comment" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Comment)
    :> Post '[JSON] Comment

type SAddPatch
  = "r" :> "patch" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Patch)
    :> Post '[JSON] Patch


-- | Packaged vdoc ready for use by client.
--
-- TODO: this should go to the module where the application logic using / producing it is
-- implemented. until that implementation exists, it is kept here where it is used.  (also, we're
-- still looking for a name.  canidates are 'AugmentedVDoc', 'VDocWithContext', 'RichVDoc',
-- 'PackagedVDoc', ...?)
--
-- TODO: Rename it to CompositeVDoc
-- As it uses composite information around the VDoc
data HeavyVDoc = HeavyVDoc
  { _heavyVDoc         :: VDoc
  , _heavyVDocVersion  :: VDocVersion 'HTMLWithMarks
  , _heavyVDocPatches  :: [Patch]
  , _heavyVDocComments :: [Comment]
  , _heavyVDocNotes    :: [Note]
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeRefineType ''HeavyVDoc
