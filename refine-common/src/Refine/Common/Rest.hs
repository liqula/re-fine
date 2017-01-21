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
  :<|> SAddNote
  :<|> SAddPatch


type SListVDocs
  = "r" :> "vdocs"
    :> Get '[JSON] [VDoc]

type SGetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] CompositeVDoc

type SCreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Create VDoc)
    :> Post '[JSON] CompositeVDoc

type SAddComment
  = "r" :> "comment" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Comment)
    :> Post '[JSON] Comment

type SAddNote
  = "r" :> "note" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Note)
    :> Post '[JSON] Note

type SAddPatch
  = "r" :> "patch" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Patch)
    :> Post '[JSON] Patch


-- | Packaged vdoc ready for use by client.
--
-- - morally we have three phases in working on a document: (1) add comments and patches, (2) merge a
--   bunch of patches and (3) create a new version.
--
-- - what follows from this:
--     - there are no patches on patches that we need to display
--     - it's ok to only display patches on head, not on any other version
--     - same for comments: comments collect on head, then then are discarded in (2), (3).
--
-- - if we try to consider comments, patches, ... on other versions than head, we are in trouble.
data CompositeVDoc = CompositeVDoc
  { _compositeVDoc         :: VDoc
  , _compositeVDocRepo     :: VDocRepo
  , _compositeVDocVersion  :: VDocVersion 'HTMLWithMarks
  , _compositeVDocPatches  :: [Patch]
  , _compositeVDocComments :: [Comment]
  , _compositeVDocNotes    :: [Note]
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeRefineType ''CompositeVDoc
