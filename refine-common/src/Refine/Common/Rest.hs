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
  :<|> SAddPatch
  :<|> SAddNote
  :<|> SAddQuestion
  :<|> SAddAnswer
  :<|> SAddDiscussion
  :<|> SAddStatemment
  :<|> SAddReplyStatement


type SListVDocs
  = "r" :> "vdocs"
    :> Get '[JSON] [VDoc]

type SGetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] CompositeVDoc

type SCreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Create VDoc)
    :> Post '[JSON] CompositeVDoc

type SAddPatch
  = "r" :> "patch" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Patch)
    :> Post '[JSON] Patch

type SAddNote
  = "r" :> "note" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Note)
    :> Post '[JSON] Note

type SAddQuestion
  = "r" :> "question" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Question)
    :> Post '[JSON] CompositeQuestion

type SAddAnswer
  = "r" :> "answer" :> Capture "onquestionid" (ID Question) :> ReqBody '[JSON] (Create Answer)
    :> Post '[JSON] Answer

type SAddDiscussion
  = "r" :> "discussion" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Create Discussion)
    :> Post '[JSON] CompositeDiscussion

type SAddStatemment
  = "r" :> "statement" :> Capture "ondiscussionid" (ID Discussion) :> ReqBody '[JSON] (Create Statement)
    :> Post '[JSON] Statement

type SAddReplyStatement
  = "r" :> "statement" :> "reply" :> Capture "onstatementid" (ID Statement) :> ReqBody '[JSON] (Create Statement)
    :> Post '[JSON] Statement

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
  }
  deriving (Eq, Show, Read, Generic)

makeRefineType ''CompositeVDoc
