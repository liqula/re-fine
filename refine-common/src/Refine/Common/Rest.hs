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

import Refine.Common.Types


type RefineAPI =
       GetVDocs
  :<|> GetVDoc
  :<|> PostVDoc
  :<|> PutTitle
  :<|> PutAbstract
  :<|> DeleteVDoc
  :<|> PostComment
  :<|> DeleteComment
  :<|> PostNote
  :<|> DeleteNote
  :<|> PutNoteVisibility
  :<|> GetVersion
  :<|> PostPatch
  :<|> DeletePatch
  :<|> MergePatch
  :<|> RebasePatch
  :<|> PutVote
  :<|> DeleteVote


-- * vdocs

type ListVDocs
  = "r" :> "vdocs"
    :> Get '[JSON] [ID VDoc]

type GetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] AugmentedVDoc

type CreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Proto VDoc)
    :> Post '[JSON] AugmentedVDoc

type AddComment
  = "r" :> "comment" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Proto Comment)
    :> Post '[JSON] Comment

type AddPatch
  = "r" :> "patch" :> Capture "onpatchid" (ID Patch) :> ReqBody '[JSON] (Proto Patch)
    :> Post '[JSON] Patch


---------------------------------------------------------------------------
-- For now, the frontend needs the REST calls above, and not the ones below
---------------------------------------------------------------------------

type ChangeTitle
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc) :> "title" :> ReqBody '[JSON] Title
    :> Put '[JSON] ()

type ChangeAbstract
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc) :> "abstract" :> ReqBody '[JSON] Abstract
    :> Put '[JSON] ()

type DeleteVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Delete '[JSON] VDoc


-- * left ui column (coments, notes, ...)

type DeleteComment
  = "r" :> "comment" :> Capture "commentid" (ID Comment)
    :> Delete '[JSON] Comment

type AddNote
  = "r" :> "patch" :> Capture "patchid" (ID Patch) :> "note" :> ReqBody '[JSON] (Proto Note)
    :> Post '[JSON] (ID Note)

type DeleteNote
  = "r" :> "note" :> Capture "noteid" (ID Note)
    :> Delete '[JSON] Note

type ChangeNoteVisibility
  = "r" :> "note" :> Capture "noteid" (ID Note) :> QueryParam "visibility" Bool
    :> Put '[JSON] ()


-- * right ui column (patches)

type GetVersion                                       -- that one is not related to patches?!
  = "r" :> "patch" :> Capture "patchid" (ID Patch)
    :> Get '[JSON] (VDocVersion 'HTMLWithMarks)

type DeletePatch
  = "r" :> "patch" :> Capture "patchid" (ID Patch)
    :> Delete '[JSON] Patch

type MergePatch
  = "r" :> "patch" :> Capture "patchid" (ID Patch) :> "merge"
    :> Post '[JSON] (ID Patch)

type RebasePatch
  = "r" :> "patch" :> "rebase" :> ReqBody '[JSON] ConflictResolution
    :> Post '[JSON] (ID Patch)


-- * votes

type AddVote
  = "r" :> "vote" :> Capture "patchid" (ID Patch)
    :> ReqBody '[JSON] (Proto Vote) :> Put '[JSON] (ID Vote)

type DeleteVote
  = "r" :> "vote" :> Capture "voteid" (ID Vote)
    :> Delete '[JSON] Vote
