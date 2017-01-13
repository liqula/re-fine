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

type GetVDocs
  = "r" :> "vdoc"
    :> Get '[JSON] [ID VDoc]

type GetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] VDoc

type PostVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Proto VDoc)
    :> Post '[JSON] (ID VDoc)

type PutTitle
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc) :> "title" :> ReqBody '[JSON] Title
    :> Put '[JSON] ()

type PutAbstract
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc) :> "abstract" :> ReqBody '[JSON] Abstract
    :> Put '[JSON] ()

type DeleteVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Delete '[JSON] VDoc


-- * left ui column (coments, notes, ...)

type PostComment
  = "r" :> "patch" :> Capture "patchid" (ID Patch) :> "comment" :> ReqBody '[JSON] (Proto Comment)
    :> Post '[JSON] (ID Comment)

type DeleteComment
  = "r" :> "comment" :> Capture "commentid" (ID Comment)
    :> Delete '[JSON] Comment

type PostNote
  = "r" :> "patch" :> Capture "patchid" (ID Patch) :> "note" :> ReqBody '[JSON] (Proto Note)
    :> Post '[JSON] (ID Note)

type DeleteNote
  = "r" :> "note" :> Capture "noteid" (ID Note)
    :> Delete '[JSON] Note

type PutNoteVisibility
  = "r" :> "note" :> Capture "noteid" (ID Note) :> QueryParam "visibility" Bool
    :> Put '[JSON] ()


-- * right ui column (patches)

type GetVersion
  = "r" :> "patch" :> Capture "patchid" (ID Patch)
    :> Get '[JSON] (VDocVersion 'HTMLWithMarks)

type PostPatch
  = "r" :> "patch" :> Capture "patchid" (ID Patch) :> ReqBody '[JSON] (Proto Patch)
    :> Post '[JSON] (ID Patch)

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

type PutVote
  = "r" :> "vote" :> Capture "patchid" (ID Patch)
    :> ReqBody '[JSON] (Proto Vote) :> Put '[JSON] (ID Vote)

type DeleteVote
  = "r" :> "vote" :> Capture "voteid" (ID Vote)
    :> Delete '[JSON] Vote
