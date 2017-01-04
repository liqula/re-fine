module Refine.Api.Rest where

import Servant.API hiding (Patch)

import Refine.Common.Types



type RefineAPI =
        ListVDocInfos
  :<|>  GetVDoc
  :<|>  AddVDoc
  :<|>  ChangeTitleAndDesc
  :<|>  NoteVisibility
  :<|>  RmVDoc
  :<|>  GetVersion
  :<|>  AddPatch
  :<|>  RmPatch
  :<|>  MergePatch
  :<|>  RebasePatch
  :<|>  AddComment
  :<|>  RmComment
  :<|>  AddNote
  :<|>  RmNote
  :<|>  SetVote
  :<|>  RmVote


type ListVDocInfos
  = "r" :> "vdoc"
    :> Get '[JSON] [VDocInfo]

type GetVDoc
  = "r" :> "vdoc" :> Capture "vdockey" (ID VDoc)
    :> Get '[JSON] VDoc

type AddVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Proto VDoc)
    :> Post '[JSON] VDoc

type ChangeTitleAndDesc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc) :> "change-title-desc"
    :> ReqBody '[JSON] VDocTitleAndDesc :> Post '[JSON] VDocTitleAndDesc

type NoteVisibility
  = "r" :> "vdoc" :> "change-note-visibility"
    :> ReqBody '[JSON] ChangeNoteVisibility :> Post '[JSON] ()

type RmVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Delete '[JSON] VDoc

type GetVersion
  = "r" :> "patch" :> Capture "patchkey" (ID Patch)
    :> Get '[JSON] Document

type AddPatch
  = "r" :> "patch" :> Capture "patchkey" (ID Patch)
    :> ReqBody '[JSON] PatchFromClient :> Post '[JSON] Patch

type RmPatch
  = "r" :> "patch" :> Capture "patchkey" (ID Patch)
    :> Delete '[JSON] Patch

type MergePatch
  = "r" :> "patch" :> Capture "patchkey" (ID Patch) :> "merge"
    :> Post '[JSON] Patch

type RebasePatch
  = "r" :> "patch" :> Capture "auid" (ID VDoc) :> "rebase"
    :> ReqBody '[JSON] ConflictResolution :> Post '[JSON] VDoc

type AddComment
  = "r" :> "patch" :> Capture "patchkey" (ID Patch) :> "comment"
    :> ReqBody '[JSON] (Proto Comment) :> Post '[JSON] Comment

type RmComment
  = "r" :> "comment" :> Capture "patchcommentkey" (ID Comment)
    :> Delete '[JSON] Comment

type AddNote
  = "r" :> "patch" :> Capture "patchkey" (ID Patch) :> "note"
    :> ReqBody '[JSON] (Proto Note) :> Post '[JSON] Note

type RmNote
  = "r" :> "note" :> Capture "patchnotekey" (ID Note)
    :> Delete '[JSON] Note

type SetVote
  = "r" :> "vote" :> Capture "patchkey" (ID Patch)
    :> ReqBody '[JSON] (Proto Vote) :> Post '[JSON] Vote

type RmVote
  = "r" :> "vote" :> Capture "patchvotekey" (ID Vote)
    :> Delete '[JSON] Vote
