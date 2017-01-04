module Refine.Common.Rest where

import Servant.API hiding (Patch)

import Refine.Common.Types


type RefineAPI =
        ListVDocInfos
  :<|>  GetVDoc
  :<|>  AddVDoc
  :<|>  PutTitle
  :<|>  PutAbstract
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
  = "r" :> "vdoc" :> Capture "vdockey" (ID VDoc)  -- TODO: consistency: "vdockey" vs. "vdocid".
    :> Get '[JSON] VDoc

-- TODO: i wonder if the types should always be called Get, Post, Put, or Delete, depending on the
-- http method?
type AddVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Proto VDoc)
    :> Post '[JSON] VDoc

type PutTitle
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc) :> "title"
    :> ReqBody '[JSON] VDocTitle :> Put '[JSON] ()

type PutAbstract
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc) :> "abstract"
    :> ReqBody '[JSON] VDocAbstract :> Put '[JSON] ()

-- TODO: use Put for changing a value, Post for creating a new value, Delete for removing a value.
-- this one is Put.
-- TODO: the new note visibilyt shouldn't be a body type, but a query parameter with a reasonable
-- default.
-- TODO: identifiers of vdocs, notes, and all other objects should be path segments.
-- TODO: avoid "long-path-segments-with-many-dashes".
type NoteVisibility
  = "r" :> "vdoc" :> "change-note-visibility"
    -- :> ReqBody '[JSON] ChangeNoteVisibility
    :> Post '[JSON] ()

type RmVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Delete '[JSON] VDoc

type GetVersion
  = "r" :> "patch" :> Capture "patchkey" (ID Patch)  -- TODO: consistency (like above).
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
  = "r" :> "patch" :> Capture "auid" (ID VDoc) :> "rebase"  -- TODO: "auid" should be "uid"
    :> ReqBody '[JSON] ConflictResolution
    :> Post '[JSON] VDoc  -- not sure: is this creating a new item or changing an old one.  (the
                          -- former would be better, then Post would be correct.)

type AddComment
  = "r" :> "patch" :> Capture "patchkey" (ID Patch) :> "comment"
    :> ReqBody '[JSON] (Proto Comment)
    :> Post '[JSON] Comment

type RmComment
  = "r" :> "comment" :> Capture "patchcommentkey" (ID Comment)  -- TODO: consistency: "commentid", not "patchcommentid".
    :> Delete '[JSON] Comment

type AddNote
  = "r" :> "patch" :> Capture "patchkey" (ID Patch) :> "note"
    :> ReqBody '[JSON] (Proto Note)
    :> Post '[JSON] Note

type RmNote
  = "r" :> "note" :> Capture "patchnotekey" (ID Note)
    :> Delete '[JSON] Note

type SetVote
  = "r" :> "vote" :> Capture "patchkey" (ID Patch)
    :> ReqBody '[JSON] (Proto Vote) :> Post '[JSON] Vote

type RmVote
  = "r" :> "vote" :> Capture "patchvotekey" (ID Vote)  -- TODO: consistency: "voteid".
    :> Delete '[JSON] Vote
