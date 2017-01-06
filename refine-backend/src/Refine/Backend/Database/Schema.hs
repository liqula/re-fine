{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
module Refine.Backend.Database.Schema where

import Data.Text
import Database.Persist.TH



share [mkPersist sqlSettings, mkMigrate "refineMigrate"] [persistLowerCase|
User
    name        Text

VDoc
    title       Text
    description Text

Patch
    description Text
    commitHash  Text

Repository
    hash        Text

Comment
    text        Text
    public      Bool
    parent      CommentId Maybe

Note
    text        Text
    kind        Text

Vote
    value       Text
    voter       UserId

VR
    vdoc        VDocId
    repository  RepositoryId

VP
    vdoc        VDocId
    patch       PatchId
    UniqVP vdoc patch

PC
    patch       PatchId
    comment     CommentId
    UniPC patch comment

PN
    patch       PatchId
    note        NoteId
    UniPN patch note

PV
    patch       PatchId
    vote        VoteId
    UniPV patch vote

|]
