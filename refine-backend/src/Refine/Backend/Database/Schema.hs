{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Refine.Backend.Database.Schema where

import Control.Elim
import Data.Text
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Refine.Common.Types.Prelude


share [mkPersist sqlSettings, mkMigrate "migrateRefine"] [persistLowerCase|
User
    name         Text

VDoc
    title        Text  -- TODO: use Title here
    desc         Text  -- TODO: use Abstract here
    repo         RepoId

Patch
    desc        Text
    patchHandle Text  -- TODO: use PatchHandle here.

Repo
    name        Text
    repoHandle  Text  -- TODO: use RepoHandle here.
    headId      PatchId

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

-- Connection tables

VR
    vdoc        VDocId
    repository  RepoId
    UniVR vdoc repository

RC
    repository  RepoId
    commit      PatchId
    UniVC repository commit

CC
    commit      PatchId
    comment     CommentId
    UniCC commit comment

CN
    commit      PatchId
    note        NoteId
    UniCN commit note

CV
    commit      PatchId
    vote        VoteId
    UniPV commit vote
|]


-- * helpers

type family EntityRep c = b | b -> c


idToKey :: (ToBackendKey SqlBackend (EntityRep a))
        => ID a -> Key (EntityRep a)
idToKey = toSqlKey . _unID

keyToId :: (ToBackendKey SqlBackend (EntityRep a))
        => Key (EntityRep a) -> ID a
keyToId = ID . fromSqlKey

-- * eliminators

makeElim ''VDoc
makeElim ''Patch
makeElim ''Repo
makeElim ''Comment
makeElim ''Note
makeElim ''Vote

makeElim ''VR
makeElim ''RC
makeElim ''CC
makeElim ''CN
makeElim ''CV
