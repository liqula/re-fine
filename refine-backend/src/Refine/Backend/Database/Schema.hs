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
import Refine.Common.Prelude



share [mkPersist sqlSettings, mkMigrate "migrateRefine"] [persistLowerCase|
User
    name        Text

VDoc
    title       Text
    description Text
    repository  RepositoryId

Commit
    description Text
    commitHash  Text

Repository
    name        Text
    repoId      Text
    headId      CommitId

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
    repository  RepositoryId
    UniVR vdoc repository

RC
    repository  RepositoryId
    commit      CommitId
    UniVC repository commit

CC
    commit      CommitId
    comment     CommentId
    UniCC commit comment

CN
    commit      CommitId
    note        NoteId
    UniCN commit note

CV
    commit      CommitId
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
makeElim ''Commit
makeElim ''Repository
makeElim ''Comment
makeElim ''Note
makeElim ''Vote

makeElim ''VR
makeElim ''RC
makeElim ''CC
makeElim ''CN
makeElim ''CV
