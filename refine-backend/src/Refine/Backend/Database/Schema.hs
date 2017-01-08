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

-- * helpers

type family EntityRep c = b | b -> c


idToKey :: (ToBackendKey SqlBackend (EntityRep a))
        => ID a -> Key (EntityRep a)
idToKey (ID vid) = toSqlKey vid

-- * eliminators

makeElim ''VDoc
makeElim ''Patch
makeElim ''Repository
makeElim ''Comment
makeElim ''Note
makeElim ''Vote

makeElim ''VR
makeElim ''VP
makeElim ''PC
makeElim ''PN
makeElim ''PV
