{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Refine.Backend.Database.Schema where

import Control.Elim
import Data.Text
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Refine.Common.Types.Chunk
import Refine.Common.Types.Prelude
import Refine.Common.Types.Note (NoteKind)
import Refine.Common.Types.VDoc (Abstract, Title)
import Refine.Backend.Database.Field()
import Refine.Backend.DocRepo.Core(PatchHandle, RepoHandle)


share [mkPersist sqlSettings, mkMigrate "migrateRefine"] [persistLowerCase|
User
    name        Text

VDoc
    title       Title
    desc        Abstract
    repo        RepoId

Patch
    desc        Text
    patchHandle PatchHandle

Repo
    name        Text
    repoHandle  RepoHandle
    headId      PatchId

Comment
    text        Text
    public      Bool
    range       CreateChunkRange
    parent      CommentId Maybe

Note
    text        Text
    kind        NoteKind
    range       CreateChunkRange

Vote
    value       Text
    voter       UserId

-- Connection tables

VR
    vdoc        VDocId
    repository  RepoId
    UniVR vdoc repository
    UniVRV vdoc
    UniVRR repository

RP
    repository  RepoId
    patch       PatchId
    UniRP repository patch

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
makeElim ''RP
makeElim ''PC
makeElim ''PN
makeElim ''PV
