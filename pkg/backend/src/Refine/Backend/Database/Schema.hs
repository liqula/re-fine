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
import Database.Persist.Sql hiding (Statement)
import Database.Persist.TH

import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc (Abstract, Title)
import Refine.Backend.Database.Field()
import Refine.Backend.Database.Types
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

Note
    text        Text
    public      Bool
    range       DBChunkRange

Question
    text        Text
    public      Bool
    answered    Bool
    range       DBChunkRange

Answer
    question    QuestionId
    text        Text

Discussion
    public      Bool
    range       DBChunkRange

Statement
    text        Text
    parent      StatementId Maybe

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

PQ
    patch       PatchId
    question    QuestionId
    UniPQ patch question

PD
    patch       PatchId
    discussion  DiscussionId
    UniPD patch discussion

DS
    discussion  DiscussionId
    statement   StatementId
    UniDS discussion statement

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
makeElim ''Note
makeElim ''Question
makeElim ''Answer
makeElim ''Discussion
makeElim ''Statement
makeElim ''Vote

makeElim ''VR
makeElim ''RP
makeElim ''PN
makeElim ''PQ
makeElim ''PD
makeElim ''DS
makeElim ''PV
