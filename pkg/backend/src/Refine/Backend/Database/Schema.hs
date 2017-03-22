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
import Data.String.Conversions (ST)
import Data.Text hiding (group)
import Database.Persist
import Database.Persist.Sql hiding (Statement)
import Database.Persist.TH
import Web.Users.Persistent (LoginId) -- Same as Refine.Backend.User.LoginId, but that produced an import cycle.
                                      -- "Refine.Backend.User.Types" could be introduced instead.

import Refine.Common.Types.Chunk (ChunkRange(..))
import Refine.Common.Types.Prelude
import Refine.Common.Types.Process (CollaborativeEditPhase)
import Refine.Common.Types.Role (Role)
import Refine.Common.Types.VDoc (Abstract, EditKind, Title)
import Refine.Backend.Database.Field()
import Refine.Backend.DocRepo.Core (EditHandle, RepoHandle)



share [mkPersist sqlSettings, mkMigrate "migrateRefine"] [persistLowerCase|
VDoc
    title       Title
    desc        Abstract
    repo        RepoId

Edit
    desc        Text
    range       ChunkRange
    editHandle  EditHandle
    kind        EditKind
    motivation  ST

Repo
    name        Text
    repoHandle  RepoHandle
    headId      EditId

Note
    text        Text
    public      Bool
    range       ChunkRange
    owner       LoginId

Question
    text        Text
    public      Bool
    answered    Bool
    range       ChunkRange
    owner       LoginId

Answer
    question    QuestionId
    text        Text

Discussion
    public      Bool
    range       ChunkRange
    owner       LoginId

Statement
    text        Text
    parent      StatementId Maybe

Vote
    value       Text
    owner       LoginId

-- Groups

Group
    title       Text
    description Text

SubGroup
    parent      GroupId
    child       GroupId
    UniSG parent child

-- Roles

Roles
    group GroupId
    user  LoginId
    role  Role
    UniRoles group user role

-- Processes

Process
    group   GroupId

CollabEditProcess
    vdoc    VDocId
    phase   CollaborativeEditPhase

ProcessOfCollabEdit
    process    ProcessId
    collabEdit CollabEditProcessId

AulaProcess
    class   ST

ProcessOfAula
    process    ProcessId
    aula       AulaProcessId

-- Connection tables

VR
    vdoc        VDocId
    repository  RepoId
    UniVR vdoc repository
    UniVRV vdoc
    UniVRR repository

RP  -- TODO: should be RE
    repository  RepoId
    edit       EditId
    UniRP repository edit

PC
    parent EditId
    child  EditId
    UniPC parent child

PQ
    edit       EditId
    question    QuestionId
    UniPQ edit question

PD
    edit       EditId
    discussion  DiscussionId
    UniPD edit discussion

DS
    discussion  DiscussionId
    statement   StatementId
    UniDS discussion statement

PN
    edit       EditId
    note        NoteId
    UniPN edit note

PV
    edit       EditId
    vote        VoteId
    UniPV edit vote
|]


-- * helpers

-- | Connect a type defined in the common with a type defined in the database.
type family EntityRep c = b

-- | Connect a process data type with its representation in the database.
type family ProcessDataRep c = b

-- | Defines the connection type for a given process data type.
type family ProcessDataConnectionRep c = b

idToKey :: (ToBackendKey SqlBackend (EntityRep a))
        => ID a -> Key (EntityRep a)
idToKey = toSqlKey . _unID

keyToId :: (ToBackendKey SqlBackend (EntityRep a))
        => Key (EntityRep a) -> ID a
keyToId = ID . fromSqlKey

-- * eliminators

makeElim ''VDoc
makeElim ''Edit
makeElim ''Repo
makeElim ''Note
makeElim ''Question
makeElim ''Answer
makeElim ''Discussion
makeElim ''Statement
makeElim ''Vote

makeElim ''Group
makeElim ''SubGroup

makeElim ''Roles

makeElim ''VR
makeElim ''RP
makeElim ''PN
makeElim ''PC
makeElim ''PQ
makeElim ''PD
makeElim ''DS
makeElim ''PV
