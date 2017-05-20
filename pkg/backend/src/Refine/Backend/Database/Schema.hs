{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Backend.Prelude

import Control.Elim
import Data.String.Conversions (ST)
import Data.Text hiding (group)
import Database.Persist.Sql hiding (Statement)
import Web.Users.Persistent (LoginId) -- Same as Refine.Backend.User.LoginId, but that produced an import cycle.
                                      -- "Refine.Backend.User.Types" could be introduced instead.

import Refine.Prelude (Timestamp)
import Refine.Common.Types.Chunk (ChunkRange(..))
import Refine.Common.Types.Prelude hiding (MetaInfo)
import Refine.Common.Types.Process (CollaborativeEditPhase)
import Refine.Common.Types.Role (Role)
import Refine.Common.Types.Core (Abstract, EditKind, Title, VDocVersion)
import Refine.Backend.Database.Field()
import Refine.Backend.Database.Types (MetaInfoID, RawContentEdit)

share [mkPersist sqlSettings, mkMigrate "migrateRefine"] [persistLowerCase|
MetaInfo
    typeTag     MetaInfoID
    createBy    UserInfo
    createAt    Timestamp
    modBy       UserInfo
    modAt       Timestamp
    UniMetaInfo typeTag

VDoc
    title       Title
    desc        Abstract
    headId      EditId Maybe

Edit
    desc        Text
    range       ChunkRange
    editVDoc    VDocVersion
    repository  VDocId
    kind        EditKind

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
    universal   Bool

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
    UniCEPVDoc vdoc

ProcessOfCollabEdit
    process    ProcessId
    collabEdit CollabEditProcessId
    UniPOCE process collabEdit

AulaProcess
    class   ST

ProcessOfAula
    process    ProcessId
    aula       AulaProcessId
    UniPOA process aula

-- Connection tables

ParentChild
    parent EditId
    edit   RawContentEdit
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

makeElim ''MetaInfo
makeElim ''VDoc
makeElim ''Edit
makeElim ''Note
makeElim ''Question
makeElim ''Answer
makeElim ''Discussion
makeElim ''Statement
makeElim ''Vote

makeElim ''Group
makeElim ''SubGroup

makeElim ''Roles

makeElim ''ParentChild
makeElim ''PN
makeElim ''PQ
makeElim ''PD
makeElim ''DS
makeElim ''PV
