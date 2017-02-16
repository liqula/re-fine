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
import Refine.Backend.DocRepo.Core (EditHandle, RepoHandle)
import Refine.Backend.User.Core (LoginId)


share [mkPersist sqlSettings, mkMigrate "migrateRefine"] [persistLowerCase|
VDoc
    title       Title
    desc        Abstract
    repo        RepoId

Edit
    desc        Text
    editHandle EditHandle

Repo
    name        Text
    repoHandle  RepoHandle
    headId      EditId

Note
    text        Text
    public      Bool
    range       DBChunkRange

Question
    text        Text
    public      Bool
    answered    Bool
    range       DBChunkRange
    owner       LoginId

Answer
    question    QuestionId
    text        Text

Discussion
    public      Bool
    range       DBChunkRange
    owner       LoginId

Statement
    text        Text
    parent      StatementId Maybe

Vote
    value       Text
    voter       LoginId

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

type family EntityRep c = b | b -> c


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

makeElim ''VR
makeElim ''RP
makeElim ''PN
makeElim ''PC
makeElim ''PQ
makeElim ''PD
makeElim ''DS
makeElim ''PV
