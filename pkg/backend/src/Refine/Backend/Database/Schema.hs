{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Refine.Backend.Database.Schema where

import Refine.Backend.Prelude

import Control.Elim
import Data.Text hiding (group)
import Database.Persist.Sql hiding (Statement)
import Web.Users.Persistent (LoginId) -- Same as Refine.Backend.User.LoginId, but that produced an import cycle.
                                      -- "Refine.Backend.User.Types" could be introduced instead.

import Refine.Prelude (Timestamp)
import Refine.Common.Types.Prelude hiding (MetaInfo)
import Refine.Common.Types.Role (GroupRole, GlobalRole)
import Refine.Common.Types.Core (Abstract, EditKind, Title, RawContent)
import Refine.Backend.Database.Field()
import Refine.Backend.Database.Types

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
    group       GroupId

Edit
    desc        Text
    editVDoc    RawContent
    repository  VDocId
    kind        EditKind
    votes       DBVotes

Discussion
    votes       DBVotes
    isNote      Bool

Statement
    text        Text
    parent      StatementId Maybe
    discussion  DiscussionId

-- Groups

Group
    title       Text
    description Text

SubGroup
    parent      GroupId
    child       GroupId
    UniSG parent child

-- Roles

GroupRoles
    group GroupId
    user  LoginId
    role  GroupRole
    UniGroupRoles group user role

GlobalRoles
    user  LoginId
    role  GlobalRole
    UniGlobalRoles user role

-- Users

DBUser
    avatar       Image Maybe

-- Connection tables

ParentChild
    parent EditId
    edit   RawContentEdit
    child  EditId
    UniPC parent child

PD
    edit       EditId
    discussion DiscussionId
    range      RangePosition
    UniPD edit discussion
|]


-- * helpers

-- | Connect a type defined in the common with a type defined in the database.
type family EntityRep c = b

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
makeElim ''Discussion
makeElim ''Statement

makeElim ''Group
makeElim ''SubGroup

makeElim ''GroupRoles
makeElim ''GlobalRoles

makeElim ''ParentChild
makeElim ''PD

makeElim ''DBUser
