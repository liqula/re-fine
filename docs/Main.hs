{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS  #-}

module Main
where

import Control.Applicative
import Data.Data
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Monoid
import Data.String.Conversions
import Data.Typeable
import GHC.Generics
import Prelude hiding ((++))
import Safe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Locale
import System.Process
import System.Random

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT




-- (fill in the CRUD bits and pieces!)


-- * As a process initiator I want to assign roles to groups or persons

data Group = Group  -- not in aula

data Role = Role

{-
data Members = Map (UID (Group | Process)) [UserSet]
data UserSet =
    CopyGroup (UID Group)    -- look up all group members now and invite them
  | LinkGroup (UID Group)    -- at the time of checking auth, lookup group members
  | UserHandle UserHandle    -- explicit list of individual users
  | Authenticated            -- everybody logged in with an account on the system
  | Anonymous                -- everybody, whether logged in or not
-}

data User = User

assignRoleToGroup :: Monad m => Role -> Group -> m ()
assignRoleToGroup = undefined

assignRoleToUser :: Monad m => Role -> User -> m ()
assignRoleToUser = undefined


-- * As a User I want to get notifications at important events.

data Notification = Notification  -- (Event?)

notify :: Monad m => User -> Notification -> m ()
notify = undefined


-- * As a User I want to be able to create a new group.

createGroup :: Monad m => () -> m Group
createGroup = undefined


-- * As a Group Initiator I want to be able to create subgroups.

-- (merge with createGroup?)
createSubGroup :: Monad m => Group -> () -> m Group
createSubGroup = undefined


-- * As a user I want a list with all subgroups of a group.

listSubGroups :: (Monad m, Traversable t) => Group -> m (t Group)
listSubGroups = undefined


-- * As a Group-Initiator I want to be able to create a new process inside a group.

data Process = Process

createProcess :: Monad m => Group -> () -> m Process
createProcess = undefined


-- * As a user I want a list with all processes of a group.

listProcesses :: (Monad m, Traversable t) => Group -> m (t Process)
listProcesses = undefined


-- * As a User I want my own user-space, to have alle my groups and content I'm involved in one
-- list.

-- (`DashBoard`?)
data UserSpace = UserSpace () SearchResult

getUserSpace :: Monad m => User -> m UserSpace
getUserSpace = undefined


-- * As a User I want a full-text-search to search for proposals, groups, comments, documents and
-- other users.  (also sort, filter and search)

-- (see category search for approach with poly-typed result traversable.  fisx thinks we should use
-- that here.)
class FullTextSearch m a where
    fullTextSearch :: Traversable t => ST -> m (t a)
    fullTextSearch = undefined

data Proposal = Proposal

data Comment = Comment

data Document = Document

instance FullTextSearch m Proposal
instance FullTextSearch m Group
instance FullTextSearch m Process  -- ?
instance FullTextSearch m Comment
instance FullTextSearch m Document
instance FullTextSearch m User


-- * As a User I want different possibilities to rate content.

data VoteImportant = VoteImportant          -- aula 'like'
data VoteOffTopic = VoteOffTopic            -- troll notification
data VoteOffensive = VoteOffensive          -- stronger troll notification, sort of
data VoteFavourAgainst = VoteFavourAgainst  -- aula 'vote'

class Vote m target vote where
    vote :: target -> vote -> m ()

class (Vote m target vote) => UnVote m target vote where
    unvote :: target -> vote -> m ()

-- instance Vote ...


-- * As a User I want a change history of all proposals, documents and comments, to be able to
-- comprehend changes.

-- TODO


-- * As a User I want different lists with all edits and comments for each paragraph and the whole
-- document.

-- TODO, see above


-- * As a participant I want to edit several paragraphs of a document and treat them as one edit.

-- TODO, see above


-- * As a Initiator I want global categories on the hole platform, such that it is possible to
-- filter content in multiple groups with the same categories.

-- (we should combine with with FullTextSearch to have one query language that can do both at the
-- same time.)

data Category = Category

data SearchResult = SearchResultComment Comment | SearchResultProposal Proposal

categorySearch :: Category -> m (t SearchResult)
categorySearch = undefined


-- * As a User I want the same functionality to edit text myself and collaboratively for all kinds
-- of text on the Plattform.

-- (this is etherpad)

class CollabEdit m a where
    collabEdit :: a -> m ()
    collabEdit = undefined

-- instance ...


-- * As a participant I want to discuss single paragraphs of a document.

-- (fisx wnats this to be more general.  document can also be Comment.)
data Paragraph = Paragraph Document Int Int

createComment :: Paragraph -> () -> m Comment
createComment = undefined


-- * As a User I want a index for complex documents, to have a quick navigation to the abstract,
-- discussion or edit I'm looking for.

data Index = Index


-- * As a Group-Initiator I want to be able to invite users to a group or a process and give them a
-- specific user-role.

inviteToGroup :: t User -> Group -> Role -> m ()
inviteToGroup = undefined

-- (fisx wants this to merge with inviteToGroup: groups are for hosting users and processes, all
-- users of a group hosting a process can participate in that process.  if necessary, create a
-- trivial group for each process.)
inviteToProcess :: t User -> Process -> Role -> m ()
inviteToProcess = undefined


-- * As a Visitor I want to be able to ask the Initiator of a private group to join the group.

type Visitor = User  -- fisx wants to kill this line.  andorp concurs.  yeay.

data GroupType = Public | Closed | Hidden

private :: GroupType  -- this name must die!
private = Closed

requestJoinGroup :: Visitor -> Group -> m ()
requestJoinGroup = undefined


-- * As a User I want to be able to invite other Users inside and outside of the platform to groups,
-- processes, proposals and discussions.  As a User I want to be able to share groups, processes and
-- content to other Users of the platform and outside.

data Anybody = Somebody User | Nobody Email

type Email = ST

inviteJoinGroup :: t Anybody -> Group -> m ()
inviteJoinGroup = undefined


-- user can be member of a process without being member of the process's home group.  she won't see
-- anything in the home group in this case.

-- processes are a special kind of group.

-- groups have set theory: darmstadt + developers + member of this process.
