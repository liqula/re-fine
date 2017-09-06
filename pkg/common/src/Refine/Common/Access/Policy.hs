{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Access.Policy
  ( addGroup
  , getGroup
  , updateGroup
  , deleteGroup
  , createUser
  , getUser
  , updateUser
  , createProcessInGroup
  , createOrUpdateVDoc
  , viewVDoc
  , createOrUpdateComment
  , voteOnComment
  , updateStatement
  , createOrUpdateEdit
  , voteOnEdit
  , getTranslations
  , login
  , logout
  , currentUser
  ) where
#include "import_common.hs"

import Refine.Common.Access
import Refine.Common.Types


-- * combinators (not suitable for export)

orAdmin :: Creds -> Creds
orAdmin xs = CredsAny (CredsLeaf (CredGlobalRole GlobalAdmin) :| [xs])

groupMember :: ID Group -> Creds
groupMember gid = CredsAny . NEL.fromList $ CredsLeaf . (`CredGroupRole` gid) <$> [GroupMember ..]


-- * group, user

addGroup :: Creds
addGroup = orAdmin CredsNeverAllow

getGroup :: ID Group -> Creds
getGroup = orAdmin . groupMember

updateGroup :: ID Group -> Creds
updateGroup _gid = orAdmin CredsNeverAllow

deleteGroup :: ID Group -> Creds
deleteGroup _gid = orAdmin CredsNeverAllow

createUser :: [GlobalRole] -> [(GroupRole, ID Group)] -> Creds
createUser [] [] = orAdmin $ CredsLeaf CredNotLoggedIn
createUser _ _ = orAdmin CredsNeverAllow

-- | At least one of the following conditions needs to hold: 1) client has admin; 2) client is the
-- target user; 3) client shares a group with the target user.
getUser :: User -> [ID Group] -> Creds
getUser user gids = CredsAny
   $ (CredsLeaf . CredUser . UserID $ user ^. userID)
  :| (CredsLeaf (CredGlobalRole GlobalAdmin)
      : (groupMember <$> gids))

updateUser :: ID User -> Creds
updateUser uid = CredsAny
   $ (CredsLeaf . CredUser $ UserID uid)
  :| (CredsLeaf (CredGlobalRole GlobalAdmin)
      : [])


-- * process

createProcessInGroup :: ID Group -> Creds
createProcessInGroup = orAdmin . CredsLeaf . CredGroupRole GroupModerator


-- * vdoc

createOrUpdateVDoc :: ID Group -> Creds
createOrUpdateVDoc = orAdmin . groupMember

viewVDoc :: VDoc -> Creds
viewVDoc = orAdmin . groupMember . (^. vdocGroup)

createOrUpdateComment :: VDoc -> Creds
createOrUpdateComment = orAdmin . groupMember . (^. vdocGroup)

voteOnComment :: VDoc -> Creds
voteOnComment = orAdmin . groupMember . (^. vdocGroup)

updateStatement :: Statement -> Creds
updateStatement stmt = orAdmin . CredsLeaf $ CredUser (stmt ^. statementMetaID . miMeta . metaCreatedBy)

createOrUpdateEdit :: VDoc -> Creds
createOrUpdateEdit = orAdmin . groupMember . (^. vdocGroup)

voteOnEdit :: VDoc -> Creds
voteOnEdit = orAdmin . groupMember . (^. vdocGroup)


-- * other

getTranslations :: Creds
getTranslations = CredsAlwaysAllow

login :: Creds
login = CredsAlwaysAllow

logout :: Creds
logout = CredsAlwaysAllow

currentUser :: Creds
currentUser = CredsAlwaysAllow
