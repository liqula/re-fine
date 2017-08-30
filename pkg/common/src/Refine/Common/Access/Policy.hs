{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Access.Policy where
#include "import_common.hs"

import Refine.Common.Access
import Refine.Common.Types


-- * combinators

bottom :: Creds
bottom = CredsNeverAllow

orAdmin :: Creds -> Creds
orAdmin xs = CredsAny (CredsLeaf (CredGlobalRole GlobalAdmin) :| [xs])

groupMember :: ID Group -> Creds
groupMember gid = orAdmin . CredsAny . NEL.fromList $ CredsLeaf . (`CredGroupRole` gid) <$> [GroupMember ..]


-- * group, user

addGroup :: Creds
addGroup = orAdmin bottom

getGroup :: ID Group -> Creds
getGroup = groupMember

updateGroup :: ID Group -> Creds
updateGroup _gid = orAdmin bottom

deleteGroup :: ID Group -> Creds
deleteGroup _gid = orAdmin bottom

createUser :: [GlobalRole] -> [(GroupRole, ID Group)] -> Creds
createUser [] [] = orAdmin $ CredsLeaf CredNotLoggedIn
createUser _ _ = orAdmin bottom


-- * process

createProcessInGroup :: ID Group -> Creds
createProcessInGroup = orAdmin . CredsLeaf . CredGroupRole GroupModerator


-- * vdoc

createOrUpdateVDoc :: ID Group -> Creds
createOrUpdateVDoc = groupMember

viewVDoc :: VDoc -> Creds
viewVDoc = groupMember . (^. vdocGroup)

createOrUpdateComment :: VDoc -> Creds
createOrUpdateComment = groupMember . (^. vdocGroup)

voteOnComment :: VDoc -> Creds
voteOnComment = groupMember . (^. vdocGroup)

updateStatement :: Statement -> Creds
updateStatement stmt = orAdmin . CredsLeaf $ CredUser (stmt ^. statementMetaID . miMeta . metaCreatedBy)

createOrUpdateEdit :: VDoc -> Creds
createOrUpdateEdit = groupMember . (^. vdocGroup)

voteOnEdit :: VDoc -> Creds
voteOnEdit = groupMember . (^. vdocGroup)
