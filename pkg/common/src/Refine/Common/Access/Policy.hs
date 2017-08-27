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


-- * group, user

addGroup :: Creds
addGroup = orAdmin bottom

getGroup :: ID Group -> Creds
getGroup gid = orAdmin . CredsLeaf $ CredGroupRole GroupMember gid

updateGroup :: ID Group -> Creds
updateGroup _gid = orAdmin bottom

deleteGroup :: ID Group -> Creds
deleteGroup _gid = orAdmin bottom

createUser :: [GlobalRole] -> [(GroupRole, ID Group)] -> Creds
createUser [] [] = orAdmin $ CredsLeaf CredNotLoggedIn
createUser _ _ = orAdmin CredsNeverAllow


-- * process

createProcessInGroup :: ID Group -> Creds
createProcessInGroup = orAdmin . CredsLeaf . CredGroupRole GroupModerator


-- * vdoc

createVDoc :: ID Group -> Creds
createVDoc = orAdmin . CredsLeaf . CredGroupRole GroupMember

viewVDoc :: VDoc -> Creds
viewVDoc = createEdit

createComment :: VDoc -> Creds
createComment = createEdit

updateStatement :: Statement -> Creds
updateStatement stmt = orAdmin . CredsLeaf $ CredUser (stmt ^. statementMetaID . miMeta . metaCreatedBy)

createEdit :: VDoc -> Creds
createEdit vdoc = orAdmin . CredsAny . NEL.fromList $ CredsLeaf . (`CredGroupRole` (vdoc ^. vdocGroup)) <$> [GroupMember ..]
