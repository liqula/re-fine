{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Access.Policy where

import Refine.Common.Prelude

import Refine.Common.Access
import Refine.Common.Types


-- * combinators

bottom :: Creds
bottom = CredsNeverAllow

orAdmin :: Creds -> Creds
orAdmin xs = CredsAny (CredsLeaf (CredGlobalRole GlobalAdmin) :| [xs])


-- * group

addGroup :: Creds
addGroup = orAdmin bottom

getGroup :: ID Group -> Creds
getGroup gid = orAdmin . CredsLeaf $ CredGroupRole GroupMember gid

updateGroup :: ID Group -> Creds
updateGroup _gid = orAdmin bottom

deleteGroup :: ID Group -> Creds
deleteGroup _gid = orAdmin bottom


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
createEdit vdoc = orAdmin . credsAny $ (flip CredGroupRole (vdoc ^. vdocGroup) <$> [GroupMember ..])
