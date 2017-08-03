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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Rest where

import Refine.Frontend.Prelude

import           Refine.Common.Rest
import           Refine.Common.Types


cfg :: HasCallStack => ApiRequestConfig RefineAPI
cfg = ApiRequestConfig "" NoTimeout


-- | look up a vdoc by its auid.
getVDoc :: HasCallStack => ID VDoc -> HandleResponse CompositeVDoc -> IO ()
getVDoc = request cfg (Proxy :: Proxy SGetVDoc)

-- | create a new vdoc.
createVDoc :: HasCallStack => CreateVDoc -> HandleResponse CompositeVDoc -> IO ()
createVDoc = request cfg (Proxy :: Proxy SCreateVDoc)

updateVDoc :: HasCallStack => ID VDoc -> UpdateVDoc -> HandleResponse VDoc -> IO ()
updateVDoc = request cfg (Proxy :: Proxy SUpdateVDoc)

addDiscussion :: HasCallStack => ID Edit -> CreateDiscussion -> HandleResponse Discussion -> IO ()
addDiscussion = request cfg (Proxy :: Proxy SAddDiscussion)

addNote :: HasCallStack => ID Edit -> CreateNote -> HandleResponse Note -> IO ()
addNote = request cfg (Proxy :: Proxy SAddNote)

-- | create a new edit given a base edit and chunk range, new contents.
addEdit :: HasCallStack => ID Edit -> CreateEdit -> HandleResponse Edit -> IO ()
addEdit = request cfg (Proxy :: Proxy SAddEdit)

updateEdit :: HasCallStack => ID Edit -> CreateEdit -> HandleResponse Edit -> IO ()
updateEdit = request cfg (Proxy :: Proxy SUpdateEdit)

createUser :: HasCallStack => CreateUser -> HandleResponse User -> IO ()
createUser = request cfg (Proxy :: Proxy SCreateUser)

login :: HasCallStack => Login -> HandleResponse User -> IO ()
login = request cfg (Proxy :: Proxy SLogin)

logout :: HasCallStack => HandleResponse () -> IO ()
logout = request cfg (Proxy :: Proxy SLogout)

getTranslations :: HasCallStack => GetTranslations -> HandleResponse L10 -> IO ()
getTranslations = request cfg (Proxy :: Proxy SGetTranslations)

sPutSimpleVoteOnEdit :: HasCallStack =>  ID Edit -> Vote -> HandleResponse () -> IO ()
sPutSimpleVoteOnEdit = request cfg (Proxy :: Proxy SPutSimpleVoteOnEdit)

sDeleteSimpleVoteOnEdit :: HasCallStack =>  ID Edit -> HandleResponse () -> IO ()
sDeleteSimpleVoteOnEdit = request cfg (Proxy :: Proxy SDeleteSimpleVoteOnEdit)

sGetSimpleVotesOnEdit :: HasCallStack =>  ID Edit -> HandleResponse VoteCount -> IO ()
sGetSimpleVotesOnEdit = request cfg (Proxy :: Proxy SGetSimpleVotesOnEdit)

getGroups :: HasCallStack => HandleResponse [Group] -> IO ()
getGroups = request cfg (Proxy :: Proxy SGetGroups)

createGroup :: HasCallStack => CreateGroup -> HandleResponse Group -> IO ()
createGroup = request cfg (Proxy :: Proxy SAddGroup)

updateGroup :: HasCallStack => ID Group -> CreateGroup -> HandleResponse Group -> IO ()
updateGroup = request cfg (Proxy :: Proxy SUpdateGroup)
