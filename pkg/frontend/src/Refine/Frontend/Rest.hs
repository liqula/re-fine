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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Rest where

import           Data.Proxy
import           React.Flux.Addons.Servant
import           Refine.Common.Rest
import           Refine.Common.Types


cfg :: ApiRequestConfig RefineAPI
cfg = ApiRequestConfig "" NoTimeout


-- | obtain a list of all vdocs stored in the backend with auid, title.
listVDocs :: HandleResponse [VDoc] -> IO ()
listVDocs = request cfg (Proxy :: Proxy SListVDocs)

-- | look up a vdoc by its auid.
getVDoc :: ID VDoc -> HandleResponse CompositeVDoc -> IO ()
getVDoc = request cfg (Proxy :: Proxy SGetVDoc)

-- | create a new vdoc.
createVDoc :: Create VDoc -> HandleResponse CompositeVDoc -> IO ()
createVDoc = request cfg (Proxy :: Proxy SCreateVDoc)

addDiscussion :: ID Edit -> Create Discussion -> HandleResponse CompositeDiscussion -> IO ()
addDiscussion = request cfg (Proxy :: Proxy SAddDiscussion)

addNote :: ID Edit -> Create Note -> HandleResponse Note -> IO ()
addNote = request cfg (Proxy :: Proxy SAddNote)

-- | create a new edit given a base edit and chunk range, new contents.
addEdit :: ID Edit -> Create Edit -> HandleResponse Edit -> IO ()
addEdit = request cfg (Proxy :: Proxy SAddEdit)

createUser :: CreateUser -> HandleResponse User -> IO ()
createUser = request cfg (Proxy :: Proxy SCreateUser)

login :: Login -> HandleResponse () -> IO ()
login = request cfg (Proxy :: Proxy SLogin)

logout :: HandleResponse () -> IO ()
logout = request cfg (Proxy :: Proxy SLogout)
