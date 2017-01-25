{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Werror -Wall #-}

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

addDiscussion :: ID Patch -> Create Comment -> HandleResponse Comment -> IO ()
addDiscussion = request cfg (Proxy :: Proxy SAddComment)

-- | create a new edit given a base edit and chunk range, new contents.
addEdit :: ID Patch -> Create Patch -> HandleResponse Patch -> IO ()
addEdit = request cfg (Proxy :: Proxy SAddPatch)
