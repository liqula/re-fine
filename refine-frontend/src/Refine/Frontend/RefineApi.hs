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

module Refine.Frontend.RefineApi where

import           Data.Proxy
import           React.Flux.Addons.Servant
import           Refine.Common.Rest
import           Refine.Common.Types


cfg :: ApiRequestConfig RefineAPI
cfg = ApiRequestConfig "" NoTimeout


-- TODO: perhaps we can probably extend @RestAPIEndPoint@ with a class method that is defined in
-- terms of 'request'.

-- | obtain a list of all vdocs stored in the backend with auid, title.
listVDocs :: HandleResponse [ID VDoc] -> IO ()
listVDocs = request cfg (Proxy :: Proxy SListVDocs)

-- | look up a vdoc by its auid.
getVDoc :: ID VDoc -> HandleResponse CompositeVDoc -> IO ()
getVDoc = request cfg (Proxy :: Proxy SGetVDoc)

-- | create a new vdoc.
createVDoc :: Create VDoc -> HandleResponse CompositeVDoc -> IO ()
createVDoc = request cfg (Proxy :: Proxy SCreateVDoc)

addComment :: ID Patch -> Create Comment -> HandleResponse Comment -> IO ()
addComment = request cfg (Proxy :: Proxy SAddComment)

-- | create a new patch given a base patch and chunk range, new contents.
addPatch :: ID Patch -> Create Patch -> HandleResponse Patch -> IO ()
addPatch = request cfg (Proxy :: Proxy SAddPatch)
