{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Core where

import Data.Proxy
import GHC.Generics
import React.Flux
import React.Flux.Addons.Servant

import Refine.Common.Rest
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc
import Refine.Prelude.TH



data RequestStatus = NoPendingRequest | PendingRequest | PreviousRequestHadError String

newtype UserStore = UserStore
  { reqStatus :: RequestStatus
  }

data UserStoreAction = AskVDoc (ID VDoc)
                     | GotVDoc (ID VDoc) (Either (Int, String) CompositeVDoc)
  deriving (Show, Generic)

cfg :: ApiRequestConfig RefineAPI
cfg = ApiRequestConfig "127.0.0.1" NoTimeout

instance StoreData UserStore where
  type StoreAction UserStore = UserStoreAction

  transform (AskVDoc uid) us = do
    request cfg (Proxy :: Proxy SGetVDoc) uid $
      \r -> return [SomeStoreAction userStore $ GotVDoc uid r]
    return $ us {reqStatus = PendingRequest}

  transform (GotVDoc _ (Left (_errCode, err))) us = do
    return $ us {reqStatus = PreviousRequestHadError err}

  transform (GotVDoc _ (Right _vdoc)) us = do
    return $ us {reqStatus = NoPendingRequest}

userStore :: ReactStore UserStore
userStore = mkStore $ UserStore NoPendingRequest

-- * refine type

makeRefineType ''UserStoreAction
