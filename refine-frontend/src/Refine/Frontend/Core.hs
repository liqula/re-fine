module Refine.Frontend.Core where

import React.Flux
import React.Flux.Addons.Servant
import Refine.Common.Rest
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc
import Refine.Prelude.TH

import Data.Proxy
import GHC.Generics



data RequestStatus = NoPendingRequest | PendingRequest | PreviousRequestHadError String

newtype UserStore = UserStore
  { reqStatus :: RequestStatus
  }

data UserStoreAction = AskVDoc (ID VDoc)
                     | GotVDoc (ID VDoc) (Either (Int, String) VDoc)
  deriving (Show, Generic)

cfg :: ApiRequestConfig RefineAPI
cfg = ApiRequestConfig "127.0.0.1" NoTimeout

instance StoreData UserStore where
  type StoreAction UserStore = UserStoreAction

  transform (AskVDoc uid) us = do
    request cfg (Proxy :: Proxy GetVDoc) uid $
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
