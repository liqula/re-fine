{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Rest where


import Data.String.Conversions (ST)
import GHC.Generics (Generic)
import Servant.API

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core
import Refine.Common.ChangeAPI
import Refine.Prelude.TH


data ApiError
  = ApiUnknownError ST
  | ApiVDocError [ChunkRangeError]
  | ApiDBError ST
  | ApiDocRepoError ST
  | ApiUserNotFound ST
  | ApiUserNotLoggedIn
  | ApiUserCreationError ST
  | ApiCsrfError ST
  | ApiSessionError
  | ApiSanityCheckError ST
  | ApiUserHandleError ST
  | ApiL10ParseErrors [ST]
  deriving (Eq, Show, Generic)

-- | The 'S' prefix in the handlers stands for "server" (see 'refineApi' for an explanation).
type RefineAPI =
       SListVDocs
  :<|> SGetVDoc
  :<|> SCreateVDoc
  :<|> SAddEdit
  :<|> SAddNote
  :<|> SAddQuestion
  :<|> SAddAnswer
  :<|> SAddDiscussion
  :<|> SAddStatement
  :<|> SCreateUser
  :<|> SChangeAccess
  :<|> SLogin
  :<|> SLogout
  :<|> SGetTranslations
  :<|> SAddGroup
  :<|> SChangeSubGroup
  :<|> SChangeRole


type SListVDocs
  = "r" :> "vdocs"
    :> Get '[JSON] [VDoc]

type SGetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] CompositeVDoc

type SCreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] (Create VDoc)
    :> Post '[JSON] CompositeVDoc

type SAddEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Edit)
    :> Post '[JSON] Edit

type SAddNote
  = "r" :> "note" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Note)
    :> Post '[JSON] Note

type SAddQuestion
  = "r" :> "question" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Question)
    :> Post '[JSON] CompositeQuestion

type SAddAnswer
  = "r" :> "answer" :> Capture "onquestionid" (ID Question) :> ReqBody '[JSON] (Create Answer)
    :> Post '[JSON] Answer

type SAddDiscussion
  = "r" :> "discussion" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Discussion)
    :> Post '[JSON] CompositeDiscussion

type SAddStatement
  = "r" :> "statement" :> "reply" :> Capture "onstatementid" (ID Statement) :> ReqBody '[JSON] (Create Statement)
    :> Post '[JSON] CompositeDiscussion

type SCreateUser
  = "r" :> "user" :> "create" :> ReqBody '[JSON] CreateUser
    :> Post '[JSON] User

type SLogin
  = "r" :> "user" :> "login" :> ReqBody '[JSON] Login
    :> Post '[JSON] Username

type SLogout
  = "r" :> "user" :> "logout"
    :> Post '[JSON] ()

type SChangeAccess
  = "r" :> "access" :> ReqBody '[JSON] ChangeAccess
    :> Post '[JSON] ()

type SGetTranslations
  = "r" :> "get-translations" :> ReqBody '[JSON] GetTranslations
    :> Post '[JSON] L10

type SAddGroup
  = "r" :> "group" :> ReqBody '[JSON] (Create Group)
    :> Post '[JSON] Group

type SChangeSubGroup
  = "r" :> "subgroup" :> ReqBody '[JSON] ChangeSubGroup
    :> Post '[JSON] ()

type SChangeRole
  = "r" :>  "role" :> ReqBody '[JSON] ChangeRole
    :> Post '[JSON] ()

makeRefineType ''ApiError
