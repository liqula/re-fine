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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Rest where


import Refine.Common.Prelude

import Refine.Common.Types
import Refine.Common.ChangeAPI


data ApiError
  = ApiUnknownError ST
  | ApiVDocVersionError
  | ApiDBError ST
  | ApiDocRepoError ST
  | ApiUserNotFound ST
  | ApiUserNotLoggedIn
  | ApiUserCreationError ApiErrorCreateUser
  | ApiCsrfError ST
  | ApiSessionError
  | ApiSanityCheckError ST
  | ApiUserHandleError ST
  | ApiL10ParseErrors [ST]
  | ApiUnauthorized
  deriving (Eq, Show, Generic)

data ApiErrorCreateUser
  = ApiErrorInvalidPassword
  | ApiErrorUsernameAlreadyTaken
  | ApiErrorEmailAlreadyTaken
  | ApiErrorUsernameAndEmailAlreadyTaken
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
  :<|> SLogin
  :<|> SLogout
  :<|> SGetTranslations
  :<|> SAddGroup
  :<|> SChangeSubGroup
  :<|> SChangeRole
  :<|> SAddProcess
  :<|> SChangeProcess
  :<|> SRemoveProcess


type SListVDocs
  = "r" :> "vdocs"
    :> Get '[JSON] [VDoc]

type SGetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] CompositeVDoc

type SCreateVDoc  -- FIXME: remove this, vdocs should only be created inside CollabEditProcesses.
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
      -- FIXME: should be @"r" :> "statement" :> Capture "onstatementid" (ID Statement) :> "reply" ...@
      -- to be consistent with 'SSimpleVoteOnStatement', see #277.

type SCreateUser
  = "r" :> "user" :> "create" :> ReqBody '[JSON] CreateUser
    :> Post '[JSON] User

-- | FUTUREWORK: this may be a little simple.  take a look at servant-auth-token and see if that
-- inspires more trust into its security than what we cooked together ourselves here.
type SLogin
  = "r" :> "user" :> "login" :> ReqBody '[JSON] Login
    :> Post '[JSON] Username

type SLogout
  = "r" :> "user" :> "logout"
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

type SAddProcess
  = "r" :> "process" :> ReqBody '[JSON] AddProcess
    :> Post '[JSON] CreatedProcess

type SChangeProcess
  = "r" :> "process" :> "change" :> ReqBody '[JSON] ChangeProcess
    :> Post '[JSON] ()

type SRemoveProcess
  = "r" :> "process" :> "remove" :> ReqBody '[JSON] RemoveProcess
    :> Post '[JSON] ()

makeRefineType ''ApiError
makeRefineType ''ApiErrorCreateUser
