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
  | ApiMergeError ST
  | ApiRebaseError
  | ApiSmtpError
  deriving (Eq, Show, Generic)

data ApiErrorCreateUser
  = ApiErrorInvalidPassword
  | ApiErrorUsernameAlreadyTaken
  | ApiErrorEmailAlreadyTaken
  | ApiErrorUsernameAndEmailAlreadyTaken
  deriving (Eq, Show, Generic)

-- | The 'S' prefix in the handlers stands for "server" (see 'refineApi' for an explanation).
--
-- FUTUREWORK: use https://github.com/chpatrick/servant-generic#tldr
type RefineAPI =
       SGetVDoc
  :<|> SCreateVDoc
  :<|> SUpdateVDoc
  :<|> SAddEdit
  :<|> SUpdateEdit
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
  :<|> SUpdateGroup
  :<|> SGetGroups
  :<|> SChangeSubGroup
  :<|> SChangeRole
  :<|> SPutSimpleVoteOnEdit
  :<|> SDeleteSimpleVoteOnEdit
  :<|> SGetSimpleVotesOnEdit


type SGetVDoc
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] CompositeVDoc

type SCreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] CreateVDoc
    :> Post '[JSON] CompositeVDoc

type SUpdateVDoc
  = "r" :> "vdoc" :> Capture "onvdocid" (ID VDoc) :> ReqBody '[JSON] UpdateVDoc
    :> Put '[JSON] VDoc

type SAddEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] CreateEdit
    :> Post '[JSON] Edit

type SUpdateEdit
  = "r" :> "updateEdit" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] CreateEdit
    :> Put '[JSON] Edit

type SAddNote
  = "r" :> "note" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] CreateNote
    :> Post '[JSON] Note

type SAddQuestion
  = "r" :> "question" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] CreateQuestion
    :> Post '[JSON] CompositeQuestion

type SAddAnswer
  = "r" :> "answer" :> Capture "onquestionid" (ID Question) :> ReqBody '[JSON] CreateAnswer
    :> Post '[JSON] Answer

type SAddDiscussion
  = "r" :> "discussion" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] CreateDiscussion
    :> Post '[JSON] Discussion

type SAddStatement
  = "r" :> "statement" :> "reply" :> Capture "onstatementid" (ID Statement) :> ReqBody '[JSON] CreateStatement
    :> Post '[JSON] Discussion
      -- FIXME: should be @"r" :> "statement" :> Capture "onstatementid" (ID Statement) :> "reply" ...@
      -- to be consistent with 'SPutSimpleVoteOnEdit' etc.

type SCreateUser
  = "r" :> "user" :> "create" :> ReqBody '[JSON] CreateUser
    :> Post '[JSON] User

-- | FUTUREWORK: this may be a little simple.  take a look at servant-auth-token and see if that
-- inspires more trust into its security than what we cooked together ourselves here.
type SLogin
  = "r" :> "user" :> "login" :> ReqBody '[JSON] Login
    :> Post '[JSON] User

type SLogout
  = "r" :> "user" :> "logout"
    :> Post '[JSON] ()

type SGetTranslations
  = "r" :> "get-translations" :> ReqBody '[JSON] GetTranslations
    :> Post '[JSON] L10

type SAddGroup
  = "r" :> "group" :> ReqBody '[JSON] CreateGroup
    :> Post '[JSON] Group

type SUpdateGroup
  = "r" :> "updateGroup" :> Capture "ongroupid" (ID Group) :> ReqBody '[JSON] CreateGroup
    :> Put '[JSON] Group

type SGetGroups
  = "r" :> "get-groups"
    :> Get '[JSON] [Group]

type SChangeSubGroup
  = "r" :> "subgroup" :> ReqBody '[JSON] ChangeSubGroup
    :> Post '[JSON] ()

type SChangeRole
  = "r" :>  "role" :> ReqBody '[JSON] ChangeRole
    :> Post '[JSON] ()


type SPutSimpleVoteOnEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> "vote" :> Capture "vote" Vote :> Put '[JSON] ()

-- | delete *my* vote on an edit
type SDeleteSimpleVoteOnEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> "vote" :> Delete '[JSON] ()

-- | get *all* votes on an edit
type SGetSimpleVotesOnEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> "vote" :> Get '[JSON] VoteCount


makeRefineTypes [''ApiError, ''ApiErrorCreateUser]
