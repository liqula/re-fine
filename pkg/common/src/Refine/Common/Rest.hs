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
       SListVDocs
  :<|> SGetVDoc
  :<|> SCreateVDoc
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
  :<|> SGetGroups
  :<|> SChangeSubGroup
  :<|> SChangeRole
  :<|> SPutSimpleVoteOnEdit
  :<|> SDeleteSimpleVoteOnEdit
  :<|> SGetSimpleVotesOnEdit


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

type SUpdateEdit
  = "r" :> "updateEdit" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (Create Edit)
    :> Put '[JSON] Edit

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
    :> Post '[JSON] Discussion

type SAddStatement
  = "r" :> "statement" :> "reply" :> Capture "onstatementid" (ID Statement) :> ReqBody '[JSON] (Create Statement)
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
  = "r" :> "group" :> ReqBody '[JSON] (Create Group)
    :> Post '[JSON] Group

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
