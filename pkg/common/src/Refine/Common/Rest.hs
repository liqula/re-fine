{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Rest where
#include "import_common.hs"

import Refine.Common.ChangeAPI
import Refine.Common.Types.Core
import Refine.Common.Types.Prelude
import Refine.Common.Types.Translation
import Refine.Common.Types.Vote

-- | FUTUREWORK: this is used both for Rest or WebSockets; more to a more appropriate module.
data ApiError
  = ApiUnknownError ST
  | ApiVDocVersionError
  | ApiDBError ApiErrorDB
  | ApiUserNotFound ST
  | ApiUserNotLoggedIn
  | ApiUserCreationError ApiErrorCreateUser
  | ApiCsrfError ST
  | ApiSessionInvalid
  | ApiSanityCheckError ST
  | ApiL10ParseErrors [ST]
  | ApiUnauthorized ST
  | ApiMergeError ST
  | ApiRebaseError
  | ApiSmtpError
  deriving (Eq, Show, Generic)

data ApiErrorDB
  = ApiDBUnknownError String
  | ApiDBNotFound String
  | ApiDBNotUnique String
  | ApiDBException String
  | ApiDBUserNotLoggedIn
  | ApiDBMigrationParseErrors
  | ApiDBUnsafeMigration
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
       SGetVDocSimple
  :<|> SCreateVDoc
  :<|> SUpdateVDoc
  :<|> SAddEdit
  :<|> SGetEdit
  :<|> SMergeEdit
  :<|> SUpdateEdit
  :<|> SAddDiscussion
  :<|> SGetDiscussion
  :<|> SAddStatement
  :<|> SUpdateStatement
  :<|> SCreateUser
  :<|> SGetUser
  :<|> SLogin
  :<|> SLogout
  :<|> SGetTranslations
  :<|> SAddGroup
  :<|> SGetGroup
  :<|> SUpdateGroup
  :<|> SGetGroups
  :<|> SChangeSubGroup
  :<|> SChangeRole
  :<|> SPutSimpleVoteOnEdit
  :<|> SDeleteSimpleVoteOnEdit
  :<|> SGetSimpleVotesOnEdit


type SGetVDocSimple
  = "r" :> "vdoc" :> Capture "vdocid" (ID VDoc)
    :> Get '[JSON] VDoc

type SCreateVDoc
  = "r" :> "vdoc" :> ReqBody '[JSON] CreateVDoc
    :> Post '[JSON] VDoc

type SUpdateVDoc
  = "r" :> "vdoc" :> Capture "onvdocid" (ID VDoc) :> ReqBody '[JSON] UpdateVDoc
    :> Put '[JSON] VDoc

type SAddEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] CreateEdit
    :> Post '[JSON] Edit

type SGetEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit)
    :> Get '[JSON] Edit

type SMergeEdit
  = "r" :> "edit" :> Capture "oneditid" (ID Edit) :> "merge"
    :> Post '[JSON] ()

type SUpdateEdit
  = "r" :> "updateEdit" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] CreateEdit
    :> Put '[JSON] Edit

type SAddDiscussion
  = "r" :> "discussion" :> Capture "oneditid" (ID Edit) :> ReqBody '[JSON] (CreateDiscussion (Maybe (Range Position)))
    :> Post '[JSON] Discussion

type SGetDiscussion
  = "r" :> "discussion" :> Capture "ondiscussionid" (ID Discussion)
    :> Get '[JSON] Discussion

type SAddStatement
  = "r" :> "statement" :> "reply" :> Capture "onstatementid" (ID Statement) :> ReqBody '[JSON] CreateStatement
    :> Post '[JSON] Discussion
      -- FIXME: should be @"r" :> "statement" :> Capture "onstatementid" (ID Statement) :> "reply" ...@
      -- to be consistent with 'SPutSimpleVoteOnEdit' etc.

type SUpdateStatement
  = "r" :> "statement" :> "update" :> Capture "onstatementid" (ID Statement) :> ReqBody '[JSON] CreateStatement
    :> Post '[JSON] Discussion

type SCreateUser
  = "r" :> "user" :> "create" :> ReqBody '[JSON] CreateUser
    :> Post '[JSON] User

type SGetUser
  = "r" :> "user" :> Capture "onuserid" (ID User)
    :> Get '[JSON] User

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

type SGetGroup
  = "r" :> "group" :> Capture "ongroupid" (ID Group)
    :> Get '[JSON] Group

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


makeRefineTypes [''ApiError, ''ApiErrorDB, ''ApiErrorCreateUser]
