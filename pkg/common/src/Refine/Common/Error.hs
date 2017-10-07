{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Error where
#include "import_common.hs"


-- | FUTUREWORK: this is used both for Rest or WebSockets; more to a more appropriate module.
data ApiError
  = ApiUnknownError
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
  | ApiTimeoutError Timespan
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

makeRefineTypes [''ApiError, ''ApiErrorDB, ''ApiErrorCreateUser]
