{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Route where
#include "import_common.hs"

import qualified Refine.Common.Types as T


data Route
  = Help
  | Login
  | Register
  | Profile (T.ID T.User)
  | Groups
  | GroupCreate
  | GroupProcesses (T.ID T.Group)
  | GroupMembers (T.ID T.Group)
  | GroupUpdate (T.ID T.Group)
  | Process (T.ID T.VDoc)
  deriving (Eq, Show, Generic)

rrender :: Route -> ST
rrender = \case
  Help                      -> "#/help"
  Login                     -> "#/login"
  Register                  -> "#/register"
  Profile (T.ID uid)        -> "#/profile/" <> cs (show uid)
  Groups                    -> "#/groups"
  GroupCreate               -> "#/group/new"
  GroupProcesses (T.ID gid) -> "#/group/" <> cs (show gid) <> "/procs"
  GroupMembers (T.ID gid)   -> "#/group/" <> cs (show gid) <> "/members"
  GroupUpdate (T.ID gid)    -> "#/group/" <> cs (show gid) <> "/update"
  Process (T.ID vid)        -> "#/process/" <> cs (show vid)

newtype RouteParseError = RouteParseError String
  deriving (Eq, Show, Generic)

rparse :: ST -> Either RouteParseError Route
rparse hash = (removeLeadingHash . removeTrailingSlashes $ cs hash) >>= removeLeadingSlash >>= \case
  "help"                                   -> pure   Help
  "login"                                  -> pure   Login
  "register"                               -> pure   Register
  (strip "profile/" -> Just (i, ""))       -> pure . Profile $ T.ID i
  "groups"                                 -> pure   Groups
  "group/new"                              -> pure   GroupCreate
  (strip "group/" -> Just (i, "/procs"))   -> pure . GroupProcesses $ T.ID i
  (strip "group/" -> Just (i, "/members")) -> pure . GroupMembers $ T.ID i
  (strip "group/" -> Just (i, "/update"))  -> pure . GroupUpdate $ T.ID i
  (strip "process/" -> Just (i, ""))       -> pure . Process $ T.ID i
  ""                                       -> pure   Login
  bad                                      -> throwError . RouteParseError $ "could not parse route: " <> show bad
  where
    removeLeadingHash ('#':s) = pure s
    removeLeadingHash bad = throwError . RouteParseError $ "no leading '#': " <> show bad

    removeLeadingSlash ('/':s) = pure s
    removeLeadingSlash bad = throwError . RouteParseError $ "no leading '/': " <> show bad

    removeTrailingSlashes = reverse . dropWhile (== '/') . reverse

    strip s k | take (length s) k == s = case reads $ drop (length s) k of
                  [(a, suff)] -> Just (a, suff)
                  _ -> Nothing
              | otherwise = Nothing
