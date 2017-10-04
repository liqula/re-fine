{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | FIXME: is this module redundant to the users package?  should we make better use of the latter?
module Refine.Backend.App.User
  ( login
  , currentUser
  , logout
  , createUserWith
  , createUser
  , getUser
  , getUsers
  , doesUserExist
  , withCurrentUser
  , updateUser
  , verifyAppState
  ) where
#include "import_backend.hs"

import qualified Web.Users.Types as Users

import Refine.Backend.App.Access
import Refine.Backend.App.Core
import Refine.Backend.App.Session
import Refine.Backend.App.Smtp
import Refine.Backend.App.Role
import Refine.Backend.Config
import Refine.Backend.Types
import Refine.Backend.Database.Class (Database, createMetaID_, getMetaID, insertDBUser, replaceDBUser, getDBUser)
import qualified Refine.Backend.Database.Class as DB
import Refine.Backend.Database.Entity (toUserID, fromUserID)
import qualified Refine.Common.Access.Policy as AP
import qualified Refine.Common.Access as AP
import Refine.Common.Types as Common
import Refine.Prelude (nothingToError, leftToError, timespanToNominalDiffTime)


-- Username is returned after login. This turnes implicit user handling
-- to explicit one. The frontend code should use the returned username.
-- The rational here: It helps the future integration of different login
-- providers.
login :: Login -> App User
login (Login username (Users.PasswordPlain -> password)) = do
  appLog LogDebug "login"
  appLog LogDebug . ("login.before:: " <>) . show =<< get
  assertCreds AP.login
  sessionDuration <- asks . view $ appConfig . cfgSessionLength . to timespanToNominalDiffTime
  session <- nothingToError (AppUserNotFound username)
             =<< dbUsersCmd (\db_ -> Users.authUser db_ username password sessionDuration)
  loginId <- nothingToError (AppUnknownError "session did not validate after successful login")
             =<< dbUsersCmd (\db_ -> Users.verifySession db_ session 0)
  void $ setUserSession (toUserID loginId) (UserSession session)
  dbUser <- getUser $ toUserID loginId
  appLog LogDebug . ("login.after:: " <>) . show =<< get
  pure dbUser

-- | Returns (Just (current ID)) of the current user if the user
-- is logged in otherwise Nothing.
currentUser :: App (Maybe (ID User))
currentUser = do
  assertCreds AP.currentUser
  st <- gets (view appUserState)
  pure $ case st of
    UserLoggedIn user _session -> Just user
    UserLoggedOut              -> Nothing

logout :: App ()
logout = do
  appLog LogDebug "logout"
  appLog LogDebug . ("logout.before:: " <>) . show =<< get
  assertCreds AP.logout
  st <- gets (view appUserState)
  case st of
    UserLoggedIn _user session -> do
      void . dbUsersCmd $ \db_ -> Users.destroySession db_ (session ^. unUserSession)
      clearUserSession
    UserLoggedOut -> do
      pure ()
  appLog LogDebug . ("logout.after:: " <>) . show =<< get

createUserWith :: [GlobalRole] -> [(GroupRole, ID Group)] -> CreateUser -> App User
createUserWith globalRoles groupRoles (CreateUser name email password avatar desc) = do
  appLog LogDebug "createUser"
  assertCreds $ AP.createUser globalRoles groupRoles
  let user = Users.User
              { Users.u_name     = name
              , Users.u_email    = email
              , Users.u_password = Users.makePassword (Users.PasswordPlain password)
              , Users.u_active   = True
              }
  loginId <- leftToError AppUserCreationError
             =<< dbUsersCmd (`Users.createUser` user)
  result <- User <$> db (createMetaID_ $ toUserID loginId) <*> pure name <*> pure email <*> pure avatar <*> pure desc

  let uid = result ^. userID
  db $ insertDBUser uid (avatar, desc)

  (`assignGlobalRole` uid) `mapM_` globalRoles
  (\(r, g) -> assignGroupRole r uid g) `mapM_` groupRoles

  sendMailTo $ EmailMessage result "you have a re-fine account now!" "congratulations (:"
  invalidateCaches $ Set.fromList [CacheKeyUserIds]
  pure result

createUser :: CreateUser -> App User
createUser = createUserWith [] []

getUser :: ID User -> App User
getUser uid = do
  appLog LogDebug "getUser"
  nothingToError (AppUserNotFound . cs . show $ uid) =<< getUserIfAllowedAndExists uid

getUserIfAllowedAndExists :: ID User -> App (Maybe User)
getUserIfAllowedAndExists uid = do
  muser_ <- dbUsersCmd (`Users.getUserById` fromUserID uid)
  case muser_ of
    Nothing -> pure Nothing
    Just user_ -> do
      mid <- db $ getMetaID uid
      (avatar, desc) <- db $ getDBUser uid
      let user = User mid (Users.u_name user_) (Users.u_email user_) avatar desc
      ok <- AP.hasCreds . AP.getUser user . fmap snd . filter ((>= GroupMember) . fst) =<< db (DB.getGroupRoles uid)
      pure $ if ok then Just user else Nothing

-- | TUNING: this is probably quite sub-optimally fast.
getUsers :: App (Set (ID User))
getUsers = do
  appLog LogDebug "getUsers"
  users_ <- dbUsersCmd (\b -> Users.listUsers b Nothing (Users.SortAsc Users.UserFieldId))
  users :: [Maybe User] <- getUserIfAllowedAndExists `mapM` (toUserID . fst <$> users_)
  pure . Set.fromList . fmap (^. userID) . catMaybes $ users

doesUserExist :: ID User -> App Bool
doesUserExist uid = do
  appLog LogDebug "doesUserExist"
  isJust <$> getUserIfAllowedAndExists uid

withCurrentUser :: MonadApp app => (ID User -> app a) -> app a
withCurrentUser f = do
  assertCreds AP.currentUser
  mu <- currentUser
  case mu of
    Just u -> f u
    Nothing -> throwError AppUserNotLoggedIn

updateUser :: ID User -> UserDetails -> App ()
updateUser uid avatar = do
  assertCreds $ AP.updateUser uid
  db $ replaceDBUser uid avatar
  invalidateCaches $ Set.fromList [CacheKeyUser uid]


-- | Set 'AppUserState' to logged out if session is valid.  Returns 'False' if session is no longer
-- valid.
verifyAppState :: Database db => AppM db Bool
verifyAppState = do
  appLog LogDebug "verifyAppState"
  let ok = appLog LogDebug "verifyAppState: ok" >> pure True
      notok = appLog LogDebug "verifyAppState: not ok" >> pure False

  sessionDuration <- asks . view $ appConfig . cfgSessionLength . to timespanToNominalDiffTime
  gets (view appUserState) >>= \case
    UserLoggedOut -> ok
    UserLoggedIn _ (UserSession sid) -> dbUsersCmd (\db_ -> Users.verifySession db_ sid sessionDuration) >>= \case
      Nothing -> modify (appUserState .~ UserLoggedOut) >> notok
      Just _ -> ok
