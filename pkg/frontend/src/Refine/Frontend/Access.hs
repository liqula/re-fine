{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Refine.Frontend.Access where

import Refine.Frontend.Prelude

import           Refine.Common.Access
import           Refine.Common.Types hiding (Login(..))
import qualified Refine.Common.Types as Common
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Document.Types


data AccessState = AccessState
  { _accLoginState         :: LoginState
  , _accGroupRoles         :: [(GroupRole, ID Group)]
  , _accGlobalRoles        :: [GlobalRole]
  , _accDispatchAfterLogin :: [GlobalAction]
  }
  deriving (Show, Eq, Generic)

makeRefineTypes [''AccessState]

emptyAccessState :: AccessState
emptyAccessState = AccessState emptyLoginState [] [] []


type MainHeaderProps = (GlobalState_ WipedDocumentState, AccessState)  -- FIXME: shrink.


data AccessAction =
    AccessUpdate AccessState
  | Login Common.Login
  | Logout
  | LoginGuardStash [GlobalAction]  -- ^ if logged in, dispatch actions directly.  otherwise, login first.
  | LoginGuardPop  -- ^ dispatched this to trigger dispatch of the stashed actions after login.
  | SetCurrentUser CurrentUser
  deriving (Show, Eq, Generic)

instance (Dispatchable GlobalAction, Sendable ToServer) => Dispatchable AccessAction where
  dispatch a = [action @AccessState a]

instance (Dispatchable GlobalAction, Sendable ToServer) => StoreData AccessState where
    type StoreAction AccessState = AccessAction

    transform act st = case act of
      AccessUpdate state' -> pure state'

      LoginGuardStash actions -> do
        case st ^. accLoginState . lsCurrentUser of
          UserLoggedOut  -> do
            dispatchAndExec . MainMenuAction . MainMenuActionOpen . MainMenuLogin $ MainMenuSubTabLogin
            pure $ st & accDispatchAfterLogin %~ (<> actions)

          UserLoggedIn _ -> do
            dispatchAndExec `mapM_` actions
            pure st

      LoginGuardPop -> do
        case st ^. accLoginState . lsCurrentUser of
          UserLoggedOut -> error "LoginGuardPop before logged in!"
          UserLoggedIn _ -> do
            dispatchAndExec `mapM_` (st ^. accDispatchAfterLogin)
            pure $ st & accDispatchAfterLogin .~ []

      Login loginData -> do
        sendToServer $ TSLogin loginData
        pure st

      Logout -> do
        sendToServer TSLogout
        sendToServer TSClearCache
        dispatchAndExec $ CompositeAction
          [ MainMenuAction MainMenuActionClearErrors
          , MainMenuAction . MainMenuActionOpen . MainMenuLogin $ MainMenuSubTabLogin
          ]
        pure $ st & accLoginState . lsCurrentUser .~ UserLoggedOut

      SetCurrentUser user ->
        pure $ st & accLoginState . lsCurrentUser .~ user


-- | FIXME: We should be using 'childrenPassedToView' here, I suspect that would help with the
-- strange handler type inflexibility.  (For the time being, fix those problems calling
-- 'liftViewToStateHandler' in all the right places.)
guardAccess :: (Dispatchable GlobalAction, Sendable ToServer) => ReactElementM 'EventHandlerCode () -> View '[Creds]
guardAccess child = mkControllerView @'[StoreArg AccessState] "GuardAccess" $
  \(accessState :: AccessState) (xs :: Creds) -> do
    if hasCreds xs accessState then child else pure ()

guardAccess_ :: (HasCallStack, Dispatchable GlobalAction, Sendable ToServer) => JSString -> Creds -> ReactElementM 'EventHandlerCode () -> ReactElementM 'EventHandlerCode ()
guardAccess_ rkey xs child = view_ (guardAccess child) rkey xs


instance MonadAccess ((->) AccessState) where
  hasCred xs (AccessState loginState groupRoles globalRoles _) = case xs of
    CredUser uid           -> allowUser loginState uid
    CredNotLoggedIn        -> allowNotLoggedIn loginState
    CredGroupRole role uid -> allowGroupRole groupRoles (role, uid)
    CredGlobalRole role    -> allowGlobalRole globalRoles role


allowUser :: LoginState -> UserInfo -> Bool
allowUser (LoginState (UserLoggedIn (view userID -> uidIs))) (UserID uidShould) = uidIs == uidShould
allowUser _ _ = False

allowNotLoggedIn :: LoginState -> Bool
allowNotLoggedIn (LoginState UserLoggedOut) = True
allowNotLoggedIn _ = False

allowGroupRole :: [(GroupRole, ID Group)] -> (GroupRole, ID Group) -> Bool
allowGroupRole = flip elem

allowGlobalRole :: [GlobalRole] -> GlobalRole -> Bool
allowGlobalRole = flip elem


-- FIXME: how can we use this new code to do redirect if not logged in?  (we already do this, but not
-- in a nice way.)