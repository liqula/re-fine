{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Frontend.MainMenu.Types where

import Refine.Frontend.Prelude

import GHC.Generics (Generic)

import React.Flux.Missing
import Refine.Common.Types
import Refine.Common.Rest (ApiErrorCreateUser)
import Refine.Common.Types.Prelude (Username)
import Refine.Frontend.Login.Types


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTabAction
  | MainMenuActionLoginError        Username
  | MainMenuActionRegistrationError ApiErrorCreateUser
  | MainMenuActionClearErrors
  deriving (Eq, Show, Generic)

data MainMenuErrors = MainMenuErrors
  { _mmeLogin        :: FormError
  , _mmeRegistration :: FormError
  }
  deriving (Eq, Show, Generic)

defaultMainMenuErrors :: HasCallStack => MainMenuErrors
defaultMainMenuErrors = MainMenuErrors
  { _mmeLogin        = Nothing
  , _mmeRegistration = Nothing
  }

data MainMenuState = MainMenuState
  { _mmState  :: MainMenu
  , _mmErrors :: MainMenuErrors
  }
  deriving (Eq, Show, Generic)

emptyMainMenuState :: HasCallStack => MainMenuState
emptyMainMenuState = MainMenuState
  { _mmState  = MainMenuOpen $ MainMenuLogin MainMenuSubTabLogin
  , _mmErrors = defaultMainMenuErrors
  }

data MainMenu
  = MainMenuClosed
  | MainMenuOpen { _mainMenuOpenTab :: MainMenuTabState }
  deriving (Eq, Show, Generic)

type MainMenuTabState = MainMenuTab
      ()
      (ID Group)
      (LocalStateRef CreateGroup)
      (LocalStateRef CreateVDoc)
type MainMenuTabAction = MainMenuTab
      (Either () [ID Group])
      (ID Group)
      (Either (LocalStateRef CreateGroup) CreateGroup)
      (Either (LocalStateRef CreateVDoc) CreateVDoc)
type MainMenuTabProps = MainMenuTab
      [Group]
      Group
      (LocalStateRef CreateGroup)
      (LocalStateRef CreateVDoc)

data MainMenuTab gids group cgroup cprocess
  = MainMenuGroups gids
  | MainMenuGroup group
  | MainMenuCreateGroup (Maybe (ID Group)) cgroup
  | MainMenuCreateProcess cprocess
  | MainMenuHelp
  | MainMenuLogin MainMenuSubTabLogin
  deriving (Eq, Show, Generic)

mapMainMenuTab :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> MainMenuTab a b c d -> MainMenuTab a' b' c' d'
mapMainMenuTab fa fb fc fd = \case
  MainMenuGroups a -> MainMenuGroups (fa a)
  MainMenuGroup b  -> MainMenuGroup (fb b)
  MainMenuCreateGroup u c -> MainMenuCreateGroup u (fc c)
  MainMenuCreateProcess d -> MainMenuCreateProcess (fd d)
  MainMenuHelp     -> MainMenuHelp
  MainMenuLogin l  -> MainMenuLogin l

defaultMainMenuTab :: MainMenuTabAction
defaultMainMenuTab = MainMenuGroups $ Left ()

data MainMenuSubTabLogin = MainMenuSubTabLogin | MainMenuSubTabRegistration
  deriving (Eq, Show, Generic)

data MainMenuProps tab = MainMenuProps
  { _mmpMainMenuTab    :: tab
  , _mmpMainMenuErrors :: MainMenuErrors
  , _mmpCurrentUser    :: CurrentUser
  }
  deriving (Eq)

data TopMenuBarInMainMenuProps = TopMenuBarInMainMenuProps
  { _tmbimmpMainMenuTab    :: MainMenuTabProps
  , _tmbimmpCurrentUser    :: CurrentUser
  }
  deriving (Eq)

makeRefineTypes [''MainMenuAction, ''MainMenuErrors, ''MainMenuState, ''MainMenu, ''MainMenuTab, ''MainMenuSubTabLogin]
