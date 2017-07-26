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
  { _mmState  = MainMenuClosed
  , _mmErrors = defaultMainMenuErrors
  }

data MainMenu
  = MainMenuClosed
  | MainMenuOpen { _mainMenuOpenTab :: MainMenuTabState }
  deriving (Eq, Show, Generic)

type MainMenuTabState  = MainMenuTab [ID Group] (ID Group) (LocalStateRef CreateGroup)
type MainMenuTabAction = MainMenuTab (Either () [ID Group]) (ID Group) (Either (LocalStateRef CreateGroup) CreateGroup)
type MainMenuTabProps  = MainMenuTab [ID Group] Group (LocalStateRef CreateGroup)

data MainMenuTab gids group cgroup
  = MainMenuProcess
  | MainMenuGroups gids
  | MainMenuGroup group
  | MainMenuCreateGroup cgroup
  | MainMenuHelp
  | MainMenuLogin MainMenuSubTabLogin
  deriving (Eq, Show, Generic)

mapMainMenuTab :: (a -> a') -> (b -> b') -> (c -> c') -> MainMenuTab a b c -> MainMenuTab a' b' c'
mapMainMenuTab fa fb fc = \case
  MainMenuProcess  -> MainMenuProcess
  MainMenuGroups a -> MainMenuGroups (fa a)
  MainMenuGroup b  -> MainMenuGroup (fb b)
  MainMenuCreateGroup c -> MainMenuCreateGroup (fc c)
  MainMenuHelp     -> MainMenuHelp
  MainMenuLogin l  -> MainMenuLogin l

defaultMainMenuTab :: HasCallStack => MainMenuTab gid group cgroup
defaultMainMenuTab = MainMenuProcess

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
