{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Frontend.MainMenu.Types where

import Refine.Frontend.Prelude

import GHC.Generics (Generic)

import Refine.Common.Types (Group)
import Refine.Common.Rest (ApiErrorCreateUser)
import Refine.Common.Types.Prelude (Username)
import Refine.Frontend.Login.Types


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTabState
  | MainMenuActionOpenGroups
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

type MainMenuTabState  = MainMenuTab [ID Group] (ID Group)
type MainMenuTabAction = MainMenuTab () (ID Group)
type MainMenuTabProps  = MainMenuTab [ID Group] Group

data MainMenuTab gids{-[ID Group] | ()-} group{-ID Group | Group-}
  = MainMenuProcess
  | MainMenuGroups gids
  | MainMenuGroup group
  | MainMenuHelp
  | MainMenuLogin MainMenuSubTabLogin
  deriving (Eq, Show, Generic)

mapMainMenuTab :: (a -> a') -> (b -> b') -> MainMenuTab a b -> MainMenuTab a' b'
mapMainMenuTab fa fb = \case
  MainMenuProcess  -> MainMenuProcess
  MainMenuGroups a -> MainMenuGroups (fa a)
  MainMenuGroup b  -> MainMenuGroup (fb b)
  MainMenuHelp     -> MainMenuHelp
  MainMenuLogin l  -> MainMenuLogin l

defaultMainMenuTab :: HasCallStack => MainMenuTab gid group
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
