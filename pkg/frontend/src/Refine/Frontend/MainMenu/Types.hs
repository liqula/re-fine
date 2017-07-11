{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Frontend.MainMenu.Types where

import Refine.Frontend.Prelude

import GHC.Generics (Generic)
import React.Flux (UnoverlapAllEq)

import Refine.Common.Rest (ApiError)
import Refine.Frontend.Login.Types


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTab
  | MainMenuActionLoginError ApiError
  | MainMenuActionRegistrationError ApiError
  | MainMenuActionClearErrors
  deriving (Eq, Show, Generic)

data MainMenuErrors = MainMenuErrors
  { _mmeLogin        :: Maybe ST
  , _mmeRegistration :: Maybe ST
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
  | MainMenuOpen { _mainMenuOpenTab :: MainMenuTab }
  deriving (Eq, Show, Generic)

data MainMenuTab
  = MainMenuProcess
  | MainMenuGroup
  | MainMenuHelp
  | MainMenuLogin MainMenuSubTabLogin
  deriving (Eq, Show, Generic)

defaultMainMenuTab :: HasCallStack => MainMenuTab
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
  { _tmbimmpMainMenuTab    :: MainMenuTab
  , _tmbimmpCurrentUser    :: CurrentUser
  }
  deriving (Eq)

instance UnoverlapAllEq (MainMenuProps tab)
instance UnoverlapAllEq TopMenuBarInMainMenuProps

makeRefineTypes [''MainMenuAction, ''MainMenuErrors, ''MainMenuState, ''MainMenu, ''MainMenuTab, ''MainMenuSubTabLogin]
