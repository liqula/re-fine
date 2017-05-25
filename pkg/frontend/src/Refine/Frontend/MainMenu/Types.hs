{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Frontend.MainMenu.Types where

import Refine.Frontend.Prelude

import GHC.Generics (Generic)
import React.Flux (UnoverlapAllEq)

import Refine.Common.Rest (ApiErrorCreateUser)
import Refine.Common.Types.Prelude (Username)
import Refine.Frontend.Login.Types
import Refine.Prelude.TH (makeRefineType)


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTab
  | MainMenuActionLoginError        Username
  | MainMenuActionRegistrationError ApiErrorCreateUser
  | MainMenuActionClearErrors
  deriving (Eq, Show, Generic)

data MainMenuErrors = MainMenuErrors
  { _mmeLogin        :: FormError
  , _mmeRegistration :: FormError
  }
  deriving (Eq, Show, Generic)

defaultMainMenuErrors :: MainMenuErrors
defaultMainMenuErrors = MainMenuErrors
  { _mmeLogin        = Nothing
  , _mmeRegistration = Nothing
  }

data MainMenuState = MainMenuState
  { _mmState  :: MainMenu
  , _mmErrors :: MainMenuErrors
  }
  deriving (Eq, Show, Generic)

emptyMainMenuState :: MainMenuState
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

defaultMainMenuTab :: MainMenuTab
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

makeRefineType ''MainMenuAction
makeRefineType ''MainMenuErrors
makeRefineType ''MainMenuState
makeRefineType ''MainMenu
makeRefineType ''MainMenuTab
makeRefineType ''MainMenuSubTabLogin
