{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Frontend.MainMenu.Types where

import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Frontend.Login.Types
import Refine.Prelude.TH (makeRefineType)


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTab
  | MainMenuActionLoginError        ST
  | MainMenuActionRegistrationError ST
  | MainMenuActionClearErrors
  deriving (Eq, Show, Generic)

data MainMenuErrors = MainMenuErrors
  { _mmeLogin        :: FormError
  , _mmeRegistration :: FormError
  }
  deriving (Eq, Show, Generic)

data MainMenuState = MainMenuState
  { _mmState  :: MainMenu
  , _mmErrors :: MainMenuErrors
  }
  deriving (Eq, Show, Generic)

emptyMainMenuState :: MainMenuState
emptyMainMenuState = MainMenuState
  { _mmState  = MainMenuClosed
  , _mmErrors = MainMenuErrors Nothing Nothing
  }

data MainMenu
  = MainMenuClosed
  | MainMenuOpen { _mainMenuOpenTab :: MainMenuTab }
  deriving (Eq, Show, Generic)

data MainMenuTab
  = MainMenuLogin
  | MainMenuRegistration
  deriving (Eq, Show, Generic)

defaultMainMenuTab :: MainMenuTab
defaultMainMenuTab = MainMenuLogin

data MainMenuProps = MainMenuProps
  { _mmpMainMenuTab    :: MainMenuTab
  , _mmpMainMenuErrors :: MainMenuErrors
  , _mmpCurrentUser    :: CurrentUser
  }

makeRefineType ''MainMenuAction
makeRefineType ''MainMenuErrors
makeRefineType ''MainMenuState
makeRefineType ''MainMenu
makeRefineType ''MainMenuTab
