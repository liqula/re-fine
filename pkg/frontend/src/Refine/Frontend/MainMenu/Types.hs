{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Frontend.MainMenu.Types where

import GHC.Generics (Generic)

import Refine.Frontend.Login.Types
import Refine.Prelude.TH (makeRefineType)


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTab
  deriving (Eq, Show, Generic)

data MainMenuState
  = MainMenuClosed
  | MainMenuOpen { _mainMenuOpenTab :: MainMenuTab }
  deriving (Eq, Show, Generic)

data MainMenuTab = MainMenuLogin | MainMenuRegistration
  deriving (Eq, Show, Generic)

defaultMainMenuTab :: MainMenuTab
defaultMainMenuTab = MainMenuLogin

data MainMenuProps = MainMenuProps
  { _mmpMainMenuTab :: MainMenuTab
  , _mmpCurrentUser :: CurrentUser
  }

makeRefineType ''MainMenuAction
makeRefineType ''MainMenuState
makeRefineType ''MainMenuTab
