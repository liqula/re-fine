module Refine.Frontend.MainMenu.Store where

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Types (RefineAction(..))


mainMenuUpdate :: RefineAction -> MainMenuState -> MainMenuState
mainMenuUpdate ToggleMainMenu   MainMenuClosed       = MainMenuOpen defaultMainMenuTab
mainMenuUpdate ToggleMainMenu   (MainMenuOpen _)     = MainMenuClosed
mainMenuUpdate OpenLogin        (MainMenuOpen _)     = MainMenuOpen MainMenuLogin
mainMenuUpdate OpenRegistration (MainMenuOpen _)     = MainMenuOpen MainMenuRegistration
mainMenuUpdate _                state                = state
