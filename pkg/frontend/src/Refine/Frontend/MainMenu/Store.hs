module Refine.Frontend.MainMenu.Store where

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Types (RefineAction(..))


mainMenuUpdate :: RefineAction -> MainMenuState -> MainMenuState
mainMenuUpdate ToggleMainMenu   MainMenuClosed       = MainMenuLogin
mainMenuUpdate ToggleMainMenu   MainMenuLogin        = MainMenuClosed
mainMenuUpdate ToggleMainMenu   MainMenuRegistration = MainMenuClosed
mainMenuUpdate OpenLogin        MainMenuLogin        = MainMenuLogin
mainMenuUpdate OpenRegistration MainMenuLogin        = MainMenuRegistration
mainMenuUpdate OpenLogin        MainMenuRegistration = MainMenuLogin
mainMenuUpdate OpenRegistration MainMenuRegistration = MainMenuRegistration
mainMenuUpdate _                state                = state
