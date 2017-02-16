module Refine.Frontend.MainMenu.Store where

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Types (RefineAction(..))


mainMenuUpdate :: RefineAction -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction MainMenuActionClose)      _     = MainMenuClosed
mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) _     = MainMenuOpen tab
mainMenuUpdate _                                         state = state
