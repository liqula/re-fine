module Refine.Frontend.MainMenu.Store where

import Control.Lens ((&), (.~))

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Types (GlobalAction(..))


-- TODO: Alignment
mainMenuUpdate :: GlobalAction -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction MainMenuActionClose) state = state & mmState .~ MainMenuClosed
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) state =
  state & mmState .~ MainMenuOpen tab

mainMenuUpdate (MainMenuAction (MainMenuActionLoginError e)) state =
  state & mmErrors . mmeLogin .~ Just e

mainMenuUpdate (MainMenuAction (MainMenuActionRegistrationError e)) state =
  state & mmErrors . mmeRegistration .~ Just e

mainMenuUpdate (MainMenuAction MainMenuActionClearErrors) state =
  state & mmErrors . mmeLogin        .~ Nothing
        & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate _ state = state
