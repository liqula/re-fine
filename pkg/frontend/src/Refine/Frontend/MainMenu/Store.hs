{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.MainMenu.Store where

import Refine.Frontend.Prelude

import Control.Lens ((&), (.~))
import Data.String.Conversions (cs)

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types (GlobalAction(..))


mainMenuUpdate :: GlobalAction -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction MainMenuActionClose) state = state
  & mmState                    .~ MainMenuClosed
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) state = state
  & mmState .~ MainMenuOpen tab

mainMenuUpdate (MainMenuAction (MainMenuActionLoginError e)) state = state
  & mmErrors . mmeLogin .~ Just e

mainMenuUpdate (MainMenuAction (MainMenuActionRegistrationError e)) state = state
  & mmErrors . mmeRegistration .~ Just (cs $ show e)

mainMenuUpdate (MainMenuAction MainMenuActionClearErrors) state = state
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate _ state = state
