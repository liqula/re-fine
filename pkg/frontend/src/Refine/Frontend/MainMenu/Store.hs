{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.MainMenu.Store where

import Refine.Frontend.Prelude

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types (GlobalAction(..))


mainMenuUpdate :: HasCallStack => GlobalAction -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction MainMenuActionClose) st = st
  & mmState                    .~ MainMenuClosed
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) st = st
  & mmState .~ MainMenuOpen tab

mainMenuUpdate (MainMenuAction (MainMenuActionLoginError e)) st = st
  & mmErrors . mmeLogin .~ (Just . cs . show $ e)

mainMenuUpdate (MainMenuAction (MainMenuActionRegistrationError e)) st = st
  & mmErrors . mmeRegistration .~ (Just . cs . show $ e)

mainMenuUpdate (MainMenuAction MainMenuActionClearErrors) st = st
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate _ st = st
