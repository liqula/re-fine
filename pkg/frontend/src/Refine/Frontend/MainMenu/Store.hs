{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.MainMenu.Store where

import Refine.Frontend.Prelude

import Refine.Frontend.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types (GlobalAction(..))


mainMenuUpdate :: HasCallStack => GlobalAction -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction MainMenuActionClose) st = st
  & mmState                    .~ MainMenuClosed
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) st = case tab of
  MainMenuGroups BeforeAjax{} -> st
  MainMenuCreateGroup _ FormComplete{} -> st
  MainMenuCreateProcess FormComplete{} -> st
  _ -> st
     & mmState .~ MainMenuOpen (mapMainMenuTab
                                (ajaxAction (error "impossible") (const ()))
                                id
                                (formAction id (error "impossible"))
                                (formAction id (error "impossible"))
                                tab)

mainMenuUpdate (MainMenuAction (MainMenuActionLoginError e)) st = st
  & mmErrors . mmeLogin .~ Just e

mainMenuUpdate (MainMenuAction (MainMenuActionRegistrationError e)) st = st
  & mmErrors . mmeRegistration .~ Just (cs $ show e)

mainMenuUpdate (MainMenuAction MainMenuActionClearErrors) st = st
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (LoadDocument AfterAjax{}) st = st
  & mmState .~ MainMenuClosed

mainMenuUpdate _ st = st
