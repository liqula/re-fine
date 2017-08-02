{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.MainMenu.Store where

import Refine.Frontend.Prelude

import Refine.Frontend.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types (GlobalAction(..))


mainMenuUpdate :: HasCallStack => GlobalAction -> Bool -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction MainMenuActionClose) isThereVDoc st = st
  & mmState                    .~ (if isThereVDoc then MainMenuClosed else MainMenuOpen MainMenuHelp)
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) _ st = case tab of
  MainMenuGroups BeforeAjax{} -> st
  MainMenuCreateOrUpdateGroup _ FormComplete{} -> st
  MainMenuCreateProcess FormComplete{} -> st
  _ -> st
     & mmState .~ MainMenuOpen (mapMainMenuTab
                                (ajaxAction (error "impossible") (const ()))
                                id
                                (formAction id (error "impossible"))
                                (formAction id (error "impossible"))
                                tab)

mainMenuUpdate (MainMenuAction (MainMenuActionLoginError e)) _ st = st
  & mmErrors . mmeLogin .~ Just e

mainMenuUpdate (MainMenuAction (MainMenuActionRegistrationError e)) _ st = st
  & mmErrors . mmeRegistration .~ Just (cs $ show e)

mainMenuUpdate (MainMenuAction MainMenuActionClearErrors) _ st = st
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (LoadDocument AfterAjax{}) _ st = st
  & mmState .~ MainMenuClosed

mainMenuUpdate _ _ st = st
