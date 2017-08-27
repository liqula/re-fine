{-# LANGUAGE CPP #-}
#include "language.hs"
module Refine.Frontend.MainMenu.Store where
#include "import_frontend.hs"

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types (GlobalAction(..))
import Refine.Frontend.Types


mainMenuUpdate :: HasCallStack => GlobalAction -> Bool -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction MainMenuActionClose) isThereVDoc st = st
  & mmState                    .~ (if isThereVDoc then MainMenuClosed else MainMenuOpen $ MainMenuGroups ())
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) _ st = case tab of
  MainMenuCreateOrUpdateGroup _ FormComplete{} -> st
  MainMenuCreateProcess FormComplete{} -> st
  MainMenuUpdateProcess _ FormComplete{} -> st
  _ -> st
     & mmState .~ MainMenuOpen (mapMainMenuTab
                                id
                                id
                                (formAction id (error "impossible - MainMenuUpdateProcess #1") (error "impossible - MainMenuUpdateProcess #2"))
                                (formAction id (error "impossible - MainMenuUpdateProcess #3") (error "impossible - MainMenuUpdateProcess #4"))
                                (formAction id (error "impossible - MainMenuUpdateProcess #5") (error "impossible - MainMenuUpdateProcess #6"))
                                tab)

mainMenuUpdate (MainMenuAction (MainMenuActionLoginError e)) _ st = st
  & mmErrors . mmeLogin .~ Just e

mainMenuUpdate (MainMenuAction (MainMenuActionRegistrationError e)) _ st = st
  & mmErrors . mmeRegistration .~ Just (cs $ show e)

mainMenuUpdate (MainMenuAction MainMenuActionClearErrors) _ st = st
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate (LoadVDoc _) _ st = st
  & mmState .~ MainMenuClosed

mainMenuUpdate _ _ st = st
