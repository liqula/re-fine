{-# LANGUAGE CPP #-}
#include "language_frontend.hs"
module Refine.Frontend.MainMenu.Store where
#include "import_frontend.hs"

import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types (GlobalAction(..))
import Refine.Frontend.Types


mainMenuUpdate :: HasCallStack => GlobalAction -> MainMenuState -> MainMenuState
mainMenuUpdate (MainMenuAction (MainMenuActionOpen tab)) st = mainMenuOpen st tab

mainMenuUpdate (MainMenuAction (MainMenuActionLoginError e)) st = st
  & mmErrors . mmeLogin .~ Just e

mainMenuUpdate (MainMenuAction (MainMenuActionRegistrationError e)) st = st
  & mmErrors . mmeRegistration .~ Just (cs $ show e)

mainMenuUpdate (MainMenuAction MainMenuActionClearErrors) st = st
  & mmErrors . mmeLogin        .~ Nothing
  & mmErrors . mmeRegistration .~ Nothing

mainMenuUpdate _ st = st


mainMenuOpen :: HasCallStack => MainMenuState -> MainMenuTabAction -> MainMenuState
mainMenuOpen st = \case
  MainMenuCreateOrUpdateGroup _ (completeOrCancel -> True) -> st
  MainMenuCreateProcess (completeOrCancel -> True)         -> st
  MainMenuUpdateProcess _ (completeOrCancel -> True)       -> st
  MainMenuProfile (_, completeOrCancel -> True)            -> st
  tab -> st
     & mmState .~ mapMainMenuTab
                                id
                                id
                                -- the following impossible cases are ruled out by the cases above
                                (formAction id (error "impossible - MainMenuUpdateProcess #1") (error "impossible - MainMenuUpdateProcess #2"))
                                (formAction id (error "impossible - MainMenuUpdateProcess #3") (error "impossible - MainMenuUpdateProcess #4"))
                                (formAction id (error "impossible - MainMenuUpdateProcess #5") (error "impossible - MainMenuUpdateProcess #6"))
                                (second $ formAction id (error "impossible - MainMenuUpdateProcess #7") (error "impossible - MainMenuUpdateProcess #8"))
                                tab
