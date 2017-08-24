{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Login.Status where

import Refine.Frontend.Prelude

import Refine.Common.Types (userName)
import Refine.Frontend.Icon
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types


loginStatusButton_ :: HasCallStack => (forall onclick. IbuttonProps onclick -> IbuttonProps onclick) -> CurrentUser -> ReactElementM handler ()
loginStatusButton_ tweak cu = ibutton_ $ emptyIbuttonProps "Login" onclick
  & ibLabel .~ mkLabel cu
  & ibSize .~ XXLarge
  & ibAlign .~ AlignRight
  & tweak
  where
    onclick = [MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)]

    mkLabel UserLoggedOut    = "login"
    mkLabel (UserLoggedIn n) = "I am " <> (n ^. userName)
