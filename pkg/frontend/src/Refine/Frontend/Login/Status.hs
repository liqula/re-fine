{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Login.Status where
#include "import_frontend.hs"

import Refine.Common.Types
import Refine.Frontend.Icon
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types


loginStatusButton_ :: HasCallStack => (forall onclick. IbuttonProps onclick -> IbuttonProps onclick) -> CurrentUser_ (Either (ID User) User) -> ReactElementM handler ()
loginStatusButton_ tweak cu = ibutton_ $ emptyIbuttonProps_ (mkIcon cu) onclick
  & ibLabel .~ mkLabel cu
  & ibSize .~ XXLarge
  & ibAlign .~ AlignRight
  & tweak
  where
    onclick = [MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)]

    mkLabel UserLoggedOut    = "login"
    mkLabel (UserLoggedIn n) = "I am " <> either (const "...") (^. Common.userName) n

    mkIcon (UserLoggedIn (Right ((^. userAvatar) -> Just img))) = ButtonImageInline img
    mkIcon _ = ButtonImageIcon "Login"
