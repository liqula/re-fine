{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Login.Status where
#include "import_frontend.hs"

import Refine.Common.Types
import Refine.Frontend.Icon
import Refine.Frontend.Icon.Svg as Svg
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Types


loginStatusButton_ :: HasCallStack
                   => ColorSchema -> Maybe Bool -> CurrentUser (Lookup User) -> ReactElementM handler ()
loginStatusButton_ schema pressed cu = div_ $ do
  ibutton_ $ emptyIbuttonProps (mkIcon cu) onclick
    & ibSize .~ XXLarge
    & ibPressed .~ pressed
  span_ $ do
    elemText $ mkLabel cu
  where
    onclick = [MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)]

    mkLabel UserLoggedOut    = "login"
    mkLabel (UserLoggedIn n) = "I am " <> either (const "...") (^. Common.userName) n

    mkIcon (UserLoggedIn (Right ((^. userAvatar) -> Just img))) = ButtonImageInline img
    mkIcon _ = ButtonImageIcon Svg.Login schema
