{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Login.Status where
#include "import_frontend.hs"

import React.Flux.Missing
import Refine.Common.Types
import Refine.Frontend.Icon
import Refine.Frontend.Icon.Svg as Svg
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Types
import Refine.Frontend.Util (hourglass)


loginStatusButton_ :: HasCallStack
                   => ColorSchema -> Maybe Bool -> CurrentUser (Lookup User) -> ReactElementM handler ()
loginStatusButton_ schema pressed cu = div_ $ do
  ibutton_ $ emptyIbuttonProps (mkIcon cu) (onLoginClick cu)
    & ibSize .~ XXLarge
    & ibPressed .~ pressed
  span_ $ mkLabel cu
  where
    mkLabel UserLoggedOut              = "login"
    mkLabel (UserLoggedIn (Left _uid)) = hourglass
    mkLabel (UserLoggedIn (Right usr)) = elemText $ "welcome " <> usr ^. Common.userName <> "!"

    mkIcon (UserLoggedIn (Right ((^. userAvatar) -> Just img))) = ButtonImageInline img
    mkIcon _ = ButtonImageIcon Svg.Login schema


onLoginClick :: CurrentUser (Lookup User) -> [GlobalAction]
onLoginClick UserLoggedOut              = [MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)]
onLoginClick (UserLoggedIn (Left _uid)) = []
onLoginClick (UserLoggedIn (Right usr)) = [MainMenuAction $ MainMenuActionOpen (MainMenuProfile (usr ^. userID, formstate))]
  where
    formstate = FormBegin $ newLocalStateRef (Nothing, Nothing) usr
