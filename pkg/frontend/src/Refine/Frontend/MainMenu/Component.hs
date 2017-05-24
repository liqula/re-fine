{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.MainMenu.Component where

import Refine.Frontend.Prelude

import           Data.Text.I18n (Locale(..))

import           Refine.Frontend.Header.UserLoginLogout
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Login.Component
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Icon


topMenuBarInMainMenu :: View '[TopMenuBarInMainMenuProps]
topMenuBarInMainMenu = mkView "TopMenuBarInMainMenu" $ \(TopMenuBarInMainMenuProps menuTab currentUser) ->
  div_ ["className" $= "row row-align-middle c-mainmenu-content"] $ do
    div_ ["className" $= "grid-wrapper"] $ do

      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-mainmenu-content__header"] $ do
            let iprops thisTab = IconProps "c-mainmenu-content" (menuTab == thisTab) ("icon-User", "dark") XXLarge

            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "close"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-header"
                  , _iconPropsHighlight = True
                  , _iconPropsDesc      = ("icon-Close", "bright")
                  , _iconPropsSize      = XXLarge
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsOnClick      = [MainMenuAction MainMenuActionClose]
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-close"]
              }

            iconButton_ $ defaultIconButtonProps @[GlobalAction]
              & iconButtonPropsListKey      .~ "login"
              & iconButtonPropsIconProps    .~ iprops MainMenuLogin
              & iconButtonPropsElementName  .~ "section-button"
              & iconButtonPropsModuleName   .~ "active"
              & iconButtonPropsOnClick      .~ [MainMenuAction $ MainMenuActionOpen MainMenuLogin]

            iconButton_ $ defaultIconButtonProps @[GlobalAction]
              & iconButtonPropsListKey      .~ "register"
              & iconButtonPropsIconProps    .~ iprops MainMenuRegistration
              & iconButtonPropsElementName  .~ "section-button"
              & iconButtonPropsModuleName   .~ "active"
              & iconButtonPropsOnClick      .~ [MainMenuAction $ MainMenuActionOpen MainMenuRegistration]
              & iconButtonPropsExtraClasses .~ ["c-mainmenu-content__btn-dashboard"]

            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "locale-EN"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = True
                  , _iconPropsDesc      = ("icon-Group", "dark")
                  , _iconPropsSize      = XXLarge
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = "EN"
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsOnClick      = [LoadTranslations $ Locale "en_GB"]
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-membership"]
              }

            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "locale-DE"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = True
                  , _iconPropsDesc      = ("icon-Group", "dark")
                  , _iconPropsSize      = XXLarge
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = "DE"
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsOnClick      = [LoadTranslations $ Locale "de_DE"]
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-membership"]
              }

            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "help"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = False
                  , _iconPropsDesc      = ("icon-Help", "dark")
                  , _iconPropsSize      = XXLarge
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsOnClick      = [] :: [GlobalAction]
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-help"]
              }

            loginStatusButton_ True currentUser

topMenuBarInMainMenu_ :: TopMenuBarInMainMenuProps -> ReactElementM eventHandler ()
topMenuBarInMainMenu_ !props = view_ topMenuBarInMainMenu "topMenuBarInMainMenu_" props


mainMenu :: View '[MainMenuProps]
mainMenu = mkView "MainMenu" $ \(MainMenuProps menuTab menuErrors currentUser) -> do
  div_ $ do
    topMenuBarInMainMenu_ (TopMenuBarInMainMenuProps menuTab currentUser)
  div_ $ do
    case menuTab of
      MainMenuLogin        -> loginOrLogout_ currentUser (menuErrors ^. mmeLogin)
      MainMenuRegistration -> registration_  (menuErrors ^. mmeRegistration)

mainMenu_ :: MainMenuTab -> MainMenuErrors -> CurrentUser -> ReactElementM eventHandler ()
mainMenu_ mt me cu = view_ mainMenu "mainMenu_" (MainMenuProps mt me cu)
