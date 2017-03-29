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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.MainMenu.Component where

import           Control.Lens
import           Data.Default (def)
import           Data.Text.I18n (Locale(..))
import           React.Flux

import           Refine.Frontend.Header.UserLoginLogout (userLoginLogoutButton_)
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Login.Component
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types
import           Refine.Prelude()


data TopMenuBarInMainMenuProps = TopMenuBarInMainMenuProps
  { _tmbimmpMainMenuTab    :: MainMenuTab
  , _tmbimmpCurrentUser    :: CurrentUser
  }
  deriving (Eq)

topMenuBarInMainMenu :: View '[TopMenuBarInMainMenuProps]
topMenuBarInMainMenu = mkView "TopMenuBarInMainMenu" $ \(TopMenuBarInMainMenuProps menuTab currentUser) ->
  div_ ["className" $= "row row-align-middle c-mainmenu-content"] $ do
    div_ ["className" $= "grid-wrapper"] $ do

      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-mainmenu-content__header"] $ do
            let iprops activeTab = IconProps "c-mainmenu-content" (menuTab == activeTab) ("icon-User", "dark") XXL

            iconButton_ $ def
              & iconButtonPropsListKey      .~ "login"
              & iconButtonPropsIconProps    .~ iprops MainMenuLogin
              & iconButtonPropsElementName  .~ "section-button"
              & iconButtonPropsModuleName   .~ "active"
              & iconButtonPropsClickActions .~ [MainMenuAction $ MainMenuActionOpen MainMenuLogin]
              -- not translated from prototype2016:
              -- button attribute data-section="dashboard"

            iconButton_ $ def
              & iconButtonPropsListKey      .~ "register"
              & iconButtonPropsIconProps    .~ iprops MainMenuRegistration
              & iconButtonPropsElementName  .~ "section-button"
              & iconButtonPropsModuleName   .~ "active"
              & iconButtonPropsClickActions .~ [MainMenuAction $ MainMenuActionOpen MainMenuRegistration]
              & iconButtonPropsExtraClasses .~ ["c-mainmenu-content__btn-dashboard"]
              -- not translated from prototype2016:
              -- button attribute data-section="dashboard"

            -- TODO: Change language button should not be in the main menu
            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "locale-EN"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = False
                  , _iconPropsDesc      = ("icon-Group", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = "EN"
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsClickActions = [LoadTranslations $ Locale "en_GB"]
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-membership"]
              }

            -- TODO: Change language button should not be in the main menu
            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "locale-DE"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = False
                  , _iconPropsDesc      = ("icon-Group", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = "DE"
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsClickActions = [LoadTranslations $ Locale "de_DE"]
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-membership"]
              }


            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "help"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = False
                  , _iconPropsDesc      = ("icon-Help", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsClickActions = []
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-help"]
              -- not translated from prototype2016:
              -- button attribute data-section="help"
              }

            userLoginLogoutButton_ currentUser

            iconButton_ IconButtonProps
              { _iconButtonPropsListKey = "close"
              , _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-header"
                  , _iconPropsHighlight = True
                  , _iconPropsDesc      = ("icon-Close", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsPosition     = Nothing
              , _iconButtonPropsAlignRight   = False
              , _iconButtonPropsClickActions = [MainMenuAction MainMenuActionClose]
              , _iconButtonPropsClickPropag  = True
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-close"]
              -- not translated from prototype2016:
              -- n/a
              }


topMenuBarInMainMenu_ :: TopMenuBarInMainMenuProps -> ReactElementM eventHandler ()
topMenuBarInMainMenu_ !props = view_ topMenuBarInMainMenu "topMenuBarInMainMenu_" props


mainMenu :: View '[MainMenuProps]
mainMenu = mkView "MainMenu" $ \(MainMenuProps menuTab menuErrors currentUser) ->
  div_ $ do
    topMenuBarInMainMenu_ (TopMenuBarInMainMenuProps menuTab currentUser)
    case menuTab of
      MainMenuLogin        -> loginOrLogout_ currentUser (menuErrors ^. mmeLogin)
      MainMenuRegistration -> registration_  (menuErrors ^. mmeRegistration)


mainMenu_ :: MainMenuTab -> MainMenuErrors -> CurrentUser -> ReactElementM eventHandler ()
mainMenu_ mt me cu = view_ mainMenu "mainMenu_" (MainMenuProps mt me cu)


{-

prototype2016 draft:

        div_ ["className" $= "c-mainmenu-content__main"]
            div_ ["className" $= "c-mainmenu-content__section c-mainmenu-content--dashboard c-mainmenu-content__section--active"]
                <h4 class="c-mainmenu-content__headline">Mein Dashboard</h4>
                div_ ["className" $= "c-mainmenu-content__meta"]
                    div_ ["className" $= "c-mainmenu-content__meta-username"]admin admin
                    div_ ["className" $= "c-mainmenu-content__meta-useravatar"]
                        div_ ["className" $= "icon-User_dark iconsize-xxl"]
                            <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                <h5 class="c-mainmenu-content__separator">Letzte Aktivitäten</h5>
                div_ ["className" $= "c-mainmenu-content__row"]
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu-content__icon"]
                            div_ ["className" $= "o-icon-highlight icon-User_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]User XY
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu-content__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Discussion_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Discussion with C
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu-content__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Idea_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Idea A
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu-content__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Index_desktop_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Dokument A
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu-content__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Index_desktop_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Dokument C
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu-content__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Idea_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Idea A
                    </button>
                <h5 class="c-mainmenu-content__separator">Meine Prozesse</h5>
                div_ ["className" $= "c-mainmenu-content__row"]
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Process_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Prozess A
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Process_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Prozess B
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Process_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Prozess C
                    </button>
                <h5 class="c-mainmenu-content__separator">Meine Dokumente</h5>
                div_ ["className" $= "c-mainmenu-content__row"]
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Index_desktop_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Dokument A
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Index_desktop_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Dokument B
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Index_desktop_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Dokument C
                    </button>
                <h5 class="c-mainmenu-content__separator">Meine Favoriten</h5>
                div_ ["className" $= "c-mainmenu-content__row"]
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Idea_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Idea A
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-User_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]User XY
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Index_desktop_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Dokument C
                    </button>
                    <button class="c-mainmenu-content__button">
                        div_ ["className" $= "c-mainmenu__icon"]
                            div_ ["className" $= "o-icon-highlight icon-Discussion_dark iconsize-xxl"]
                                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                        div_ ["className" $= "c-mainmenu-content__button-label"]Discussion with C
                    </button>
            div_ ["className" $= "c-mainmenu-content__section c-mainmenu-content--membership"]
                <h4 class="c-mainmenu-content__headline">Meine Gruppen</h4>
            div_ ["className" $= "c-mainmenu-content__section c-mainmenu-content--help"]
                <h4 class="c-mainmenu-content__headline">Hilfe</h4>

-}
