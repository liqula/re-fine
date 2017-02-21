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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.MainMenu.Component where

import           Data.String.Conversions (cs)
import           React.Flux

import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Prelude()
import           Refine.Frontend.Store
import           Refine.Frontend.Types
import           Refine.Frontend.UtilityWidgets

import Refine.Frontend.Login.Component (loginOrLogout_, registration_)


mainMenu :: ReactView MainMenuProps
mainMenu = defineView "MainMenu" $ \(MainMenuProps menuTab currentUser) ->
  div_ ["className" $= "row row-align-middle c-mainmenu-content"] $ do
    div_ ["className" $= "grid-wrapper"] $ do

      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-mainmenu-content__header"] $ do
            iconButton_ IconButtonProps
              { _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = menuTab == MainMenuLogin
                  , _iconPropsDesc      = ("icon-User", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = "active"
              , _iconButtonPropsContentType  = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsClickHandler = \_ -> dispatch . MainMenuAction $ MainMenuActionOpen MainMenuLogin
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-dashboard"]
              -- not translated from prototype2016:
              -- button attribute data-section="dashboard"
              }

            iconButton_ IconButtonProps
              { _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = menuTab == MainMenuRegistration
                  , _iconPropsDesc      = ("icon-User", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = "active"
              , _iconButtonPropsContentType  = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsClickHandler = \_ -> dispatch . MainMenuAction $ MainMenuActionOpen MainMenuRegistration
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-dashboard"]
              -- not translated from prototype2016:
              -- button attribute data-section="dashboard"
              }

            iconButton_ IconButtonProps
              { _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = False
                  , _iconPropsDesc      = ("icon-Group", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsContentType  = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsClickHandler = \_ -> []
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-membership"]
              -- not translated from prototype2016:
              -- button attribute data-section="membership"
              }

            iconButton_ IconButtonProps
              { _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-content"
                  , _iconPropsHighlight = False
                  , _iconPropsDesc      = ("icon-Help", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsContentType  = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsClickHandler = \_ -> []
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-help"]
              -- not translated from prototype2016:
              -- button attribute data-section="help"
              }

            case currentUser of
              NonLoggedInUser -> pure ()
              LoggedInUser username -> do
                iconButton_ IconButtonProps
                  { _iconButtonPropsIconProps = IconProps
                      { _iconPropsBlockName = "c-mainmenu-content"
                      , _iconPropsHighlight = False
                      , _iconPropsDesc      = ("icon-Exit", "dark")
                      , _iconPropsSize      = XXL
                      }
                  , _iconButtonPropsElementName  = "section-button"
                  , _iconButtonPropsModuleName   = ""
                  , _iconButtonPropsContentType  = ""
                  , _iconButtonPropsLabel        = cs username
                  , _iconButtonPropsDisabled     = False
                  , _iconButtonPropsClickHandler = \_ -> []
                  , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-help"]
                  -- not translated from prototype2016:
                  -- button attribute data-section="help"
                  }

            iconButton_ IconButtonProps
              { _iconButtonPropsIconProps = IconProps
                  { _iconPropsBlockName = "c-mainmenu-header"
                  , _iconPropsHighlight = True
                  , _iconPropsDesc      = ("icon-Close", "dark")
                  , _iconPropsSize      = XXL
                  }
              , _iconButtonPropsElementName  = "section-button"
              , _iconButtonPropsModuleName   = ""
              , _iconButtonPropsContentType  = ""
              , _iconButtonPropsLabel        = ""
              , _iconButtonPropsDisabled     = False
              , _iconButtonPropsClickHandler = \_ -> dispatch . MainMenuAction $ MainMenuActionClose
              , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-close"]
              -- not translated from prototype2016:
              -- n/a
              }

      case menuTab of
        MainMenuLogin        -> loginOrLogout_ currentUser
        MainMenuRegistration -> registration_


mainMenu_ :: MainMenuTab -> CurrentUser -> ReactElementM eventHandler ()
mainMenu_ ms cu = view mainMenu (MainMenuProps ms cu) mempty


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
