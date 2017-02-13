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

{-# OPTIONS_GHC -w #-}

module Refine.Frontend.MainMenu where

import           Control.Lens ((^.))
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.String.Conversions
import           Data.String (fromString)
import qualified Data.Tree as DT
import           Data.Void
import           React.Flux
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Prelude (ClearTypeParameter(..))
import           Refine.Frontend.Bubbles.Bubble
import           Refine.Frontend.Bubbles.Mark
import           Refine.Frontend.Bubbles.Overlay
import           Refine.Frontend.Bubbles.QuickCreate
import           Refine.Frontend.Bubbles.Types as RS
import           Refine.Frontend.Loader.Component (vdocLoader_)
import           Refine.Frontend.Login.Component (login_)
import           Refine.Frontend.ThirdPartyViews (sticky_, stickyContainer_)
import           Refine.Frontend.Screen.WindowSize (windowSize_, WindowSizeProps(..))
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets


mainMenu :: ReactView Bool
mainMenu = defineView "MainMenu" $ \visible -> if not visible then mempty else do
  div_ ["className" $= "row row-align-middle c-mainmenu-content"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-mainmenu-content__header"] $ do
            iconButton_ $ IconButtonProps
              { _iconProps = IconProps
                  { _blockName     = "c-mainmenu-content"
                  , _iconHighlight = True
                  , _iconDesc      = ("icon-User", "dark")
                  , _size          = XXL
                  }
              , _elementName   = "section-button"
              , _moduleName    = "active"
              , _contentType   = ""
              , _label         = ""
              , _disabled      = False
              , _clickHandler  = \_ -> []
              }
              -- not translated from prototype2016:
              -- button attribute data-section="dashboard"
              -- button attribute class="c-mainmenu-content__btn-dashboard"



{-



-- next steps:
-- 3. rename fields to carry prefices.
-- 4. translate all the other buttons
-- 5. remove -w flag everywhere.  MR, closing issue!



            <button data-section="membership" class="c-mainmenu-content__section-button c-mainmenu-content__btn-membership">
                div_ ["className" $= "c-mainmenu-content__icon"]
                    div_ ["className" $= "icon-Group_dark iconsize-xxl"]
                        <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
            </button>
            <button data-section="help" class="c-mainmenu-content__section-button c-mainmenu-content__btn-help">
                div_ ["className" $= "c-mainmenu-content__icon"]
                    div_ ["className" $= "icon-Help_dark iconsize-xxl"]
                        <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
            </button>
            <button class="c-mainmenu-header__icon c-mainmenu-content__btn-close">
                div_ ["className" $= "o-icon-highlight icon-Close_dark iconsize-xxl"]
                    <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
            </button>
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

mainMenu_ :: Bool -> ReactElementM eventHandler ()
mainMenu_ props = view mainMenu props mempty
