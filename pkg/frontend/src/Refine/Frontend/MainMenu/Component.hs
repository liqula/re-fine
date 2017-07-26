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

-- import           Data.Text.I18n (Locale(..))
import qualified Data.Text as ST
import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
import qualified Refine.Frontend.Colors as Colors
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Component
import           Refine.Frontend.Login.Status
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Contribution.Dialog (contributionDialogTextForm)


topMenuBarInMainMenu :: HasCallStack => View '[TopMenuBarInMainMenuProps]
topMenuBarInMainMenu = mkView "TopMenuBarInMainMenu" $ \(TopMenuBarInMainMenuProps currentTab currentUser) ->
  div_ ["className" $= "c-mainmenu-content__header"] $ do
    div_ ["className" $= "gr-2"] $ do
      ibutton_ $ emptyIbuttonProps "Close" [MainMenuAction MainMenuActionClose]
        & ibListKey .~ "1"
        & ibDarkBackground .~ True
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

    div_ ["className" $= "gr-20"] $ do
      ibutton_ $ emptyIbuttonProps "Process" [MainMenuAction $ MainMenuActionOpen MainMenuProcess]
        & ibListKey .~ "2"
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ (if currentTab == MainMenuProcess then HighlightAlways else HighlightOnMouseOver)
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

      ibutton_ $ emptyIbuttonProps "Group" [MainMenuAction . MainMenuActionOpen . MainMenuGroups $ Left ()]
        & ibListKey .~ "3"
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ (if currentTab & has _MainMenuGroup then HighlightAlways else HighlightOnMouseOver)
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

      ibutton_ $ emptyIbuttonProps "Help" [MainMenuAction $ MainMenuActionOpen MainMenuHelp]
        & ibListKey .~ "4"
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ (if currentTab == MainMenuHelp then HighlightAlways else HighlightOnMouseOver)
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

      ibutton_ $ emptyIbuttonProps "00_joker" [ShowNotImplementedYet]
        & ibListKey .~ "5"
        & ibDarkBackground .~ True
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

      -- search

      loginStatusButton_
        ( (ibDarkBackground .~ True)
        . (ibLabel .~ mempty)
        . (ibHighlightWhen .~ case currentTab of MainMenuLogin _ -> HighlightAlways; _ -> HighlightOnMouseOver)
        )
        currentUser

    div_ ["className" $= "gr-2"] $ do
      ibutton_ $ emptyIbuttonProps "00_joker" [ShowNotImplementedYet]
        & ibListKey .~ "7"
        & ibDarkBackground .~ True
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

topMenuBarInMainMenu_ :: HasCallStack => TopMenuBarInMainMenuProps -> ReactElementM eventHandler ()
topMenuBarInMainMenu_ = view_ topMenuBarInMainMenu "topMenuBarInMainMenu_"


tabStyles :: HasCallStack => [Decl]
tabStyles =
  [ decl "position" (Ident "absolute")
  , zindex ZIxLoginTab
  , decl "backgroundColor" Colors.SCBlue06
  , decl "padding" (Px 50)
  , decl "borderRadius" (Px 12)
  ]

mainMenu :: HasCallStack => View '[MainMenuProps MainMenuTabProps]
mainMenu = mkView "MainMenu" $ \(MainMenuProps currentTab menuErrors currentUser) -> do
  div_ ["className" $= "row row-align-middle c-mainmenu-content"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      topMenuBarInMainMenu_ (TopMenuBarInMainMenuProps currentTab currentUser)
      div_ [ "className" $= "gr-2" ] $ do
        pure ()
      div_ [ "className" $= "gr-20"
           , "style" @@= tabStyles
           ] $ do
        case currentTab of
          MainMenuProcess      -> "[MainMenuProcess]"
          MainMenuGroups gids  -> div_ $ do
            h1_ "Groups"
            br_ []
            toButton `mapM_` gids
            br_ []
            button_ [ "id" $= "add-vdoc-to-backend"
                    , onClick $ \_ _ -> simpleHandler . dispatch . MainMenuAction . MainMenuActionOpen . MainMenuCreateGroup . Left $ newLocalStateRef (CreateGroup "" "" [] []) gids
                    ] $
                    elemString "Create new group"
            where
              toButton :: HasCallStack => ID Group -> ReactElementM 'EventHandlerCode ()
              toButton li = button_
                [ "id" $= cs ("load-group-list" <> show (li ^. unID))
                , onClick $ \_ _ -> simpleHandler . dispatch . MainMenuAction . MainMenuActionOpen . MainMenuGroup $ li
                ]
                (elemText $ toUrlPiece li)

          MainMenuGroup{}      -> "[MainMenuGroup]"   -- TODO

          MainMenuCreateGroup lst -> createGroup_ lst

          MainMenuHelp         -> "[MainMenuHelp]"
          MainMenuLogin subtab -> mainMenuLoginTab_ (MainMenuProps subtab menuErrors currentUser)
      div_ [ "className" $= "gr-2" ] $ do
        pure ()

mainMenu_ :: HasCallStack => MainMenuProps MainMenuTabProps -> ReactElementM eventHandler ()
mainMenu_ = view_ mainMenu "mainMenu_"


mainMenuLoginTab :: HasCallStack => View '[MainMenuProps MainMenuSubTabLogin]
mainMenuLoginTab = mkView "MainMenuLoginTab" $ \(MainMenuProps currentTab menuErrors currentUser) -> do
      let tabButton :: Int -> MainMenuSubTabLogin -> ReactElementM eventHandler ()
          tabButton key this = div_ ["style" @@= [decl "marginLeft" (Px 40)]] $ do
            ibutton_ $ emptyIbuttonProps "00_joker" [MainMenuAction . MainMenuActionOpen . MainMenuLogin $ this]
              & ibListKey .~ cs (show key)
              & ibDarkBackground .~ False
              & ibHighlightWhen .~ (if currentTab == this then HighlightAlways else HighlightOnMouseOver)
              & ibLabel .~ (case this of
                             MainMenuSubTabLogin        -> "login"
                             MainMenuSubTabRegistration -> "register")

      div_ $ do
        tabButton 0 MainMenuSubTabLogin
        tabButton 1 MainMenuSubTabRegistration

      br_ [] >> br_ [] >> br_ [] >> hr_ []

      div_ $ do
        case currentTab of
          MainMenuSubTabLogin        -> loginOrLogout_ currentUser (menuErrors ^. mmeLogin)
          MainMenuSubTabRegistration -> registration_  (menuErrors ^. mmeRegistration)

mainMenuLoginTab_ :: HasCallStack => MainMenuProps MainMenuSubTabLogin -> ReactElementM eventHandler ()
mainMenuLoginTab_ = view_ mainMenuLoginTab "mainMenuLoginTab_"


createGroup :: HasCallStack => LocalStateRef CreateGroup -> View '[]
createGroup lst = mkPersistentStatefulView "CreateGroup" lst $
  \st@(CreateGroup title desc _ _) -> do

    contributionDialogTextForm createGroupTitle st 2 "group title"

    hr_ []

    contributionDialogTextForm createGroupDesc st 2 "group description"

    hr_ []

    let enableOrDisable props = if ST.null desc || ST.null title
          then props
            & iconButtonPropsDisabled     .~ True
          else props
            & iconButtonPropsDisabled     .~ False
            & iconButtonPropsOnClick      .~ [ MainMenuAction . MainMenuActionOpen . MainMenuCreateGroup $ Right st
                                             ]

    -- FIXME: make new button, like in 'commentInput_' above.  we
    -- don't have to save this in global state until the 'editInput_'
    -- dialog is closed again without save or cancel.
    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & enableOrDisable

createGroup_ :: HasCallStack => LocalStateRef CreateGroup -> ReactElementM eventHandler ()
createGroup_ lst = view_ (createGroup lst) "createGroup"
