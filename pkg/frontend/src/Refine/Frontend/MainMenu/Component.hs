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

import qualified Data.Text as ST
import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Common.Test.Samples
import           Refine.Frontend.Types
import qualified Refine.Frontend.Colors as Colors
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Component
import           Refine.Frontend.Login.Status
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Contribution.Dialog (contributionDialogTextForm)
import qualified Refine.Prelude.BuildInfo as BuildInfo


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

      ibutton_ $ emptyIbuttonProps "Group" [MainMenuAction . MainMenuActionOpen . MainMenuGroups $ BeforeAjax ()]
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
  , decl "backgroundColor" Colors.MainmenuContentColor
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
          MainMenuGroups groups       -> mainMenuGroups_ groups
          MainMenuGroup group         -> mainMenuGroup_ group
          MainMenuCreateGroup mid lst -> mainMenuCreateGroup_ mid lst
          MainMenuCreateProcess lst   -> mainMenuCreateProcess_ lst

          MainMenuHelp -> pre_ $ do
            elemString $ "commit hash: " <> show BuildInfo.gitCommitHash
            "\n"
            elemString $ "build timestamp: " <> show BuildInfo.gitBuildTimestamp
            "\n"

          MainMenuLogin subtab -> mainMenuLoginTab_ (MainMenuProps subtab menuErrors currentUser)

      div_ [ "className" $= "gr-2" ] $ do
        pure ()

mainMenu_ :: HasCallStack => MainMenuProps MainMenuTabProps -> ReactElementM eventHandler ()
mainMenu_ = view_ mainMenu "mainMenu_"


mainMenuGroups :: View '[[Group]]
mainMenuGroups = mkView "MainMenuGroups" $ \groups -> do
  div_ $ do
    div_ ["style" @@= [decl "marginLeft" (Px 3)]] $ do
      let mkCreateGroupAction :: GlobalAction
          mkCreateGroupAction = MainMenuAction . MainMenuActionOpen . MainMenuCreateGroup Nothing . FormOngoing
                              $ newLocalStateRef (CreateGroup "" "" [] []) groups

      ibutton_ $ emptyIbuttonProps "Group_add" [mkCreateGroupAction]
        & ibListKey .~ "create_group"
        & ibSize .~ XLarge
        & ibDarkBackground .~ False
        & ibHighlightWhen .~ HighlightOnMouseOver
        & ibLabel .~ "create group"

    div_ $ do
      br_ [] >> br_ [] >> br_ [] >> hr_ []

    div_ $ do
      mainMenuGroupShort_ `mapM_` groups

mainMenuGroups_ :: HasCallStack => [Group] -> ReactElementM eventHandler ()
mainMenuGroups_ = view_ mainMenuGroups "mainMenuGroups"

mainMenuGroupShort :: HasCallStack => View '[Group]
mainMenuGroupShort = mkView "MainMenuGroupShort" $ \group -> do
  let listKey = cs ("group-list-item-" <> show (group ^. groupID . unID))
  div_ [ onClick $ \_ _ -> simpleHandler . dispatch
                         . MainMenuAction . MainMenuActionOpen . MainMenuGroup $ group ^. groupID
       , "id" $= listKey  -- FUTUREWORK: get rid of this.  at the time of writing this it's only
                          -- used in the acceptance test.

       , "style" @@= [ decl "background-color" Colors.SCBlue03
                     , decl "padding" (Px 50)
                     , decl "margin" (Px 50)
                     , decl "borderRadius" (Px 12)
                     ]
       ] $ do

    -- image (FIXME: allow the user to store one in the group and use that)
    br_ []
    ibutton_ $ emptyIbuttonProps "Group" ([] :: [GlobalAction])
      & ibListKey .~ "group"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ True
      & ibHighlightWhen .~ HighlightOnMouseOver
      & ibLabel .~ mempty

    -- title
    br_ []
    div_ [] $ do
      elemText $ group ^. groupTitle

    -- buttons for processes, users (FUTUREWORK: make those clickable in addition to the entire
    -- tile.  clicking on 'users' button should get you there directly.)
    br_ []
    div_ [] $ do
      let numProcesses :: Int = length $ group ^. groupVDocs
      ibutton_ $ emptyIbuttonProps "Process" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-processes")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightOnMouseOver
        & ibLabel .~ cs (show numProcesses)  -- FIXME: should be rendered differently, see click dummy

      let numUsers :: Int = 13  -- FIXME
      ibutton_ $ emptyIbuttonProps "User" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-users")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightOnMouseOver
        & ibLabel .~ cs (show numUsers)  -- FIXME: should be rendered differently, see click dummy


mainMenuGroupShort_ :: HasCallStack => Group -> ReactElementM eventHandler ()
mainMenuGroupShort_ group = view_ mainMenuGroupShort listKey group
  where
    listKey = "mainMenuGroupShort-" <> (cs . show $ group ^. groupID . unID)

mainMenuGroup :: View '[Group]
mainMenuGroup = mkView "mainMenuGroup" $ \group -> do
  div_ $ do
    let
      gid = group ^. groupID

      toButton :: HasCallStack => ID VDoc -> ReactElementM 'EventHandlerCode ()
      toButton vdoc = button_
        [ "id" $= cs ("process-list-item-" <> show (vdoc ^. unID))
        , onClick $ \_ _ -> simpleHandler . dispatch . LoadDocument $ BeforeAjax vdoc
        ]
        (elemText . cs . show $ vdoc ^. unID)

    h1_ . elemText $ "Group " <> (group ^. groupTitle)
    elemText $ group ^. groupDesc
    br_ []
    br_ []
    elemText "Processes: "
    toButton `mapM_` (group ^. groupVDocs)
    br_ []
    br_ []
    button_ [ "id" $= "create-process"
            , onClick $ \_ _ -> simpleHandler . dispatch
                              . MainMenuAction . MainMenuActionOpen . MainMenuCreateProcess . FormOngoing
                              $ newLocalStateRef (CreateVDoc sampleTitle sampleAbstract sampleVDocVersion gid) group
            ] $
            elemString "Create new process"
    br_ []
    button_ [ "id" $= "update-group"
            , onClick $ \_ _ -> simpleHandler . dispatch
                              . MainMenuAction . MainMenuActionOpen . MainMenuCreateGroup (Just gid) . FormOngoing
                              $ newLocalStateRef (CreateGroup (group ^. groupTitle) (group ^. groupDesc) [] []) group
            ] $
            elemString "Update group details"
    br_ []
    button_ [ "id" $= "group-back"
            , onClick $ \_ _ -> simpleHandler . dispatch
                              . MainMenuAction . MainMenuActionOpen . MainMenuGroups $ BeforeAjax ()
            ] $
            elemString "Back"

mainMenuGroup_ :: HasCallStack => Group -> ReactElementM eventHandler ()
mainMenuGroup_ = view_ mainMenuGroup "mainMenuGroup"

-- | FUTUREWORK: should this be @View '[LocalStateRef CreateGroup]@ or @View '[Maybe (ID Group),
-- LocalStateRef CreateGroup]@?  (same with 'mainMenuCreateProcess'.)
mainMenuCreateGroup :: HasCallStack => Maybe (ID Group) -> LocalStateRef CreateGroup -> View '[]
mainMenuCreateGroup mid lst = mkPersistentStatefulView "MainMenuCreateGroup" lst $
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
            & iconButtonPropsOnClick      .~ [ MainMenuAction . MainMenuActionOpen . MainMenuCreateGroup mid $ FormComplete st
                                             ]

    -- FIXME: make new button, like in 'commentInput_' above.  we
    -- don't have to save this in global state until the 'editInput_'
    -- dialog is closed again without save or cancel.
    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ maybe "save" (const "update") mid
            & iconButtonPropsAlignRight   .~ True
            & enableOrDisable

mainMenuCreateGroup_ :: HasCallStack => Maybe (ID Group) -> LocalStateRef CreateGroup -> ReactElementM eventHandler ()
mainMenuCreateGroup_ mid lst = view_ (mainMenuCreateGroup mid lst) "mainMenuCreateGroup"

mainMenuCreateProcess :: HasCallStack => LocalStateRef CreateVDoc -> View '[]
mainMenuCreateProcess lst = mkPersistentStatefulView "MainMenuCreateProcess" lst $
  \st@(CreateVDoc title _ _ _) -> do

    contributionDialogTextForm (createVDocTitle . unTitle) st 2 "title"
    hr_ []
    contributionDialogTextForm (createVDocAbstract . unAbstract) st 2 "abstract"
    hr_ []
    contributionDialogTextForm (createVDocInitVersion . unVDocVersion) st 2 "initial version"
    hr_ []

    let enableOrDisable props = if ST.null (title ^. unTitle)
          then props
            & iconButtonPropsDisabled     .~ True
          else props
            & iconButtonPropsDisabled     .~ False
            & iconButtonPropsOnClick      .~ [ MainMenuAction . MainMenuActionOpen . MainMenuCreateProcess $ FormComplete st
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

mainMenuCreateProcess_ :: HasCallStack => LocalStateRef CreateVDoc -> ReactElementM eventHandler ()
mainMenuCreateProcess_ lst = view_ (mainMenuCreateProcess lst) "mainMenuCreateProcess"


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
