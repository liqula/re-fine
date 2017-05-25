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

import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Component
import           Refine.Frontend.Login.Status
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store.Types


topMenuBarInMainMenu :: View '[TopMenuBarInMainMenuProps]
topMenuBarInMainMenu = mkView "TopMenuBarInMainMenu" $ \(TopMenuBarInMainMenuProps menuTab currentUser) ->
  div_ ["className" $= "row row-align-middle c-mainmenu-content"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "c-mainmenu-content__header"] $ do
        div_ ["className" $= "gr-2"] $ do
          ibutton_ $ emptyIbuttonProps "Close" [MainMenuAction MainMenuActionClose]
            & ibListKey .~ "1"
            & ibDarkBackground .~ True
            & ibSize .~ XXLarge
            & ibLabel .~ mempty

        div_ ["className" $= "gr-20"] $ do
          ibutton_ $ emptyIbuttonProps "Process" []
            & ibListKey .~ "2"
            & ibDarkBackground .~ True
            & ibSize .~ XXLarge
            & ibLabel .~ mempty

          ibutton_ $ emptyIbuttonProps "Group" []
            & ibListKey .~ "3"
            & ibDarkBackground .~ True
            & ibSize .~ XXLarge
            & ibLabel .~ mempty

          ibutton_ $ emptyIbuttonProps "Help" []
            & ibListKey .~ "4"
            & ibDarkBackground .~ True
            & ibSize .~ XXLarge
            & ibLabel .~ mempty

          ibutton_ $ emptyIbuttonProps "00_joker" []
            & ibListKey .~ "5"
            & ibDarkBackground .~ True
            & ibSize .~ XXLarge
            & ibLabel .~ mempty

          -- search

          loginStatusButton_ True currentUser

        div_ ["className" $= "gr-2"] $ do
          ibutton_ $ emptyIbuttonProps "00_joker" []
            & ibListKey .~ "7"
            & ibDarkBackground .~ True
            & ibSize .~ XXLarge
            & ibLabel .~ mempty

topMenuBarInMainMenu_ :: TopMenuBarInMainMenuProps -> ReactElementM eventHandler ()
topMenuBarInMainMenu_ !props = view_ topMenuBarInMainMenu "topMenuBarInMainMenu_" props


mainMenu :: View '[MainMenuProps]
mainMenu = mkView "MainMenu" $ \(MainMenuProps menuTab menuErrors currentUser) -> do
  div_ $ do
    topMenuBarInMainMenu_ (TopMenuBarInMainMenuProps menuTab currentUser)
  div_ ["className" $= "gr-20 gr-center"] $ do
    case menuTab of
      MainMenuLogin        -> loginOrLogout_ currentUser (menuErrors ^. mmeLogin)
      MainMenuRegistration -> registration_  (menuErrors ^. mmeRegistration)

mainMenu_ :: MainMenuTab -> MainMenuErrors -> CurrentUser -> ReactElementM eventHandler ()
mainMenu_ mt me cu = view_ mainMenu "mainMenu_" (MainMenuProps mt me cu)
