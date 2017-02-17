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

module Refine.Frontend.Types where

import           Data.Text (Text)
import           GHC.Generics (Generic)

import Refine.Common.Types as Common

import Refine.Frontend.Contribution.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Screen.Types
import Refine.Prelude.TH (makeRefineType)


data GlobalState = GlobalState
  { _gsVDoc                       :: Maybe CompositeVDoc
  , _gsVDocList                   :: Maybe [ID VDoc]
  , _gsContributionState          :: ContributionState
  , _gsHeaderState                :: HeaderState
  , _gsScreenState                :: ScreenState
  , _gsNotImplementedYetIsVisible :: Bool
  , _gsMainMenuState              :: MainMenuState
  , _gsToolbarSticky              :: Bool
  } deriving (Show, Generic)

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState Nothing Nothing emptyContributionState emptyHeaderState emptyScreenState False MainMenuClosed False

data RefineAction = LoadDocumentList
                  | LoadedDocumentList [ID VDoc]
                  | LoadDocument (ID VDoc)
                  | OpenDocument CompositeVDoc
                  | AddDemoDocument
                  | AddHeaderHeight Int
                  | SetWindowSize WindowSize
                  | ContributionAction ContributionAction
                  | HeaderAction HeaderAction
                  | ToolbarStickyStateChange Bool
                  -- ...
                  | AddDiscussion CompositeDiscussion
                  | AddNote Note
                  | SaveSelect Text Text
                  -- ...
                  | CreateUser CreateUser
                  | Login Login
                  | Logout
                  | ShowNotImplementedYet
                  | HideNotImplementedYet
                  | MainMenuAction MainMenuAction
                  -- Actions that will be transformed because they need IO:
                  | TriggerUpdateSelection DeviceOffset
                  -- Action only for testing:
                  | ClearState
  deriving (Show, Generic)

makeRefineType ''GlobalState
makeRefineType ''RefineAction
