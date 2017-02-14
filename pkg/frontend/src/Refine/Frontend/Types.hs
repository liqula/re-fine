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

import Refine.Frontend.Bubbles.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Screen.Types
import Refine.Prelude.TH (makeRefineType)


data GlobalState = GlobalState
  { _gsVDoc                       :: Maybe CompositeVDoc
  , _gsVDocList                   :: Maybe [ID VDoc]
  , _gsBubblesState               :: BubblesState
  , _gsHeaderState                :: HeaderState
  , _gsScreenState                :: ScreenState
  , _gsNotImplementedYetIsVisible :: Bool
  , _gsMainMenuOpen               :: Bool
  , _gsToolbarSticky              :: Bool
  } deriving (Show, Generic)

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState Nothing Nothing emptyBubblesState emptyHeaderState emptyScreenState False False False

data RefineAction = LoadDocumentList
                  | LoadedDocumentList [ID VDoc]
                  | LoadDocument (ID VDoc)
                  | OpenDocument CompositeVDoc
                  | AddDemoDocument
                  | AddHeaderHeight Int
                  | SetWindowSize WindowSize
                  | BubblesAction BubblesAction
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
                  | ToggleMainMenu
                  -- Actions that will be transformed because they need IO:
                  | TriggerUpdateSelection DeviceOffset
  deriving (Show, Generic)

makeRefineType ''GlobalState
makeRefineType ''RefineAction
