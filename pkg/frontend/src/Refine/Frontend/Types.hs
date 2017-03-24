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

module Refine.Frontend.Types where

import           Data.Text (Text)
import           Data.Text.I18n
import           GHC.Generics (Generic)

import Refine.Common.Types
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Screen.Types
import Refine.Prelude.Aeson (NoJSONRep(..))
import Refine.Prelude.TH (makeRefineType)


data GlobalState = GlobalState
  { _gsVDoc                       :: Maybe CompositeVDoc
  , _gsVDocList                   :: Maybe [ID VDoc]
  , _gsContributionState          :: ContributionState
  , _gsHeaderState                :: HeaderState
  , _gsDocumentState              :: DocumentState
  , _gsScreenState                :: ScreenState
  , _gsNotImplementedYetIsVisible :: Bool
  , _gsMainMenuState              :: MainMenuState
  , _gsLoginState                 :: LoginState
  , _gsToolbarSticky              :: Bool
  , _gsTranslations               :: NoJSONRep Translations
  } deriving (Show, Generic)

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState
  { _gsVDoc                       = Nothing
  , _gsVDocList                   = Nothing
  , _gsContributionState          = emptyContributionState
  , _gsHeaderState                = emptyHeaderState
  , _gsDocumentState              = DocumentStateView
  , _gsScreenState                = emptyScreenState
  , _gsNotImplementedYetIsVisible = False
  , _gsMainMenuState              = emptyMainMenuState
  , _gsLoginState                 = emptyLoginState
  , _gsToolbarSticky              = False
  , _gsTranslations               = NoJSONRep emptyTranslations
  }

data GlobalAction =
    -- documents
    LoadDocumentList
  | LoadedDocumentList [ID VDoc]
  | LoadDocument (ID VDoc)
  | OpenDocument CompositeVDoc

    -- contributions
  | ScreenAction ScreenAction
  | ContributionAction ContributionAction
  | HeaderAction HeaderAction
  | DocumentAction DocumentAction
  | ToolbarStickyStateChange Bool
  | MainMenuAction MainMenuAction
  | AddNote Note
  | AddDiscussion CompositeDiscussion
  | AddEdit Edit
  | SaveSelect Text Text
  | TriggerUpdateSelection OffsetFromDocumentTop ToolbarExtensionStatus

    -- i18n
  | LoadTranslations Locale
  | ChangeTranslations L10

    -- users
  | CreateUser CreateUser
  | Login Login
  | Logout
  | ChangeCurrentUser CurrentUser

    -- testing & dev
  | AddDemoDocument
  | ClearState
  | ShowNotImplementedYet
  | HideNotImplementedYet
  deriving (Show, Generic)

makeRefineType ''GlobalState
makeRefineType ''GlobalAction
