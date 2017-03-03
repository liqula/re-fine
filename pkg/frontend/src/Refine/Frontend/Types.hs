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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Types where

import           Control.Lens (makeLenses)
import           Data.Text (Text)
import           Data.Text.I18n
import           GHC.Generics (Generic)

import Refine.Common.Types as Common
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Login.Types
import Refine.Prelude.TH (makeRefineType)


data GlobalState = GlobalState
  { _gsVDoc                       :: Maybe CompositeVDoc
  , _gsVDocList                   :: Maybe [ID VDoc]
  , _gsContributionState          :: ContributionState
  , _gsHeaderState                :: HeaderState
  , _gsScreenState                :: ScreenState
  , _gsNotImplementedYetIsVisible :: Bool
  , _gsMainMenuState              :: MainMenuState
  , _gsLoginState                 :: LoginState
  , _gsToolbarSticky              :: Bool
  , _gsTranslations               :: Translations
  } deriving (Show, Generic)

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState
  { _gsVDoc                       = Nothing
  , _gsVDocList                   = Nothing
  , _gsContributionState          = emptyContributionState
  , _gsHeaderState                = emptyHeaderState
  , _gsScreenState                = emptyScreenState
  , _gsNotImplementedYetIsVisible = False
  , _gsMainMenuState              = emptyMainMenuState
  , _gsLoginState                 = emptyLoginState
  , _gsToolbarSticky              = False
  , _gsTranslations               = emptyTranslations
  }

data RefineAction = LoadDocumentList
                  | LoadedDocumentList [ID VDoc]
                  | LoadDocument (ID VDoc)
                  | OpenDocument CompositeVDoc
                  | AddDemoDocument
                  | ScreenAction ScreenAction
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
                  | ChangeCurrentUser CurrentUser
                  | ChangeTranslations L10
                  -- Actions that will be transformed because they need IO:
                  | TriggerUpdateSelection OffsetFromDocumentTop ToolbarExtensionStatus

                  | LoadTranslations Locale

                  -- Action only for testing:
                  | ClearState
  deriving (Show, Generic)

type Translations = Text -> Text

emptyTranslations :: Translations
emptyTranslations = id


makeLenses ''GlobalState
makeRefineType ''RefineAction
