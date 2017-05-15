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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Store.Types where

import           Data.String.Conversions (ST)
import           Data.Text.I18n
import           GHC.Generics (Generic)
import           React.Flux (UnoverlapAllEq)

import Refine.Common.Types
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Screen.Types
import Refine.Prelude.TH (makeRefineType)


data GlobalState = GlobalState
  { _gsVDoc                       :: Maybe CompositeVDoc  -- ^ FIXME: this should be split up into
                                                          -- its 'gsDocumentState' part and its
                                                          -- 'gsContributionState' part.
  , _gsVDocList                   :: Maybe [ID VDoc]  -- ^ FIXME: this should be live in it's own
                                                      -- 'GlobalState' constructor.
  , _gsContributionState          :: ContributionState
  , _gsHeaderState                :: HeaderState
  , _gsDocumentState              :: DocumentState
  , _gsScreenState                :: ScreenState
  , _gsMainMenuState              :: MainMenuState
  , _gsLoginState                 :: LoginState
  , _gsToolbarSticky              :: Bool
  , _gsTranslations               :: Trans
  , _gsDevState                   :: Maybe DevState  -- ^ for development & testing, see 'devStateUpdate'.
  } deriving (Show, Eq, Generic)

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState
  { _gsVDoc                       = Nothing
  , _gsVDocList                   = Nothing
  , _gsContributionState          = emptyContributionState
  , _gsHeaderState                = emptyHeaderState
  , _gsDocumentState              = emptyDocumentState
  , _gsScreenState                = emptyScreenState
  , _gsMainMenuState              = emptyMainMenuState
  , _gsLoginState                 = emptyLoginState
  , _gsToolbarSticky              = False
  , _gsTranslations               = emptyTrans
  , _gsDevState                   = Nothing
  }

newtype DevState = DevState
  { _devStateTrace :: [GlobalAction]
  }
  deriving (Show, Eq, Generic)

emptyDevState :: DevState
emptyDevState = DevState []

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
  | SaveSelect ST ST

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
  | ResetState GlobalState
  | ShowNotImplementedYet
  deriving (Show, Eq, Generic)

makeRefineType ''GlobalState
makeRefineType ''DevState
makeRefineType ''GlobalAction

instance UnoverlapAllEq GlobalState
