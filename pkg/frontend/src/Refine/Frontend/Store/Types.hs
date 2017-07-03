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

import Refine.Frontend.Prelude

import           Control.Lens (Getter, lens)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String.Conversions (ST)
import           Data.Text.I18n
import           GHC.Generics (Generic)
import           React.Flux (UnoverlapAllEq)

import Refine.Common.Types
import Refine.Common.VDoc.Draft (rawContentFromCompositeVDoc)
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types


type GlobalState = GlobalState_ GlobalDocumentState

data GlobalState_ a = GlobalState
  { _gsEdit                       :: Maybe (ID Edit)
  , _gsVDocList                   :: Maybe [ID VDoc]  -- ^ FIXME: this should be live in it's own
                                                      -- 'GlobalState' constructor.
  , _gsContributionState          :: ContributionState
  , _gsHeaderState                :: HeaderState
  , _gsDocumentState              :: a
  , _gsScreenState                :: ScreenState
  , _gsMainMenuState              :: MainMenuState
  , _gsLoginState                 :: LoginState
  , _gsToolbarSticky              :: Bool
  , _gsTranslations               :: Trans
  , _gsDevState                   :: Maybe DevState  -- ^ for development & testing, see 'devStateUpdate'.
  , _gsServerCache                :: ServerCache
  } deriving (Show, Eq, Generic, Functor)

data ServerCache = ServerCache
  { _scVDocs       :: Map (ID VDoc)       VDoc
  , _scEdits       :: Map (ID Edit)       Edit
  , _scNotes       :: Map (ID Note)       Note
  , _scDiscussions :: Map (ID Discussion) Discussion
  , _scUsers       :: Map (ID User)       User
  , _scGroups      :: Map (ID Group)      Group
  }
  deriving (Show, Eq, Generic)

emptyGlobalState :: HasCallStack => GlobalState
emptyGlobalState = GlobalState
  { _gsEdit                       = Nothing
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
  , _gsServerCache                = emptyServerCache
  }

emptyServerCache :: ServerCache
emptyServerCache = ServerCache mempty mempty mempty mempty mempty mempty

type MainHeaderProps = GlobalState_ WipedDocumentState

newtype DevState = DevState
  { _devStateTrace :: [GlobalAction]
  }
  deriving (Show, Eq, Generic)

emptyDevState :: HasCallStack => DevState
emptyDevState = DevState []

data GlobalAction =
    -- documents
    LoadDocumentList
  | RegisterDocumentList [ID VDoc]
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
  | AddDiscussion Discussion
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

makeRefineTypes [''ServerCache, ''GlobalState_, ''DevState, ''GlobalAction]

getDocumentState :: GlobalState -> DocumentState
getDocumentState gs@(view gsVDoc -> Just cvdoc)
  = mapDocumentState
      (const $ rawContentFromCompositeVDoc cvdoc)
      ((gs ^. gsServerCache . scEdits) Map.!)
  $ gs ^. gsDocumentState
getDocumentState _
  = error "getDocumentState: no gsVDoc"

gsVDoc :: Lens' (GlobalState_ a) (Maybe CompositeVDoc)
gsVDoc = lens getCompositeVDoc setCompositeVDoc
  where
    getCompositeVDoc :: GlobalState_ a -> Maybe CompositeVDoc
    getCompositeVDoc gs = mkCompositeVDoc (gs ^. gsServerCache) <$> (gs ^. gsEdit)

    mkCompositeVDoc :: ServerCache -> ID Edit -> CompositeVDoc
    mkCompositeVDoc sc eid = CompositeVDoc
      ((sc ^. scVDocs) Map.! (edit ^. editVDoc))
      edit
      (mkMap scEdits editChildren)
      (mkMap scNotes editNotes')
      (mkMap scDiscussions editDiscussions')
      where
        edit = (sc ^. scEdits) Map.! eid

        -- TUNING: this go through y and construct x from that, this way we don't have to touch the
        -- elements of x we want to throw out.
        mkMap :: Lens' ServerCache (Map (ID a) a) -> Lens' Edit (Set (ID a)) -> Map (ID a) a
        mkMap x y = Map.filterWithKey (\k _ -> k `Set.member` (edit ^. y)) $ sc ^. x

    setCompositeVDoc :: GlobalState_ a -> Maybe CompositeVDoc -> GlobalState_ a
    setCompositeVDoc gs Nothing = gs & gsEdit .~ Nothing
    setCompositeVDoc gs (Just cvd) = gs
      & gsEdit .~ Just (cvd ^. compositeVDocThisEdit . editID)
      & gsServerCache %~ updateCache
      where
        updateCache sc = sc
          & scVDocs       %~ uncurry Map.insert (mkItem vdocID $ cvd ^. compositeVDoc)
          & scEdits       %~ uncurry Map.insert (mkItem editID $ cvd ^. compositeVDocThisEdit)
          & scEdits       %~ ((cvd ^. compositeVDocApplicableEdits) <>)
          & scNotes       %~ ((cvd ^. compositeVDocApplicableNotes) <>)
          & scDiscussions %~ ((cvd ^. compositeVDocApplicableDiscussions) <>)

        mkItem :: Lens' a (ID a) -> a -> (ID a, a)
        mkItem k x = (x ^. k, x)


instance UnoverlapAllEq GlobalState

gsCurrentSelection :: HasCallStack => Getter GlobalState (Maybe (Selection Position))
gsCurrentSelection = to (^? gsContributionState . csCurrentSelectionWithPx . _Just . sstSelectionState)
