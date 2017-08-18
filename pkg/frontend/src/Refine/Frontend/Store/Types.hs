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

import           Control.Concurrent.MVar
import           System.IO.Unsafe
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String.Conversions (ST)
import           Data.Text.I18n
import           GHC.Generics (Generic)

import Refine.Common.Types
import Refine.Common.VDoc.Draft (rawContentFromCompositeVDoc)
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
-- import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types


-- * state

type GlobalState = GlobalState_ GlobalDocumentState

data GlobalState_ a = GlobalState
  { _gsEditID                     :: Maybe (ID VDoc)
  , _gsContributionState          :: ContributionState
  , _gsHeaderState                :: HeaderState
  , _gsDocumentState              :: a
  , _gsScreenState                :: ScreenState
  , _gsMainMenuState              :: MainMenuState
  , _gsToolbarSticky              :: Bool
  , _gsTranslations               :: Trans
  , _gsDevState                   :: Maybe DevState  -- ^ for development & testing, see 'devStateUpdate'.
  , _gsServerCache                :: ServerCache
  } deriving (Show, Eq, Generic, Functor)

emptyGlobalState :: HasCallStack => GlobalState
emptyGlobalState = GlobalState
  { _gsEditID                     = Nothing
  , _gsContributionState          = emptyContributionState
  , _gsHeaderState                = emptyHeaderState
  , _gsDocumentState              = emptyDocumentState
  , _gsScreenState                = emptyScreenState
  , _gsMainMenuState              = emptyMainMenuState
  , _gsToolbarSticky              = False
  , _gsTranslations               = emptyTrans
  , _gsDevState                   = Nothing
  , _gsServerCache                = mempty
  }

newtype DevState = DevState
  { _devStateTrace :: [GlobalAction]
  }
  deriving (Show, Eq, Generic)

emptyDevState :: HasCallStack => DevState
emptyDevState = DevState []


-- * actions

data GlobalAction =
    -- documents
    LoadVDoc (ID VDoc)

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
  | AddStatement Bool{-update-} (ID Statement) CreateStatement

  | CacheAction CacheAction

    -- i18n
  | LoadTranslations Locale
  | ChangeTranslations L10

    -- users
  | CreateUser CreateUser

    -- testing & dev
  | ResetState GlobalState
  | ShowNotImplementedYet

    -- make sure that between two actions, no rendering happens (only one composite state
    -- transformation).
  | CompositeAction [GlobalAction]
  deriving (Show, Eq, Generic)

-- to ensure consistency, cache actions may only be initiated by the server
data CacheAction
  = RefreshServerCache ServerCache
  | RestrictCacheItems (Set CacheKey)
  | InvalidateCacheItems (Set CacheKey)
  deriving (Show, Eq, Generic)

makeRefineTypes [''GlobalState_, ''DevState, ''GlobalAction, ''CacheAction]


-- * stuff

{-# NOINLINE cacheMissesMVar #-}
cacheMissesMVar :: MVar [CacheKey]
cacheMissesMVar = unsafePerformIO $ newMVar []

{-# NOINLINE cacheMisses #-}
cacheMisses :: [CacheKey] -> i -> a -> i
cacheMisses keys i _ = unsafePerformIO $ do
  is <- takeMVar cacheMissesMVar
  putMVar cacheMissesMVar $ keys <> is
  pure i

cacheMiss :: CacheKey -> i -> a -> i
cacheMiss = cacheMisses . pure

gsRawContent :: GlobalState -> RawContent
gsRawContent (view gsVDoc -> Just (Just cvdoc)) = rawContentFromCompositeVDoc cvdoc
gsRawContent _ = mkRawContent $ mkBlock "loading..." :| []

class CacheLookup a where
  cacheKey :: ID a -> CacheKey
  cacheLens :: Lens' ServerCache (Map (ID a) a)

cacheLookupIn :: CacheLookup a => ServerCache -> ID a -> Maybe a
cacheLookupIn sc i@(cacheKey -> k) = maybe (cacheMiss k Nothing i) Just . Map.lookup i $ sc ^. cacheLens

cacheLookup :: (HasCallStack, CacheLookup b) => GlobalState_ a -> ID b -> Maybe b
cacheLookup gs = cacheLookupIn (gs ^. gsServerCache)

type CLT = Except ()

-- TODO: collect more missing cache keys in one round
cacheLookupM :: (HasCallStack, CacheLookup b) => GlobalState_ a -> ID b -> CLT b
cacheLookupM gs i = do
  case cacheLookup gs i of
    Just val -> pure val
    Nothing -> throwError ()

instance CacheLookup VDoc where
  cacheKey = CacheKeyVDoc
  cacheLens = scVDocs

instance CacheLookup Edit where
  cacheKey = CacheKeyEdit
  cacheLens = scEdits

instance CacheLookup Note where
  cacheKey = CacheKeyNote
  cacheLens = scNotes

instance CacheLookup Discussion where
  cacheKey = CacheKeyDiscussion
  cacheLens = scDiscussions

instance CacheLookup User where
  cacheKey = CacheKeyUser
  cacheLens = scUsers

instance CacheLookup Group where
  cacheKey = CacheKeyGroup
  cacheLens = scGroups

-- Just (Just e) -> found!
-- Just Nothing -> ID exists, but is missing in cache
-- Nothing -> no ID
gsEdit :: HasCallStack => GlobalState_ a -> Maybe (Maybe Edit)
gsEdit gs = (>>= cacheLookup gs) <$> gsEditID' gs

gsEditID' :: HasCallStack => GlobalState_ a -> Maybe (Maybe (ID Edit))
gsEditID' gs = fmap (^. vdocHeadEdit) . cacheLookup gs <$> (gs ^. gsEditID)

-- TODO: remove
getEdit :: HasCallStack => GlobalState_ a -> ID Edit -> Maybe Edit
getEdit = cacheLookup

-- TODO: remove
getDiscussion :: HasCallStack => GlobalState_ a -> ID Discussion -> Maybe Discussion
getDiscussion = cacheLookup
{-
-- FIXME: optimize this
discussionOfStatement :: HasCallStack => GlobalState_ a -> ID Statement -> Maybe Discussion
discussionOfStatement sc i
  = listToMaybe [d | d <- Map.elems $ sc ^. scDiscussions, i `elem` map (^. statementID) (toList $ d ^. discussionTree)]
-}
gsVDoc :: Getter (GlobalState_ a) (Maybe (Maybe CompositeVDoc))
gsVDoc = to getCompositeVDoc
  where
    getCompositeVDoc :: GlobalState_ a -> Maybe (Maybe CompositeVDoc)
    getCompositeVDoc gs = (>>= mkCompositeVDoc gs) <$> gsEdit gs

    mkCompositeVDoc :: GlobalState_ a -> Edit -> Maybe CompositeVDoc
    mkCompositeVDoc gs edit = do
      vdoc <- cacheLookup gs $ edit ^. editVDoc
      pure $ CompositeVDoc
        vdoc
        edit
        (mkMap editChildren)
        (mkMap editNotes')
        (mkMap editDiscussions')
      where
        mkMap :: CacheLookup a => Lens' Edit (Set (ID a)) -> Map (ID a) a
        mkMap y = Map.fromList $ catMaybes [ (,) k <$> cacheLookup gs k | k <- Set.toList $ edit ^. y ]


gsCurrentSelection :: HasCallStack => Getter GlobalState (Maybe (Selection Position))
gsCurrentSelection = to (^? gsContributionState . csCurrentSelectionWithPx . _Just . sstSelectionState)
