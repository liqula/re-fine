{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Store.Types where
#include "import_frontend.hs"

import           Control.Concurrent.MVar
import           System.IO.Unsafe
import           Data.Text.I18n

import React.Flux.Missing
import Refine.Common.Types
import Refine.Common.VDoc.Draft (rawContentFromCompositeVDoc)
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Login.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.MainMenu.Types
import qualified Refine.Frontend.Route as Route
import Refine.Frontend.Screen.Types
import Refine.Frontend.Types
import Refine.Frontend.Util


-- * state

type GlobalState = GlobalState_ DocumentState

data GlobalState_ a = GlobalState
  { _gsPageState                  :: PageState a
  , _gsScreenState                :: ScreenState
--  , _gsTranslations               :: Trans
  , _gsDevState                   :: Maybe DevState  -- ^ for development & testing, see 'devStateUpdate'.
  , _gsServerCache                :: ServerCache
  } deriving (Show, Eq, Generic, Functor)

emptyGlobalState :: HasCallStack => GlobalState
emptyGlobalState = GlobalState
  { _gsPageState                  = emptyPageState
  , _gsScreenState                = emptyScreenState
--  , _gsTranslations               = emptyTrans
  , _gsDevState                   = Nothing
  , _gsServerCache                = mempty
  }

-- | There are two pages: the vdoc process, and main menu.  If we get to the main menu from the
-- process, we store the last state.
data PageState a
  = PageStateVDoc (ProcessState a)
  | PageStateMainMenu MainMenuState (Maybe (PageState a))
  deriving (Show, Eq, Generic, Functor)

emptyPageState :: PageState a
emptyPageState = PageStateMainMenu emptyMainMenuState Nothing

data ProcessState a = ProcessState
  { _psVDocID            :: ID VDoc
  , _psHeaderState       :: HeaderState
  , _psDocumentState     :: a
  , _psContributionState :: ContributionState
  } deriving (Show, Eq, Generic, Functor)

emptyProcessState :: ID VDoc -> ProcessState DocumentState
emptyProcessState vid = ProcessState vid emptyHeaderState emptyDocumentState emptyContributionState

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
  | UnloadVDoc

    -- contributions
  | ScreenAction ScreenAction
  | ContributionAction ContributionAction
  | HeaderAction HeaderAction
  | DocumentAction DocumentAction
  | MainMenuAction MainMenuAction
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

    -- route update
  | OnLocationHashChange (Either Route.RouteParseError Route.Route)

    -- testing & dev
  | ResetState GlobalState
  | ShowNotImplementedYet
  | DumpAllState

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


-- * lenses

makeRefineTypes [''GlobalState_, ''PageState, ''ProcessState, ''DevState, ''GlobalAction, ''CacheAction]

gsProcessState :: Getter (GlobalState_ a) (Maybe (ProcessState a))
gsProcessState = to $ \gs -> case gs ^. gsPageState of PageStateVDoc p -> Just p; _ -> Nothing

-- | (Lens' does not work because we would have to be able to create the entire 'Process' value from
-- 'Nothing' if we get a vdoc id.)
gsVDocID :: forall a. Getter (GlobalState_ a) (Maybe (ID VDoc))
gsVDocID = to (fmap (^. psVDocID) . (^. gsProcessState))

gsHeaderState :: Getter (GlobalState_ a) (Maybe HeaderState)
gsHeaderState = to (fmap (^. psHeaderState) . (^. gsProcessState))

gsDocumentState :: forall a. Getter (GlobalState_ a) (Maybe a)
gsDocumentState = to (fmap (^. psDocumentState) . (^. gsProcessState))

gsContributionState :: Getter (GlobalState_ a) (Maybe ContributionState)
gsContributionState = to (fmap (^. psContributionState) . (^. gsProcessState))


-- | Currently open 'Edit'.
--
-- >>> Just (Just e) -- found
-- >>> Just Nothing  -- ID exists, but it or the corresponding vdoc is missing in cache
-- >>> Nothing       -- no such ID
--
-- NOTE: you may get an edit here even if you are, say in the menu and it is not technically open.
gsEdit :: HasCallStack => Getter (GlobalState_ a) (Maybe (Maybe Edit))
gsEdit = to $ \gs -> (>>= cacheLookup gs) <$> gs ^. gsEditID

-- | See 'gsEdit'.
gsEditID :: HasCallStack => Getter (GlobalState_ a) (Maybe (Maybe (ID Edit)))
gsEditID = to $ \gs -> fmap (^. vdocHeadEdit) . cacheLookup gs <$> gs ^. gsVDocID

gsCompositeVDoc :: Getter (GlobalState_ a) (Maybe (Maybe CompositeVDoc))
gsCompositeVDoc = to $ \gs -> (\vid -> (vid, gs ^. gsServerCache) ^. getCompositeVDoc) <$> (gs ^. gsVDocID)

getCompositeVDoc :: Getter (ID VDoc, ServerCache) (Maybe CompositeVDoc)
getCompositeVDoc = to $ \(vid, cache) -> do
  vdoc <- cacheLookupIn cache vid
  edit <- cacheLookupIn cache $ vdoc ^. vdocHeadEdit
  pure $ CompositeVDoc
    vdoc
    edit
    (Map.fromList $ catMaybes [ (,) k <$> cacheLookupIn cache k | k <- Set.toList $ edit ^. editChildren ])
    (Map.fromList $ catMaybes [ cacheLookupIn cache k <&> \d -> (k, (r, d)) | (k, r) <- Map.toList $ edit ^. editDiscussions' ])

gsCurrentSelection :: HasCallStack => Getter GlobalState (Maybe (Selection Position))
gsCurrentSelection = to (^? gsContributionState . _Just . csCurrentSelectionWithPx . _Just . sstSelectionState)

gsRawContent :: Getter GlobalState RawContent
gsRawContent = to $ \case
  gs@(view gsDocumentState -> Just (DocumentStateDiff _ _ (eid :: ID Edit) _ _))
    -> maybe (cacheMissId eid rcHourglass) (^. editVDocVersion) (cacheLookup gs eid)
  (view gsCompositeVDoc -> Just (Just cvdoc))
    -> rawContentFromCompositeVDoc cvdoc
  _ -> rcHourglass
  where
    rcHourglass = mkRawContent $ mkBlock hourglass :| []


-- * cache stuff

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

-- | prefer to use this instead of 'cacheMiss'
cacheMissId :: CacheLookup v => ID v -> a -> a
cacheMissId i x = cacheMiss (cacheKey i) x i

class CacheLookup a where
  cacheKey :: ID a -> CacheKey
  cacheLens :: Lens' ServerCache (Map (ID a) a)

cacheLookupIn :: CacheLookup a => ServerCache -> ID a -> Maybe a
cacheLookupIn sc i@(cacheKey -> k) = maybe (cacheMiss k Nothing i) Just . Map.lookup i $ sc ^. cacheLens

cacheLookup :: (HasCallStack, CacheLookup b) => GlobalState_ a -> ID b -> Maybe b
cacheLookup gs = cacheLookupIn (gs ^. gsServerCache)

-- prefer to use this instead of cacheLookup; FIXME: give a better name
cacheLookup' :: (HasCallStack, CacheLookup b) => GlobalState_ a -> ID b -> Lookup b
cacheLookup' gs i = maybe (Left i) Right $ cacheLookup gs i

type CacheLookupT = Except ()

-- FIXME: collect more missing cache keys in one round
cacheLookupM :: (HasCallStack, CacheLookup b) => GlobalState_ a -> ID b -> CacheLookupT b
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

instance CacheLookup Discussion where
  cacheKey = CacheKeyDiscussion
  cacheLens = scDiscussions

instance CacheLookup User where
  cacheKey = CacheKeyUser
  cacheLens = scUsers

instance CacheLookup Group where
  cacheKey = CacheKeyGroup
  cacheLens = scGroups


-- * user stuff

-- | beware of cyclical imports!  do not more this to Login.Types or Login.Status!
-- See also: https://github.com/ghcjs/ghcjs/issues/267
onLoginClick :: CurrentUser (Lookup User) -> [GlobalAction]
onLoginClick UserLoggedOut              = [MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)]
onLoginClick (UserLoggedIn (Left _uid)) = []
onLoginClick (UserLoggedIn (Right usr)) = [MainMenuAction $ MainMenuActionOpen (MainMenuProfile (usr ^. userID, formstate))]
  where
    formstate = FormBegin $ newLocalStateRef (Nothing, Nothing) usr
