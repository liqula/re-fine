{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.WebSocket where
#include "import_common.hs"

import Refine.Common.Rest (ApiError)
import Refine.Common.Types.Core
import Refine.Common.Types.Prelude
import Refine.Common.Types.Translation
import Refine.Common.Types.Vote


-- | (it would be nice if we could re-use the session id from 'AppUserState', but that's only
-- available when logged in, and web sockets need to work for anonymous users, too.)
newtype WSSessionId = WSSessionId { unWSSessionId :: ST }
  deriving (Eq, Ord, Show, Generic)


-- ** Server cache

data ServerCache = ServerCache
  { _scVDocs       :: Map (ID VDoc)       VDoc
  , _scEdits       :: Map (ID Edit)       Edit
  , _scDiscussions :: Map (ID Discussion) Discussion
  , _scUsers       :: Map (ID User)       User
  , _scGroups      :: Map (ID Group)      Group
  , _scGroupIds    :: Maybe (Set (ID Group))
  , _scUserIds     :: Maybe (Set (ID User))
  }
  deriving (Show, Eq, Generic)

instance Monoid ServerCache where
  mempty = ServerCache mempty mempty mempty mempty mempty mempty mempty
  ServerCache a b c d e f g `mappend` ServerCache a' b' c' d' e' f' g'
    = ServerCache (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <> g')

data CacheKey
  = CacheKeyVDoc       (ID VDoc)
  | CacheKeyEdit       (ID Edit)
  | CacheKeyDiscussion (ID Discussion)
  | CacheKeyUser       (ID User)
  | CacheKeyGroup      (ID Group)
  | CacheKeyGroupIds
  | CacheKeyUserIds
  deriving (Eq, Ord, Show, Generic)

data ToServer
  = TSMissing [CacheKey]
  | TSClearCache
  | TSUpdateVDoc (ID VDoc) UpdateVDoc
  | TSUpdateEdit (ID Edit) CreateEdit
  | TSUpdateGroup (ID Group) CreateGroup
  | TSUpdateStatement (ID Statement) CreateStatement
  | TSMergeEdit (ID Edit)
  | TSToggleVote ContributionID Vote
  | TSDeleteVote (ID Edit) -- not used yet
  | TSAddVDoc CreateVDoc
  | TSAddGroup CreateGroup
  | TSAddEdit (ID Edit){-parent-} CreateEdit
  | TSAddEditAndMerge (ID Edit){-parent-} CreateEdit
  | TSAddDiscussion (ID Edit) (CreateDiscussion (Maybe (Range Position)))
  | TSAddStatement (ID Statement){-parent-} CreateStatement
  | TSCreateUser CreateUser
  | TSUpdateUser (ID User) (Maybe ImageInline, ST)
  | TSLogin Login
  | TSLogout
  | TSGetTranslations GetTranslations

  | TSGreeting (Maybe WSSessionId)  -- ^ first message on connect with 'Nothing'; if this is a
                                -- re-connect, send @'Just' 'WSSessionId'@.
  deriving (Eq, Show, Generic)

data ToClient
  = TCServerCache ServerCache
  | TCInvalidateKeys [CacheKey]  -- ^ delete given keys from cache
  | TCRestrictKeys [CacheKey]    -- ^ intersect cache with given cache keys

  | TCCreatedVDoc (ID VDoc)      -- ^ follows an TSAddVDoc message
  | TCCreatedGroup (ID Group)    -- ^ follows an TSAddGroup message
  | TCRebase                     -- ^ follows a TSPutVote if rebase happened

  | TCCreateUserResp (Either ApiError User)  -- ^ response to 'TSCreateUser'
  | TCLoginResp (Either ApiError User)       -- ^ response to 'TSLogin'
  | TCTranslations L10                       -- ^ response to 'TSGetTranslations'

  | TCGreeting WSSessionId           -- ^ first message on connect
  | TCError ApiError
  deriving (Eq, Show, Generic)

-- filters the cache
restrictCache :: Set CacheKey -> ServerCache -> ServerCache
restrictCache = invalidateOrRestrictCache False

invalidateCache :: Set CacheKey -> ServerCache -> ServerCache
invalidateCache = invalidateOrRestrictCache True

invalidateOrRestrictCache :: Bool -> Set CacheKey -> ServerCache -> ServerCache
invalidateOrRestrictCache invalidate (Set.toList -> keys) (ServerCache a b d e f g u)
  = ServerCache
     (restrictKeys a [i | CacheKeyVDoc i       <- keys])
     (restrictKeys b [i | CacheKeyEdit i       <- keys])
     (restrictKeys d [i | CacheKeyDiscussion i <- keys])
     (restrictKeys e [i | CacheKeyUser i       <- keys])
     (restrictKeys f [i | CacheKeyGroup i      <- keys])
     (if (CacheKeyGroupIds `elem` keys) == invalidate then Nothing else g)
     (if (CacheKeyUserIds `elem` keys) == invalidate then Nothing else u)
  where
    restrictKeys :: Ord a => Map a b -> [a] -> Map a b
    restrictKeys m ks = Map.filterWithKey (\k _ -> Set.notMember k ks' == invalidate) m
      where ks' = Set.fromList ks

deriveClasses [([''ServerCache, ''CacheKey, ''ToServer, ''ToClient, ''WSSessionId], allClass)]
