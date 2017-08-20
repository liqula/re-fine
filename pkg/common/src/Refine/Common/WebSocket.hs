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

module Refine.Common.WebSocket where

import Refine.Common.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Refine.Common.Rest (ApiError)
import Refine.Common.Types.Core
import Refine.Common.Types.Prelude
import Refine.Common.Types.Translation
import Refine.Common.Types.Vote


-- | FIXME: should be renamed to 'SessionId'.
type CacheId = Int


-- ** Server cache

data ServerCache = ServerCache
  { _scVDocs       :: Map (ID VDoc)       VDoc
  , _scEdits       :: Map (ID Edit)       Edit
  , _scNotes       :: Map (ID Note)       Note
  , _scDiscussions :: Map (ID Discussion) Discussion
  , _scUsers       :: Map (ID User)       User
  , _scGroups      :: Map (ID Group)      Group
  , _scGroupIds    :: Maybe (Set (ID Group))
  }
  deriving (Show, Eq, Generic)

instance Monoid ServerCache where
  mempty = ServerCache mempty mempty mempty mempty mempty mempty Nothing
  ServerCache a b c d e f g `mappend` ServerCache a' b' c' d' e' f' g'
    = ServerCache (a <> a') (b <> b') (c <> c') (d <> d') (e <> e') (f <> f') (g <|> g')

data CacheKey
  = CacheKeyVDoc       (ID VDoc)
  | CacheKeyEdit       (ID Edit)
  | CacheKeyNote       (ID Note)
  | CacheKeyDiscussion (ID Discussion)
  | CacheKeyUser       (ID User)
  | CacheKeyGroup      (ID Group)
  | CacheKeyGroupIds
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
  | TSAddNote (ID Edit) (CreateNote (Maybe (Range Position)))
  | TSAddStatement (ID Statement){-parent-} CreateStatement
  | TSCreateUser CreateUser
  | TSLogin Login
  | TSLogout
  | TSGetTranslations GetTranslations

  | TSGreeting (Maybe CacheId)  -- ^ first message on connect with 'Nothing'; if this is a
                                -- re-connect, send @'Just' 'CacheId'@.
  | TSPing
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

  | TCGreeting CacheId           -- ^ first message on connect
  | TCPing                       -- ^ (this could be done more easily with 'sendPing', 'forkPingThread'.)
  deriving (Eq, Show, Generic)

-- filters the cache
restrictCache :: Set CacheKey -> ServerCache -> ServerCache
restrictCache = invalidateOrRestrictCache False

invalidateCache :: Set CacheKey -> ServerCache -> ServerCache
invalidateCache = invalidateOrRestrictCache True

invalidateOrRestrictCache :: Bool -> Set CacheKey -> ServerCache -> ServerCache
invalidateOrRestrictCache invalidate (Set.toList -> keys) (ServerCache a b c d e f g)
  = ServerCache
     (restrictKeys a [i | CacheKeyVDoc i       <- keys])
     (restrictKeys b [i | CacheKeyEdit i       <- keys])
     (restrictKeys c [i | CacheKeyNote i       <- keys])
     (restrictKeys d [i | CacheKeyDiscussion i <- keys])
     (restrictKeys e [i | CacheKeyUser i       <- keys])
     (restrictKeys f [i | CacheKeyGroup i      <- keys])
     (if (CacheKeyGroupIds `elem` keys) == invalidate then Nothing else g)
  where
    restrictKeys :: Ord a => Map a b -> [a] -> Map a b
    restrictKeys m ks = Map.filterWithKey (\k _ -> Set.notMember k ks' == invalidate) m
      where ks' = Set.fromList ks

deriveClasses [([''ServerCache, ''CacheKey, ''ToServer, ''ToClient], allClass)]
