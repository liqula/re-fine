{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.App.Cache
  ( -- instance (Database db) => MonadCache (AppM db)
  ) where
#include "import_backend.hs"

import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.Translation as App (getTranslations)
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import Refine.Backend.Database
import Refine.Common.Types


instance (Database db) => MonadCache (AppM db) where
  -- send notification to central invalidation TChan about invalid items.
  invalidateCaches :: Set CacheKey -> AppM db ()
  invalidateCaches keys = do
    chan <- asks $ view appCacheInvalidationChan
    liftIO (atomically (writeTChan chan keys))

  -- ask client to destroy all cache items and and update 'AppState'.
  clearCache :: AppM db ()
  clearCache = do
    modify (appCacheState .~ mempty)
    sendToClient $ TCRestrictKeys []

  -- send fresh cache items to client and update 'AppState'.
  fetchCache :: Set CacheKey -> AppM db ()
  fetchCache keys = do
    modify (appCacheState %~ (<> keys))
    sendToClient . TCServerCache =<< (mconcat <$> mapM getData (Set.toList keys))
    where
      getData :: CacheKey -> App ServerCache
      getData = \case
        CacheKeyVDoc i       -> App.getVDoc i       <&> \val -> mempty & scVDocs       .~ Map.singleton i val
        CacheKeyEdit i       -> App.getEdit i       <&> \val -> mempty & scEdits       .~ Map.singleton i val
        CacheKeyDiscussion i -> App.getDiscussion i <&> \val -> mempty & scDiscussions .~ Map.singleton i val
        CacheKeyUser i       -> App.getUser i       <&> \val -> mempty & scUsers       .~ Map.singleton i val
        CacheKeyGroup i      -> App.getGroup i      <&> \val -> mempty & scGroups      .~ Map.singleton i val
        CacheKeyGroupIds     -> App.getGroups       <&> \val -> mempty & scGroupIds    .~ Just (Set.fromList $ (^. groupID) <$> val)
        CacheKeyUserIds      -> App.getUsers        <&> \val -> mempty & scUserIds     .~ Just val
