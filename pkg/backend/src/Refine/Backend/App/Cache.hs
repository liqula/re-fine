{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.App.Cache
  ( startWebSocketServer
  ) where

import Refine.Backend.Prelude as P

import           Control.Concurrent
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Network.Wai (Middleware)
import qualified Network.Wai.Session as SCS
import qualified Servant.Cookie.Session as SCS
import           System.IO.Unsafe
import           Network.WebSockets
import           Network.Wai.Handler.WebSockets
import           Web.Cookie (parseCookies)

import Refine.Common.Types
import Refine.Common.Rest (ApiError)
import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import Refine.Backend.App.Translation as App
import Refine.Backend.Config
import Refine.Backend.Database

type WebSocketMVar = MVar (Int{-to generate fresh CacheIds-}, Map CacheId (Connection, Set CacheKey))

-- FIXME: store this in state
{-# NOINLINE webSocketMVar #-}
webSocketMVar :: WebSocketMVar
webSocketMVar = unsafePerformIO $ newMVar (0, mempty)

sendMessage :: Connection -> ToClient -> IO ()
sendMessage conn msg = sendTextData conn (cs $ encode msg :: ST)

receiveMessage :: Connection -> IO ToServer
receiveMessage conn = receiveData conn <&> fromMaybe (error "websockets json decoding error") . decode

getData :: CacheKey -> App ServerCache
getData = \case
  CacheKeyVDoc i       -> App.getVDoc i       <&> \val -> mempty & scVDocs       .~ Map.singleton i val
  CacheKeyEdit i       -> App.getEdit i       <&> \val -> mempty & scEdits       .~ Map.singleton i val
  CacheKeyNote i       -> App.getNote i       <&> \val -> mempty & scNotes       .~ Map.singleton i val
  CacheKeyDiscussion i -> App.getDiscussion i <&> \val -> mempty & scDiscussions .~ Map.singleton i val
  CacheKeyUser i       -> App.getUser i       <&> \val -> mempty & scUsers       .~ Map.singleton i val
  CacheKeyGroup i      -> App.getGroup i      <&> \val -> mempty & scGroups      .~ Map.singleton i val
  CacheKeyGroupIds     -> App.getGroups       <&> \val -> mempty & scGroupIds    .~ Just (Set.fromList $ (^. groupID) <$> val)


instance Database db => MonadCache (AppM db) where
    invalidateCaches keys = do
        cmap <- liftIO $ takeMVar webSocketMVar
        let cmap' = second (second (Set.\\ keys) <$>) cmap
            cls = [(n, conn, d) | (n, (conn, s)) <- Map.assocs $ snd cmap, let d = Set.intersection keys s, not $ Set.null d]
        appLog $ "invalidate cache items " <> show (Set.toList keys)
        forM_ cls $ \(n, conn, d) -> do
            liftIO . sendMessage conn . TCInvalidateKeys $ Set.toList d
            appLog $ "invalidate cache of client #" <> show n <> ": " <> show (Set.toList d)
        liftIO $ putMVar webSocketMVar cmap'

-- | Properties (checked by `assert`):
--
--  * there is a cookie in the connect request.
--  * that cookie string does not change throughout the session (same caveat as above).
--
-- FIXME: delete disconnected clients' data
startWebSocketServer :: (forall a. AppM DB a -> IO (Either ApiError a))
                     -> SCS.SessionStore IO () (SCS.Lockable AppState) -> SBS
                     -> Middleware
startWebSocketServer toIO sessionStore cookieName = websocketsOr options server
  where
    options :: ConnectionOptions
    options = defaultConnectionOptions

    server :: ServerApp
    server pendingconnection = do
      let rawCookie :: SBS
          rawCookie = search . requestHeaders . pendingRequest $ pendingconnection
            where
              -- (assertion may fail once there is no ajax call before the first websockets connect
              -- any more)
              search :: Network.WebSockets.Headers -> SBS
              search hs = assert (isJust v) (fromJust v)
                where v = lookup cookieName . parseCookies =<< lookup "Cookie" hs

          getSession :: MonadIO m => m (SCS.Session IO () (SCS.Lockable AppState), IO SBS)
          getSession = liftIO $ sessionStore (Just rawCookie)

          loadAppState :: AppM DB ()
          loadAppState = getSession >>= \case
            ((goget, _), _) -> maybe (pure ()) (put . fst) =<< liftIO (goget ())

          storeAppState :: AppM DB ()
          storeAppState = getSession >>= \case
            ((goget, goset), getNewRawCookie) -> do
              Just (_, lock) <- liftIO $ goget ()
              get >>= liftIO . goset () . (, lock)

              -- FIXME: we probably don't want to spend time on the next line in production.  (on the
              -- other hand, this assertion may fail in teh future, see comment above near
              -- `search`.)
              liftIO $ getNewRawCookie >>= \newRawCookie -> assert (rawCookie == newRawCookie) $ pure ()

      conn <- acceptRequest pendingconnection

      clientId <- receiveMessage conn >>= \case
        -- the client is reconnected, its CacheId is n
        TSGreeting (Just n) -> do
          cmap <- takeMVar webSocketMVar
          putMVar webSocketMVar $ second (Map.adjust (const (conn, mempty)) n) cmap
          sendMessage conn $ TCRestrictKeys []
          appLog $ "websocket client #" <> show n <> " is reconnected"
          pure n

        -- the first connection of the client
        TSGreeting Nothing -> do
          (n, cmap) <- takeMVar webSocketMVar
          putMVar webSocketMVar (n + 1, Map.insert n (conn, mempty) cmap)
          sendMessage conn $ TCGreeting n
          appLog $ "new websocket client #" <> show n
          pure n

        _ -> error "websocket communication protocol failure"

      toIO (asks . view $ appConfig . cfgWSPingPeriod) >>= \case
        Left e -> error $ show e
        Right pingFreq -> void . forkIO . forever $ do
          threadDelay (timespanUs pingFreq)
          sendMessage conn TCPing

      let wrap :: AppM DB () -> IO ()
          wrap m = toIO ((loadAppState >> m) `finally` storeAppState) >>= \case
            Left e -> appLogL LogError $ show e
            Right () -> pure ()

      forever . wrap $ do
        msg <- liftIO $ receiveMessage conn
        appLog $ "request from client #" <> show clientId <> ", " <> show msg
        appLogL LogDebug . ("appState = " <>) . show =<< get

        case msg of
            TSPing -> appLog $ show clientId <> ": ping."
            TSClearCache -> do
                cmap' <- liftIO $ takeMVar webSocketMVar
                liftIO . putMVar webSocketMVar $
                  second (Map.adjust (second $ const mempty) clientId) cmap'
                liftIO . sendMessage conn $ TCRestrictKeys []
            TSMissing keys -> do
                cache <- mconcat <$> mapM getData keys
                cmap' <- liftIO $ takeMVar webSocketMVar
                liftIO . putMVar webSocketMVar $
                  second (Map.adjust (second (<> Set.fromList keys)) clientId) cmap'
                liftIO . sendMessage conn $ TCServerCache cache

            TSAddGroup cg           -> liftIO . sendMessage conn . TCCreatedGroup . view groupID =<< App.addGroup cg
            TSUpdateGroup gid x     -> void $ App.modifyGroup gid x
            TSCreateUser cu         -> liftIO . sendMessage conn . TCCreateUserResp =<< tryApp (App.createUser cu)
            TSLogin li              -> liftIO . sendMessage conn . TCLoginResp =<< tryApp (App.login li)
            TSLogout                -> void App.logout
            TSGetTranslations k     -> liftIO . sendMessage conn . TCTranslations =<< App.getTranslations k

            TSAddVDoc cv            -> liftIO . sendMessage conn . TCCreatedVDoc . view vdocID =<< App.createVDoc cv
            TSUpdateVDoc vid upd    -> void $ App.updateVDoc vid upd
            TSAddDiscussion eid x   -> void $ App.addDiscussion eid x
            TSAddStatement sid x    -> void $ App.addStatement sid x
            TSUpdateStatement sid x -> void $ App.updateStatement sid x
            TSAddNote eid x         -> void $ App.addNote eid x
            TSAddEdit eid x         -> void $ App.addEdit eid x
            TSAddEditAndMerge eid x -> void $ App.addEditAndMerge eid x
            TSUpdateEdit eid x      -> void $ App.updateEdit eid x
            TSMergeEdit eid         -> void $ App.mergeEdit eid
            TSToggleVote (ContribIDEdit eid) x -> do
              rebased <- App.toggleSimpleVoteOnEdit eid x
              when rebased . liftIO $ sendMessage conn TCRebase
            TSToggleVote (ContribIDNote nid) x
                                    -> void $ App.toggleSimpleVoteOnNote nid x
            TSToggleVote (ContribIDDiscussion _) _x
                                    -> pure ()  -- not implemented
            TSDeleteVote eid        -> void $ App.deleteSimpleVoteOnEdit eid

            TSCookie ck             -> assert ((lookup cookieName . parseCookies . cs $ ck) == Just rawCookie) pure ()
                                         -- FIXME: if assertion holds, we should probably remove 'TSCookie'.

            bad@(TSGreeting _)      -> error $ "websocket communication protocol failure:" <> show bad
