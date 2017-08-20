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
import           System.IO.Unsafe
import           Network.WebSockets
import           Network.Wai.Handler.WebSockets

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

type WebSocketMVar = MVar (Int{-to generate fresh CacheIds-}, Map WSSessionId WebSocketSession)
type WebSocketSession = ((Connection, AppState), Set CacheKey)

-- | This is the place where all the session's 'AppState's are kept between websocket
-- connections.
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
            cls = [(n, conn, d) | (n, ((conn, _), s)) <- Map.assocs $ snd cmap, let d = Set.intersection keys s, not $ Set.null d]
        appLog $ "invalidate cache items " <> show (Set.toList keys)
        forM_ cls $ \(n, conn, d) -> do
            liftIO . sendMessage conn . TCInvalidateKeys $ Set.toList d
            appLog $ "invalidate cache of client #" <> show n <> ": " <> show (Set.toList d)
        liftIO $ putMVar webSocketMVar cmap'

-- | FIXME: delete disconnected clients' data
startWebSocketServer :: (forall a. AppM DB a -> IO (Either ApiError a))
                     -> Middleware
startWebSocketServer toIO = websocketsOr options server
  where
    options :: ConnectionOptions
    options = defaultConnectionOptions

    server :: ServerApp
    server pendingconnection = do
      conn <- acceptRequest pendingconnection
      clientId <- handshake conn
      pingLoop conn =<< toIO (asks . view $ appConfig . cfgWSPingPeriod)
      forever . cmdLoopStepFrame toIO $ cmdLoopStep conn clientId


data WSErrorUnexpectedPacket
  = WSErrorUnexpectedPacket ToServer
  | WSErrorInternal String
  deriving (Eq, Show)

fail' :: WSErrorUnexpectedPacket -> m a
fail' = error . show


handshake :: Connection -> IO WSSessionId
handshake conn = receiveMessage conn >>= \case
  -- the client is reconnected, its WSSessionId is n
  TSGreeting (Just n) -> do
    cmap <- takeMVar webSocketMVar
    putMVar webSocketMVar $ second (Map.adjust (_2 .~ mempty) n) cmap
    sendMessage conn $ TCRestrictKeys []
    appLog $ "websocket client #" <> show n <> " is reconnected"
    pure n

  -- the first connection of the client
  TSGreeting Nothing -> do
    (n, cmap) <- takeMVar webSocketMVar
    putMVar webSocketMVar (n + 1, Map.insert n ((conn, initialAppState), mempty) cmap)
    sendMessage conn $ TCGreeting n
    appLog $ "new websocket client #" <> show n
    pure n

  bad -> fail' $ WSErrorUnexpectedPacket bad

pingLoop :: Connection -> Either ApiError Timespan -> IO ()
pingLoop conn = \case
  Left e -> fail' . WSErrorInternal . show $ e
  Right pingFreq -> void . forkIO . forever $ do
    threadDelay (timespanUs pingFreq)
    sendMessage conn TCPing

cmdLoopStepFrame :: (AppM DB () -> IO (Either ApiError ())) -> AppM DB () -> IO ()
cmdLoopStepFrame toIO cmd = wrap cmd
  where
    wrap :: AppM DB () -> IO ()
    wrap m = toIO m >>= \case
    dolift cmd = toIO cmd >>= \case
      Left e -> appLogL LogError $ show e
      Right () -> pure ()

cmdLoopStep :: Connection -> WSSessionId -> AppM DB ()
cmdLoopStep conn clientId = do
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

      bad@(TSGreeting _)      -> fail' $ WSErrorUnexpectedPacket bad
