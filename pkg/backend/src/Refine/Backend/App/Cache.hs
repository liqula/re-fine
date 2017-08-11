{-# LANGUAGE NoImplicitPrelude          #-}
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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.App.Cache where

import Refine.Backend.Prelude as P

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Network.WebSockets
import           Control.Concurrent
import           System.IO.Unsafe

import Refine.Backend.Database
import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import Refine.Common.Types

type CacheId = Int
type WebSocketMVar = MVar (Int{-to generate fresh CacheIds-}, Map CacheId (Connection, Set CacheKey))

-- FIXME: store this in state
{-# NOINLINE webSocketMVar #-}
webSocketMVar :: WebSocketMVar
webSocketMVar = unsafePerformIO $ newMVar (0, mempty)

sendCache :: Connection -> ServerCache -> IO ()
sendCache conn cache = sendTextData conn (cs . encode $ Just cache :: ST)

getData' :: CacheKey -> App ServerCache
getData' = \case
  CacheKeyVDoc i       -> App.getVDoc i       <&> \val -> ServerCache (Map.singleton i val) mempty mempty mempty mempty mempty
  CacheKeyEdit i       -> App.getEdit i       <&> \val -> ServerCache mempty (Map.singleton i val) mempty mempty mempty mempty
  CacheKeyNote i       -> App.getNote i       <&> \val -> ServerCache mempty mempty (Map.singleton i val) mempty mempty mempty
  CacheKeyDiscussion i -> App.getDiscussion i <&> \val -> ServerCache mempty mempty mempty (Map.singleton i val) mempty mempty
  CacheKeyUser i       -> App.getUser i       <&> \val -> ServerCache mempty mempty mempty mempty (Map.singleton i val) mempty
  CacheKeyGroup i      -> App.getGroup i      <&> \val -> ServerCache mempty mempty mempty mempty mempty (Map.singleton i val)

-- filters the cache
restrictCache :: Set CacheKey -> ServerCache -> ServerCache
restrictCache (Set.toList -> keys) (ServerCache a b c d e f)
  = ServerCache
     (restrictKeys a [i | CacheKeyVDoc i       <- keys])
     (restrictKeys b [i | CacheKeyEdit i       <- keys])
     (restrictKeys c [i | CacheKeyNote i       <- keys])
     (restrictKeys d [i | CacheKeyDiscussion i <- keys])
     (restrictKeys e [i | CacheKeyUser i       <- keys])
     (restrictKeys f [i | CacheKeyGroup i      <- keys])
  where
    restrictKeys m ks = Map.filterWithKey (\k _ -> Set.member k ks') m where ks' = Set.fromList ks

instance Database db => MonadCache (AppM db) where
    invalidateCaches keys = do
        cache <- mconcat <$> mapM getData' (Set.toList keys)
        cmap <- liftIO $ snd <$> readMVar webSocketMVar
        let cls = [(n, conn, d) | (n, (conn, s)) <- Map.assocs cmap, let d = Set.intersection keys s, not $ Set.null d]
        appLog $ "invalidateCaches " <> show keys <> " " <> show ((\(n, _, d) -> (n, d)) <$> cls)
        liftIO . forM_ cls $ \(_n, conn, d) -> do
            sendCache conn $ restrictCache d cache
