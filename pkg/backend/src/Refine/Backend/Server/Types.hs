{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.Server.Types where
#include "import_backend.hs"

import Refine.Backend.App as App
import Refine.Backend.Logger
import Refine.Common.Types


data Backend db = Backend
  { _backendServer           :: Either (Async ()) Application
                               -- ^ the http server (static content, web sockets, rest api, client config).
                               -- the 'Left' case is a handle for a plain ws server (for testing only).
  , _backendRunApp           :: RunApp db
                               -- ^ the application engine.  pass to 'runApp'.
  , _backendLogChan          :: LogChan
                               -- ^ configurable log sink.
  , _backendWSSessionMap     :: MVar WSSessionMap
                               -- ^ websocket sessions (can survive re-connects)
  , _backendCacheInvalidateQ :: TChan (Set CacheKey)
                               -- ^ this is where cache invalidation requests are picked up and
                               -- propagated to all appicable clients.
  , _backendCacheInvalidateS :: Async ()
                               -- ^ async thread that listens on '_backendCacheInvalidateQ'.
  , _backendSessionGCS       :: Async ()
                               -- ^ async thread that garbage-collects 'backendWSSessionMap'.
  }
  deriving (Generic)

type WSSessionMap = Map WSSessionId WSSession

-- | For every client, store a mutable 'AppState' and a 'TChan' for both communication directions.
-- (Disconnected clients are not likely to grow horribly large 'ToClient' queues as long as those
-- only contain cache invalidation messages for items still in the 'AppState's 'ServerCache'.)
data WSSession = WSSession
  { _wssAppState  :: MVar AppState
  , _wssToClientQ :: TChan ToClient
  , _wssLastSeen  :: MVar (Either Timestamp (Async ()))  -- ^ see 'withWSSession'
  }
  deriving (Generic)

deriveClasses [([''Backend, ''WSSession], [''Lens'])]
