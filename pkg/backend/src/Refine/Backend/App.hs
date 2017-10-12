{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.App
  ( module App
  , RunApp(..), runApp, runApp', runAppTC
  , runToServer
  ) where
#include "import_backend.hs"

import System.Timeout

import Refine.Common.WebSocket
import Refine.Common.Types
import Refine.Backend.App.Access      as App
import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.Role        as App
import Refine.Backend.App.Smtp        as App
import Refine.Backend.App.Translation as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import Refine.Backend.App.Cache       as App
import Refine.Backend.Config
import Refine.Backend.Database.Class (Database)
import Refine.Backend.Logger


data RunApp db = RunApp
  { _runAppConfig           :: Config
  , _runAppMkDBNat          :: MkDBNat db
  , _runAppDBRunner         :: DBRunner
  , _runAppLogChan          :: LogChan
  , _runAppCacheInvalidateQ :: TChan (Set CacheKey)
  }

runApp :: forall db a. Database db => RunApp db -> TChan ToClient -> AppM db a -> ExceptT ApiError IO a
runApp (RunApp cfg dbNat dbrunner logchan cachechan) clientchan =
  limitDuration cfg . logDuration logchan . runSR
  where
    runSR :: AppM db a -> ExceptT ApiError IO a
    runSR (AppM action) = unDBRunner dbrunner $ \dbc -> do
      dbInit dbc
      let action' = evalStateT action $ initialAppState cfg
          action'' = action' `runReaderT` (dbNat, ctx)

          ctx :: AppContext
          ctx = AppContext dbc logchan clientchan cachechan cfg

      -- iff exception, rollback; otherwise, commit.
      -- https://www.sqlite.org/lang_transaction.html  -- TODO: is this the same on postgres?
      (twistException logchan (action'' <* dbCommit dbc)
        `catchError` \e -> dbRollback dbc >> throwError e)
        >>= liftIO . evaluate -- without evaluate we have issue #389

-- | Like 'runApp', but throw the exceptions in the result via @throwIO . ErrorCall . show@.
runApp' :: forall db a. Database db => RunApp db -> TChan ToClient -> AppM db a -> IO a
runApp' rapp clientchan app
  = either (throwIO . ErrorCall . show) pure
     =<< runExceptT (runApp rapp clientchan app)

-- | Like 'runApp', but flush the 'ToClient' chan and return it with exception and original result
-- as a value.
runAppTC :: forall db a. Database db => RunApp db -> AppM db a -> IO ([ToClient], Either ApiError a)
runAppTC rapp app = do
  clientchan <- newTChanIO
  result <- runExceptT $ runApp rapp clientchan app
  let finallyflush c = atomically (tryReadTChan c) >>= \case
        Nothing -> pure []
        Just e  -> (e:) <$> finallyflush c
  (, result) <$> finallyflush clientchan


-- | translate 'AppError' to 'ApiError', and also make sure that any exceptions thrown in the
-- inner 'IO' are caught and rethrown in the 'ExceptT' as 'ApiError' as well.
--
-- FUTUREWORK: this is very simliar to wrapErrors in 'createDBNat'.  there is probably a more
-- straight-forward way to implement this.
twistException :: LogChan -> ExceptT AppError IO a -> ExceptT ApiError IO a
twistException logchan (ExceptT m) = ExceptT $ twist =<< protect m
  where
    twist :: Either AppError a -> IO (Either ApiError a)
    twist (Right v) = pure (Right v)
    twist (Left e)  = Left <$> toApiError e `runReaderT` logchan

    protect :: IO (Either AppError a) -> IO (Either AppError a)
    protect = (`catch` \(SomeException e) -> pure . Left . AppUnknownError . cs . show $ e)


limitDuration :: Config -> ExceptT ApiError IO a -> ExceptT ApiError IO a
limitDuration ((^. cfgAppMLimit) -> timespan) (ExceptT ioaction) = ExceptT $
  fromMaybe (Left $ ApiTimeoutError timespan) <$>
  timeout (timespanUs timespan) ioaction

logDuration :: LogChan -> ExceptT ApiError IO a -> ExceptT ApiError IO a
logDuration logchan (ExceptT action) = ExceptT $ do
  starttime <- getCurrentTime
  let logit = do
        endtime <- getCurrentTime
        let msg = "runApp: call took " <> show (endtime `diffUTCTime` starttime)
        liftIO $ appLog LogDebug msg `runReaderT` logchan
  action <* logit


-- * the 'ToServer' interpreter

-- | TUNING: 'verifyAppState' should not be called every request, it's expensive.  perhaps it's best
-- to not use 'users' after all, and do something closer to what we did in aula.
runToServer :: ToServer -> MonadApp m => m ()
runToServer msg = do
  verifyAppState >>= (`unless` throwError AppSessionInvalid)
  appLog LogDebug . ("appState = " <>) . show =<< get

  case msg of
      TSClearCache -> clearCache
      TSMissing keys -> fetchCache $ Set.fromList keys

      TSAddGroup cg           -> sendToClient . TCCreatedGroup . view groupID =<< App.addGroup cg
      TSUpdateGroup gid x     -> void $ App.modifyGroup gid x
      TSCreateUser cu         -> sendToClient . TCCreateUserResp =<< tryApp (App.createUser cu)
      TSUpdateUser uid x      -> updateUser uid x
      TSLogin li              -> sendToClient . TCLoginResp =<< tryApp (App.login li)
      TSLogout                -> void App.logout
      TSGetTranslations k     -> sendToClient . TCTranslations =<< App.getTranslations k

      TSAddVDoc cv            -> sendToClient . TCCreatedVDoc . view vdocID =<< App.createVDoc cv
      TSUpdateVDoc vid upd    -> void $ App.updateVDoc vid upd
      TSAddDiscussion eid x   -> void $ App.addDiscussion eid x
      TSAddStatement sid x    -> void $ App.addStatement sid x
      TSUpdateStatement sid x -> void $ App.updateStatement sid x
      TSAddEdit eid x         -> void $ App.addEdit eid x
      TSAddEditAndMerge eid x -> void $ App.addEditAndMerge eid x
      TSUpdateEdit eid x      -> void $ App.updateEdit eid x
      TSMergeEdit eid         -> void $ App.mergeEditAndRebaseAllSiblings eid
      TSToggleVote (ContribIDEdit eid) x -> do
        rebased <- App.toggleSimpleVoteOnEdit eid x
        when rebased $ sendToClient TCRebase
      TSToggleVote (ContribIDDiscussion _ nid) x
                              -> void $ App.toggleSimpleVoteOnDiscussion nid x
      TSDeleteVote eid        -> void $ App.deleteSimpleVoteOnEdit eid
