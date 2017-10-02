{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.App
  ( module App
  , runApp
  ) where
#include "import_backend.hs"

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
import Refine.Common.Rest (ApiError)


runApp
  :: forall db. Database db
  => MkDBNat db
  -> DBRunner
  -> Logger
  -> Config
  -> (AppM db :~> ExceptT ApiError IO)
runApp
  dbNat
  dbrunner
  logger
  cfg
  = NT (logDuration logger . runSR . unApp)
    where
      runSR
        :: forall a. StateT AppState (ReaderT (MkDBNat db, AppContext) (ExceptT AppError IO)) a
        -> ExceptT ApiError IO a
      runSR action = unDBRunner dbrunner $ \dbc -> do
              dbInit dbc
              let action' = evalStateT action $ initialAppState cfg
                  action'' = action' `runReaderT` ctx
                  ctx = (dbNat, AppContext dbc logger cfg)

              -- iff exception, rollback; otherwise, commit.
              -- https://www.sqlite.org/lang_transaction.html
              twistException (action'' <* dbCommit dbc)
                `catchError` \e -> dbRollback dbc >> throwError e

      -- translate 'AppError' to 'ApiError', and also make sure that any exceptions thrown in the
      -- inner 'IO' are caught and rethrown in the 'ExceptT' as 'ApiError' as well.
      --
      -- FUTUREWORK: this is very simliar to wrapErrors in 'createDBNat'.  there is probably a more
      -- straight-forward way to implement this.
      twistException :: ExceptT AppError IO a -> ExceptT ApiError IO a
      twistException (ExceptT m) = ExceptT $ twist =<< protect m
        where
          twist :: Either AppError a -> IO (Either ApiError a)
          twist (Right v) = pure (Right v)
          twist (Left e)  = Left <$> toApiError e `runReaderT` logger

          protect :: IO (Either AppError a) -> IO (Either AppError a)
          protect = (`catch` \(SomeException e) -> pure . Left . AppUnknownError . cs . show $ e)


logDuration :: Logger -> ExceptT ApiError IO a -> ExceptT ApiError IO a
logDuration logger (ExceptT action) = ExceptT $ do
  starttime <- getCurrentTime
  let logit = do
        endtime <- getCurrentTime
        let msg = "runApp: call took " <> show (endtime `diffUTCTime` starttime)
        liftIO $ appLog LogDebug msg `runReaderT` logger
  action <* logit
