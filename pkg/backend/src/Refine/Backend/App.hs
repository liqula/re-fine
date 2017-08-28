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
import Refine.Backend.Logger
import Refine.Common.Rest (ApiError)


runApp
  :: forall (db :: * -> *)
  .  MkDBNat db
  -> DBRunner
  -> Logger
  -> Config
  -> (AppM db :~> ExceptT ApiError IO)
runApp
  dbNat
  dbrunner
  logger
  cfg
  = NT (runSR . unApp)
    where
      runSR
        :: forall a. StateT AppState (ReaderT (MkDBNat db, AppContext) (ExceptT AppError IO)) a
        -> ExceptT ApiError IO a
      runSR action = unDBRunner dbrunner $ \dbc -> do
              dbInit dbc
              let action' = evalStateT action $ initialAppState cfg
                  ctx = (dbNat, AppContext dbc logger cfg)
              twistException (action' `runReaderT` ctx)
                -- (always commit; rollback in case of error should already have happened here.)
                <* dbCommit dbc

      twistException :: ExceptT AppError IO a -> ExceptT ApiError IO a
      twistException (ExceptT m) = ExceptT $ either (fmap Left . toApiErrorWithLogger logger) (pure . Right) =<< m
