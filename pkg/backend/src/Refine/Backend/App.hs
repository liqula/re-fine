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
  = NT (runSR . unApp . tryApp)
    where
      runSR
        :: forall a. StateT AppState
                       (ReaderT (MkDBNat db, AppContext)
                          (ExceptT AppError IO))
                       (Either ApiError a)
        -> ExceptT ApiError IO a
      runSR action = do
        -- construct *almost* the shape we need:
        let almost :: ExceptT AppError{- made impossible by 'tryApp' -} IO (Either ApiError a)
            almost = unDBRunner dbrunner $ \dbc -> do
              dbInit dbc
              let cmd = evalStateT action $ initialAppState cfg
                  ctx = (dbNat, AppContext dbc logger cfg)
              (cmd `runReaderT` ctx) `finally` dbCommit dbc

        -- twist the shape of 'almost' so the error falls into the right place:
        ExceptT $ runExceptT almost >>=
          either (error "impossible")              -- made impossible by 'tryApp'
            (either (pure . Left) (pure . Right))  -- this is where 'tryApp' has left the 'ApiError'.
