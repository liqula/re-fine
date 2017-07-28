{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
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

module Refine.Backend.App
  ( module App
  , runApp
  ) where

import Refine.Backend.Prelude

import System.FilePath (FilePath)

import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.Role        as App
import Refine.Backend.App.Translation as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import Refine.Backend.Logger
import Refine.Backend.Types (CsrfSecret)


runApp
  :: forall (db :: * -> *)
  .  MkDBNat db
  -> DBRunner
  -> Logger
  -> CsrfSecret
  -> Timespan
  -> FilePath
  -> (AppM db :~> ExceptT AppError IO)
runApp
  dbNat
  dbrunner
  logger
  csrfSecret
  sessionLength
  poFilesRoot
  = NT (runSR . unApp)
    where
      runSR
        :: StateT AppState (ReaderT (AppContext db) (ExceptT AppError IO)) x
        -> ExceptT AppError IO x
      runSR m = do
        unDBRunner dbrunner $ \dbc -> do
          dbInit dbc
          let r = AppContext dbNat dbc logger csrfSecret sessionLength poFilesRoot
              s = AppState Nothing UserLoggedOut
          x <- runReaderT (evalStateT m s) r
               `finally`
               dbCommit dbc
          pure x
