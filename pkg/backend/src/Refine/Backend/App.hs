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

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Natural
import System.FilePath (FilePath)

import Refine.Backend.App.Comment     as App
import Refine.Backend.App.Core        as App
import Refine.Backend.App.Group       as App
import Refine.Backend.App.Process     as App
import Refine.Backend.App.Role        as App
import Refine.Backend.App.Translation as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import Refine.Backend.Logger
import Refine.Backend.Types (CsrfSecret)
import Refine.Prelude


runApp
  :: forall (db :: * -> *) (uh :: * -> *)
  .  MkDBNat db
  -> DBRunner
  -> UHNat uh
  -> Logger
  -> CsrfSecret
  -> Timespan
  -> FilePath
  -> (forall a . AppM db uh a -> AppM db uh a)
  -> (AppM db uh :~> ExceptT AppError IO)
runApp
  dbNat
  dbrunner
  uhNat
  logger
  csrfSecret
  sessionLength
  poFilesRoot
  wrapper =
    NT (runSR . unApp . wrapper)
    where
      runSR
        :: StateT AppState (ReaderT (AppContext db uh) (ExceptT AppError IO)) x
        -> ExceptT AppError IO x
      runSR m = do
        unDBRunner dbrunner $ \dbc -> do
          dbInit dbc
          let r = AppContext dbNat dbc uhNat logger csrfSecret sessionLength poFilesRoot
              s = AppState Nothing UserLoggedOut
          x <- runReaderT (evalStateT m s) r
               `finally`
               dbCommit dbc
          pure x
