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
import Refine.Backend.App.Role        as App
import Refine.Backend.App.Translation as App
import Refine.Backend.App.User        as App
import Refine.Backend.App.VDoc        as App
import Refine.Backend.Logger
import Refine.Backend.Types (CsrfSecret)
import Refine.Prelude


runApp
  :: forall (db :: * -> *) (uh :: * -> *)
  .  DBNat db
  -> DocRepoNat
  -> UHNat uh
  -> Logger
  -> CsrfSecret
  -> Timespan
  -> FilePath
  -> (forall a . AppM db uh a -> AppM db uh a)
  -> (AppM db uh :~> ExceptT AppError IO)
runApp
  dbNat
  docRepoNat
  uhNat
  logger
  csrfSecret
  sessionLength
  poFilesRoot
  wrapper =
    Nat (runSR
            (AppState Nothing UserLoggedOut)
            (AppContext dbNat docRepoNat uhNat logger csrfSecret sessionLength poFilesRoot)
          . unApp
          . wrapper)
    where
      runSR :: (Monad m) => s -> r -> StateT s (ReaderT r m) a -> m a
      runSR s r m = runReaderT (evalStateT m s) r
