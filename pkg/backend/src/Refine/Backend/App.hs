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

import Refine.Backend.App.Comment as App
import Refine.Backend.App.Core    as App
import Refine.Backend.App.User    as App
import Refine.Backend.App.VDoc    as App
import Refine.Backend.Logger
import Refine.Backend.Types (CsrfSecret)
import Refine.Backend.User.Core (UserHandle)
import Refine.Prelude


runApp :: RunDB db -> RunDocRepo -> Logger -> UserHandle -> CsrfSecret -> Timespan -> App db :~> ExceptT AppError IO
runApp runDB runDocRepo logger userHandle csrfSecret sessionLength =
  Nat $ runSR (AppState Nothing UserLoggedOut) (AppContext runDB runDocRepo logger userHandle csrfSecret sessionLength) . unApp
  where
    runSR :: (Monad m) => s -> r -> StateT s (ReaderT r m) a -> m a
    runSR s r m = runReaderT (evalStateT m s) r
