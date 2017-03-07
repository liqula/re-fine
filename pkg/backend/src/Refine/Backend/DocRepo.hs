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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.DocRepo
  ( module Refine.Backend.DocRepo.Core
  , module Refine.Backend.DocRepo.Class
  , createRepoNat
  , runDocRepo
  ) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader (runReaderT)
import Control.Natural

import Refine.Backend.Config
import Refine.Backend.DocRepo.Class
import Refine.Backend.DocRepo.Core
import Refine.Backend.DocRepo.Darcs as Darcs


createRepoNat :: Config -> IO (DocRepo :~> ExceptT DocRepoError IO)
createRepoNat cfg = pure $
  Nat (wrapErrors . runDocRepo cfg)
  where
    wrapErrors :: IO (Either DocRepoError a) -> ExceptT DocRepoError IO a
    wrapErrors m = do
      r <- liftIO (try m)
      either (throwError . DocRepoException . show @SomeException)
             (either throwError pure) r

runDocRepo :: Config -> DocRepo a -> IO (Either DocRepoError a)
runDocRepo cfg = flip runReaderT cfg . runExceptT . unDocRepo

instance DocumentRepository DocRepo where
  createRepo         = Darcs.createRepo
  createEdit         = Darcs.createEdit
  createInitialEdit  = Darcs.createInitialEdit
  getVersion         = Darcs.getVersion
  getChildEdits      = Darcs.getChildEdits
