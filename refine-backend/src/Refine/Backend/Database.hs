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

module Refine.Backend.Database
  ( module Refine.Backend.Database.Class
  , module Refine.Backend.Database.Core
  , createDBRunner
  ) where

import Control.Exception
import Control.Lens ((^.))
import Control.Monad.Except
import Control.Monad.Logger
import Control.Natural
import Data.String.Conversions (cs)
import Database.Persist.Sqlite
import Web.Users.Persistent as UserDB

import Refine.Backend.Config
import Refine.Backend.Database.Class
import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema()
import Refine.Backend.Database.Entity as Entity


createDBRunner :: Config -> IO (DB :~> ExceptT DBError IO, UserDB.Persistent)
createDBRunner cfg = do

  let sqliteDb = case cfg ^. cfgDBKind of
        DBInMemory  -> ":memory:"
        DBOnDisk fp -> fp

  pool <- runNoLoggingT $ createSqlitePool (cs sqliteDb) (cfg ^. cfgPoolSize)

  pure ( Nat (wrapErrors . (`runSqlPool` pool) . runExceptT . unDB)
       , Persistent (`runSqlPool` pool)
       )
  where
    wrapErrors :: IO (Either DBError a) -> ExceptT DBError IO a
    wrapErrors =
      lift . try >=> either (throwError . DBException) (either throwError pure)

instance Database DB where
  -- * VDoc
  listVDocs          = Entity.listVDocs
  createVDoc         = Entity.createVDoc
  getVDoc            = Entity.getVDoc
  vdocRepo           = Entity.vdocRepo

  -- * Repo
  createRepo         = Entity.createRepo
  getRepo            = Entity.getRepo
  getRepoFromHandle  = Entity.getRepoFromHandle
  getRepoHandle      = Entity.getRepoHandle
  getPatchIDs        = Entity.getPatchIDs

  -- * Patch
  createPatch        = Entity.createPatch
  getPatch           = Entity.getPatch
  getPatchFromHandle = Entity.getPatchFromHandle
  getPatchHandle     = Entity.getPatchHandle
  patchNotes         = Entity.patchNotes
  patchQuestions     = Entity.patchQuestions
  patchDiscussions   = Entity.patchDiscussions

  -- * Repo and patch
  patchVDocRepo      = Entity.patchVDocRepo

  -- * Note
  createNote         = Entity.createNote
  getNote            = Entity.getNote

  -- * Question
  createQuestion     = Entity.createQuestion
  getQuestion        = Entity.getQuestion

  -- * Answer
  createAnswer       = Entity.createAnswer
  getAnswer          = Entity.getAnswer
  answersOfQuestion  = Entity.answersOfQuestion

  -- * Discussion
  createDiscussion   = Entity.createDiscussion
  getDiscussion      = Entity.getDiscussion
  statementsOfDiscussion = Entity.statementsOfDiscussion

  -- * Statement
  createStatement      = Entity.createStatement
  getStatement         = Entity.getStatement
