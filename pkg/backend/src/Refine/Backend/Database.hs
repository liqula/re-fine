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
  , DBNat
  , createDBNat
  ) where

import Control.Exception
import Control.Lens ((^.))
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Natural
import Data.String.Conversions (cs)
import Database.Persist.Sqlite
import Web.Users.Persistent as UserDB

import Refine.Backend.Config
import Refine.Backend.Database.Class
import Refine.Backend.Database.Core
import Refine.Backend.Database.Schema()
import Refine.Backend.Database.Entity as Entity

type DBNat db = DBContext -> (db :~> ExceptT DBError IO)

createDBNat :: Config -> IO (DBNat DB, UserDB.Persistent)
createDBNat cfg = do

  let sqliteDb = case cfg ^. cfgDBKind of
        DBInMemory  -> ":memory:"
        DBOnDisk fp -> fp

  pool <- runNoLoggingT $ createSqlitePool (cs sqliteDb) (cfg ^. cfgPoolSize)

  pure ( \dbctx -> Nat (wrapErrors . (`runSqlPool` pool) . (`runReaderT` dbctx) . runExceptT . unDB)
       , Persistent (`runSqlPool` pool)
       )
  where
    wrapErrors :: IO (Either DBError a) -> ExceptT DBError IO a
    wrapErrors =
      lift . try >=> either (throwError . DBException . show @SomeException)
                            (either throwError pure)

instance Database DB where
  -- * VDoc
  listVDocs          = Entity.listVDocs
  createVDoc         = Entity.createVDoc
  getVDoc            = Entity.getVDoc
  vdocRepo           = Entity.vdocRepo
  vdocRepoOfEdit     = Entity.vdocRepoOfEdit

  -- * Repo
  createRepo         = Entity.createRepo
  getRepo            = Entity.getRepo
  getRepoFromHandle  = Entity.getRepoFromHandle
  getRepoHandle      = Entity.getRepoHandle
  getEditIDs         = Entity.getEditIDs

  -- * Edit
  createEdit         = Entity.createEdit
  getEdit            = Entity.getEdit
  getEditFromHandle  = Entity.getEditFromHandle
  getEditHandle      = Entity.getEditHandle
  editNotes          = Entity.editNotes
  editQuestions      = Entity.editQuestions
  editDiscussions    = Entity.editDiscussions
  setEditChild       = Entity.setEditChild
  getEditChildren    = Entity.getEditChildren

  -- * Repo and edit
  editVDocRepo       = Entity.editVDocRepo

  -- * Note
  createNote         = Entity.createNote
  getNote            = Entity.getNote
  addNoteUserAccess  = Entity.addNoteUserAccess
  removeNoteUserAccess   = Entity.removeNoteUserAccess
  usersOfNote        = Entity.usersOfNote

  -- * Question
  createQuestion     = Entity.createQuestion
  getQuestion        = Entity.getQuestion
  addQuestionUserAccess    = Entity.addQuestionUserAccess
  removeQuestionUserAccess = Entity.removeQuestionUserAccess
  usersOfQuestion          = Entity.usersOfQuestion

  -- * Answer
  createAnswer       = Entity.createAnswer
  getAnswer          = Entity.getAnswer
  answersOfQuestion  = Entity.answersOfQuestion

  -- * Discussion
  createDiscussion   = Entity.createDiscussion
  getDiscussion      = Entity.getDiscussion
  statementsOfDiscussion = Entity.statementsOfDiscussion
  discussionOfStatement  = Entity.discussionOfStatement
  addDiscussionUserAccess    = Entity.addDiscussionUserAccess
  removeDiscussionUserAccess = Entity.removeDiscussionUserAccess
  usersOfDiscussion          = Entity.usersOfDiscussion

  -- * Statement
  createStatement      = Entity.createStatement
  getStatement         = Entity.getStatement

  -- * Group
  createGroup       = Entity.createGroup
  getGroup          = Entity.getGroup
  modifyGroup       = Entity.modifyGroup
  removeGroup       = Entity.removeGroup
  addSubGroup       = Entity.addSubGroup
  removeSubGroup    = Entity.removeSubGroup
