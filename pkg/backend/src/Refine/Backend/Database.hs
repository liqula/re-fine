{-# LANGUAGE NoImplicitPrelude          #-}
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
  ( module DatabaseCore
  , MkDBNat
  , DBRunner(..)
  , DBConnection(..)
  , createDBNat
  ) where

import Refine.Backend.Prelude

import Control.Lens ((^.))
import Control.Monad.Logger
import Data.Pool (withResource)
import Database.Persist.Sqlite (SqlBackend, createSqlitePool, persistBackend, getStmtConn, connBegin, connRollback, connCommit)

import Refine.Backend.Config
import Refine.Backend.Database.Class as DatabaseCore
import Refine.Backend.Database.Core  as DatabaseCore
import Refine.Backend.Database.Schema()
import Refine.Backend.Database.Entity as Entity
import Refine.Backend.Database.Types as DatabaseCore


type MkDBNat db = DBConnection -> DBContext -> (db :~> ExceptT DBError IO)

newtype DBRunner = DBRunner { unDBRunner :: forall m a . MonadBaseControl IO m => (DBConnection -> m a) -> m a }

createDBNat :: Config -> IO (DBRunner, MkDBNat DB)
createDBNat cfg = do

  let sqliteDb = case cfg ^. cfgDBKind of
        DBInMemory  -> ":memory:"
        DBOnDisk fp -> fp

  pool <- runNoLoggingT $ createSqlitePool (cs sqliteDb) (cfg ^. cfgPoolSize)
  let dbConnectionCont :: (MonadBaseControl IO m) => (DBConnection -> m a) -> m a
      dbConnectionCont m = withResource pool (m . mkDBConnection)

  pure ( DBRunner dbConnectionCont
       , \dbc dbctx -> NT (wrapErrors . dbRun dbc . (`runReaderT` dbctx) . runExceptT . unDB)
       )
  where
    wrapErrors :: IO (Either DBError a) -> ExceptT DBError IO a
    wrapErrors =
      lift . try >=> either (throwError . DBException . show @SomeException)
                            (either throwError pure)

    -- Refactored from:
    -- https://hackage.haskell.org/package/persistent-2.6.1/docs/src/Database-Persist-Sql-Run.html#runSqlConn
    mkDBConnection :: SqlBackend -> DBConnection
    mkDBConnection conn =
      let conn'  = persistBackend conn
          getter = getStmtConn conn'
      in DBConnection
        { dbInit   = control $ \runInIO -> mask $ \restore -> do
                       restore $ connBegin conn' getter
                       runInIO $ pure ()
        , dbRun    = \r -> control $ \runInIO -> mask $ \restore -> do
                             onException
                               (restore (runInIO $ runReaderT r conn))
                               (restore (connRollback conn' getter))
        , dbCommit = do control $ \runInIO -> mask $ \restore -> do
                          restore $ connCommit conn' getter
                          runInIO $ pure ()
        }

data DBConnection = DBConnection
  { dbInit   :: forall m   . (MonadBaseControl IO m) => m ()
  , dbRun    :: forall m a . (MonadBaseControl IO m) => ReaderT SqlBackend m a -> m a
  , dbCommit :: forall m   . (MonadBaseControl IO m) => m ()
  }

instance Database DB where
  -- * VDoc
  listVDocs          = Entity.listVDocs
  createVDoc         = Entity.createVDoc
  updateVDoc         = Entity.updateVDoc
  getVDoc            = Entity.getVDoc
  vdocOfEdit         = Entity.vdocOfEdit
  getEditIDs         = Entity.getEditIDs
  moveVDocHead       = Entity.moveVDocHead

  -- * Edit
  createEdit         = Entity.createEdit
  getEdit            = Entity.getEdit
  getVersion         = Entity.getVersion
  editNotes          = Entity.editNotes
  editQuestions      = Entity.editQuestions
  editDiscussions    = Entity.editDiscussions
  getEditChildren    = Entity.getEditChildren
  updateVotes        = Entity.updateVotes
  getVoteCount       = Entity.getVoteCount
  updateEdit         = Entity.updateEdit
  updateEditSource   = Entity.updateEditSource

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
  createDiscussion       = Entity.createDiscussion
  rebaseDiscussion       = Entity.rebaseDiscussion
  getDiscussion          = Entity.getDiscussion
  statementsOfDiscussion = Entity.statementsOfDiscussion
  discussionOfStatement  = Entity.discussionOfStatement

  -- * Statement
  createStatement      = Entity.createStatement
  updateStatement      = Entity.updateStatement
  getStatement         = Entity.getStatement

  -- * User
  runUsersCmd        = Entity.runUsersCmd

  -- * Group
  createGroup       = Entity.createGroup
  getGroup          = Entity.getGroup
  getGroups         = Entity.getGroups
  modifyGroup       = Entity.modifyGroup
  removeGroup       = Entity.removeGroup
  addSubGroup       = Entity.addSubGroup
  removeSubGroup    = Entity.removeSubGroup

  -- * Role
  assignRole   = Entity.assignRole
  getRoles     = Entity.getRoles
  unassignRole = Entity.unassignRole

  createMetaID_ = Entity.createMetaID_
  getMetaID     = Entity.getMeta
