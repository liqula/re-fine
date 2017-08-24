{-# LANGUAGE CPP #-}
#include "language.hs"

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
import Data.Pool (Pool, withResource, destroyAllResources)
import Database.Persist.Sqlite (SqlBackend, createSqlitePool, persistBackend, getStmtConn, connBegin, connRollback, connCommit)

import Refine.Backend.Config
import Refine.Backend.Database.Class as DatabaseCore
import Refine.Backend.Database.Core  as DatabaseCore
import Refine.Backend.Database.Schema()
import Refine.Backend.Database.Entity as Entity
import Refine.Backend.Database.Types as DatabaseCore


type MkDBNat db = DBConnection -> DBContext -> (db :~> ExceptT DBError IO)

newtype DBRunner = DBRunner { unDBRunner :: forall m a . MonadBaseControl IO m => (DBConnection -> m a) -> m a }

createDBNat :: Config -> IO (DBRunner, MkDBNat DB, IO ())
createDBNat cfg = do
  pool <- runLoggerT $ createSqlitePool (cs sqliteDb) (cfg ^. cfgPoolSize)
  pure ( DBRunner (dbConnectionCont pool)
       , \dbc dbctx -> NT (wrapErrors . dbRun dbc . (`runReaderT` dbctx) . runExceptT . unDB)
       , destroyAllResources pool
       )
  where
    -- runLoggerT = runStderrLoggingT  -- for lots of debug output
    runLoggerT = runNoLoggingT

    sqliteDb = case cfg ^. cfgDBKind of
      DBOnDisk fp -> fp
      DBInMemory -> ":memory:"

    wrapErrors :: IO (Either DBError a) -> ExceptT DBError IO a
    wrapErrors =
      lift . try >=> either (throwError . DBException . show @SomeException)
                            (either throwError pure)

    dbConnectionCont :: (MonadBaseControl IO m) => Pool SqlBackend -> (DBConnection -> m a) -> m a
    dbConnectionCont pool m = withResource pool (m . mkDBConnection)

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
  editDiscussions    = Entity.editDiscussions
  getEditChildren    = Entity.getEditChildren
  updateVotes        = Entity.updateVotes
  getVoteCount       = Entity.getVoteCount
  updateEdit         = Entity.updateEdit
  updateEditSource   = Entity.updateEditSource

  -- * Note
  createNote         = Entity.createNote
  getNote            = Entity.getNote
  updateNoteVotes    = Entity.updateNoteVotes

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
  assignGroupRole   = Entity.assignGroupRole
  getGroupRolesIn   = Entity.getGroupRolesIn
  getGroupRoles     = Entity.getGroupRoles
  unassignGroupRole = Entity.unassignGroupRole

  assignGlobalRole   = Entity.assignGlobalRole
  getGlobalRoles     = Entity.getGlobalRoles
  unassignGlobalRole = Entity.unassignGlobalRole

  -- * MetaInfo
  createMetaID_ = Entity.createMetaID_
  getMetaID     = Entity.getMeta
