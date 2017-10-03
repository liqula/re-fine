{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database
  ( module DatabaseCore
  , MkDBNat
  , DBRunner(..)
  , DBConnection(..)
  , createDBNat
  ) where
#include "import_backend.hs"

import Control.Monad.Logger
import Data.Pool (Pool, withResource, destroyAllResources)
import Database.Persist
import Database.Persist.Sql
import qualified Database.Persist.Sqlite as Sqlite
#ifdef __WITH_POSTGRES__
import qualified Database.Persist.Postgresql as Postgresql
#endif

import Refine.Backend.Config
import Refine.Backend.Database.Class as DatabaseCore
import Refine.Backend.Database.Core  as DatabaseCore
import Refine.Backend.Database.Schema()
import Refine.Backend.Database.Entity as Entity
import Refine.Backend.Database.Types as DatabaseCore


-- | For transaction management.
data DBConnection = DBConnection
  { dbInit     :: forall m   . (MonadBaseControl IO m) => m ()
  , dbRun      :: forall m a . (MonadBaseControl IO m) => ReaderT SqlBackend m a -> m a
  , dbCommit   :: forall m   . (MonadBaseControl IO m) => m ()
  , dbRollback :: forall m   . (MonadBaseControl IO m) => m ()
  }

type MkDBNat db = DBConnection -> DBContext -> (db :~> ExceptT DBError IO)

newtype DBRunner = DBRunner { unDBRunner :: forall m a . MonadBaseControl IO m => (DBConnection -> m a) -> m a }

createPool :: forall m. (MonadLogger m, MonadBaseControl IO m, MonadIO m) => Config -> m (Pool SqlBackend)
createPool cfg = case cfg ^. cfgDBKind of
  Postgres connstr -> postgres connstr
  Sqlite3OnDisk fp -> sqlite3 (Just fp)
  Sqlite3InMemory  -> sqlite3 Nothing
  where
    postgres :: ST -> m (Pool SqlBackend)
#ifdef __WITH_POSTGRES__
    postgres connstr = Postgresql.createPostgresqlPool (cs connstr) (cfg ^. cfgPoolSize)
#else
    postgres = error "postgres: you need to compile with __WITH_POSTGRES__ if you want to use this."
#endif

    sqlite3 :: Maybe FilePath -> m (Pool SqlBackend)
    sqlite3 Nothing = Sqlite.createSqlitePoolFromInfo (mkSqlInfo ":memory:") (cfg ^. cfgPoolSize)
    sqlite3 (Just fp) = Sqlite.createSqlitePoolFromInfo (mkSqlInfo fp) (cfg ^. cfgPoolSize)

    mkSqlInfo :: String -> Sqlite.SqliteConnectionInfo
    mkSqlInfo = (Sqlite.walEnabled .~ False)  -- #291, #444
              . Sqlite.mkSqliteConnectionInfo
              . cs

createDBNat :: Config -> IO (DBRunner, MkDBNat DB, IO ())
createDBNat cfg = do
  pool <- runLoggerT $ createPool cfg
  pure ( DBRunner (dbConnectionCont pool)
       , \dbc dbctx -> NT (wrapErrors . dbRun dbc . (`runReaderT` dbctx) . runExceptT . unDB)
       , destroyAllResources pool
       )
  where
    -- runLoggerT = runStderrLoggingT  -- for lots of debug output, but no better error messages
    runLoggerT = runNoLoggingT

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
      DBConnection
        { dbInit   = control $ \runInIO -> mask $ \restore -> do
                       restore $ connBegin conn (getStmtConn conn)
                       runInIO $ pure ()
        , dbRun    = \r -> control $ \runInIO -> mask $ \restore -> do
                             restore (runInIO $ runReaderT r conn)
        , dbCommit = control $ \runInIO -> mask $ \restore -> do
                       restore $ connCommit conn (getStmtConn conn)
                       runInIO $ pure ()
        , dbRollback = control $ \runInIO -> mask $ \restore -> do
                         restore $ restore (connRollback conn (getStmtConn conn))
                         runInIO $ pure ()
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
  editDiscussions    = Entity.editDiscussions
  getEditChildren    = Entity.getEditChildren
  updateVotes        = Entity.updateVotes
  getVoteCount       = Entity.getVoteCount
  updateEdit         = Entity.updateEdit
  updateEditSource   = Entity.updateEditSource

  -- * Discussion
  createDiscussion       = Entity.createDiscussion
  rebaseDiscussion       = Entity.rebaseDiscussion
  getDiscussion          = Entity.getDiscussion
  statementsOfDiscussion = Entity.statementsOfDiscussion
  discussionOfStatement  = Entity.discussionOfStatement
  updateDiscussionVotes  = Entity.updateDiscussionVotes

  -- * Statement
  createStatement      = Entity.createStatement
  updateStatement      = Entity.updateStatement
  getStatement         = Entity.getStatement

  -- * User
  runUsersCmd        = Entity.runUsersCmd
  insertDBUser       = Entity.insertDBUser
  replaceDBUser      = Entity.replaceDBUser
  getDBUser          = Entity.getDBUser

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
