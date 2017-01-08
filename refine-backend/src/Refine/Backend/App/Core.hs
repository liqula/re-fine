module Refine.Backend.App.Core where

import Control.Lens (makeLenses, view)
import Control.Monad.Except

import Control.Natural
import Control.Monad.Reader

import Refine.Backend.Database
import Refine.Backend.Repository



type RunDB db   = db   :~> ExceptT DBError   IO
type RunDocRepo = Repo :~> ExceptT RepoError IO

data AppContext db = AppContext
  { _appRunDB      :: RunDB db
  , _appRunDocRepo :: RunDocRepo
  }

makeLenses ''AppContext

-- | Application monad handles
-- * database connection
-- TODO:
-- * user authorization
-- * user authentication
-- * event logging
newtype App db a = App { unApp :: ReaderT (AppContext db) (ExceptT AppError IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AppContext db)
    , MonadError AppError
    )

data AppError
  = AppError String
  | AppDBError DBError
  | AppDocRepoError RepoError
  deriving (Show)

appIO :: IO a -> App db a
appIO = App . liftIO

db :: db a -> App db a
db m = App $ do
  (Nat runDB) <- view appRunDB
  r <- liftIO (runExceptT (runDB m))
  either (throwError . AppDBError) pure r

docRepo :: Repo a -> App db a
docRepo m = App $ do
  (Nat runDocRepo) <- view appRunDocRepo
  r <- liftIO (runExceptT (runDocRepo m))
  either (throwError . AppDocRepoError) pure r

-- FIXME: More delicate logging
appLog :: String -> App db ()
appLog = appIO . putStrLn
