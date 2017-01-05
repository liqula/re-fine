module Refine.Backend.App where

import Control.Monad.Except

import Control.Natural
import Control.Monad.Reader

import Refine.Backend.Database



type RunDB db = db :~> ExceptT DBError IO

-- | Application monad handles
-- * database connection
-- TODO:
-- * user authorization
-- * user authentication
-- * event logging
newtype App db a = App { unApp :: ReaderT (RunDB db) (ExceptT AppError IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (RunDB db)
    , MonadError AppError
    )

data AppError = AppError String

runApp :: RunDB db -> App db :~> ExceptT AppError IO
runApp runDB = Nat $ (`runReaderT` runDB) . unApp

appIO :: IO a -> App db a
appIO = App . liftIO
