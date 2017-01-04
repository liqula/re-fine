module Refine.Backend.App where

import Control.Natural
import Control.Monad.Reader


-- | Application monad handles
-- * database connection
-- TODO:
-- * user authorization
-- * user authentication
-- * event logging
newtype App db a = App { unApp :: ReaderT (db :~> IO) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (db :~> IO))

runApp :: (db :~> IO) -> (App db :~> IO)
runApp runDb = Nat ((`runReaderT` runDb) . unApp)

