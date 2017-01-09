module Refine.Backend.App
  ( module Refine.Backend.App.Core
  , module Refine.Backend.App.VDoc
  , runApp
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Natural

import Refine.Backend.App.Core
import Refine.Backend.App.VDoc
import Refine.Backend.Logger


runApp :: RunDB db -> RunDocRepo -> Logger -> App db :~> ExceptT AppError IO
runApp runDB runDocRepo logger =
  Nat $ (`runReaderT` AppContext runDB runDocRepo logger) . unApp
