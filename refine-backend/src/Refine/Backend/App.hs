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


runApp :: RunDB db -> RunDocRepo -> App db :~> ExceptT AppError IO
runApp runDB runDocRepo = Nat $ (`runReaderT` (AppContext runDB runDocRepo)) . unApp
