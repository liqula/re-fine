module Refine.Backend.Natural where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (either)

import qualified Control.Natural     as CN
import qualified Servant.Utils.Enter as SN



cnToSn :: (a CN.:~> b) -> (a SN.:~> b)
cnToSn (CN.Nat n) = SN.Nat n

natThrowError :: (Functor m, Show e) => ExceptT e m CN.:~> m
natThrowError = CN.Nat (fmap (either (error . show) id) . runExceptT)
