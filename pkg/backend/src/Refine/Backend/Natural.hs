{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Natural (cnToSn, natThrowError, (.), ($$)) where
#include "import_backend.hs"

import qualified Control.Natural     as CN
import qualified Servant.Utils.Enter as SN


cnToSn :: (a CN.:~> b) -> (a SN.:~> b)
cnToSn (CN.NT n) = SN.Nat n

natThrowError :: (Functor m, Show e) => ExceptT e m CN.:~> m
natThrowError = CN.NT (fmap (either (error . show) id) . runExceptT)
