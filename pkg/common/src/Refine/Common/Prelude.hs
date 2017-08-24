{-# LANGUAGE CPP #-}
#include "language.hs"
module Refine.Common.Prelude (module P, (.:=)) where

import Data.Aeson as P hiding ((.=))
import Data.Aeson.Types as P hiding ((.=))
import Data.Int as P
import Generics.SOP as P hiding (Generic, All, to)
import Generics.SOP.JSON as P
import Generics.SOP.NFData as P
import Servant.API as P
import Servant.Utils.Enter as P hiding ((:~>))  -- FUTUREWORK: could servant be convinced to use Control.Natural?
import Servant.Utils.Links as P (safeLink)

import Refine.Prelude as P

import qualified Data.Aeson as Aeson

-- | resolve conflict with "Control.Lens"
(.:=) :: ToJSON v => ST -> v -> Pair
(.:=) = (Aeson..=)
