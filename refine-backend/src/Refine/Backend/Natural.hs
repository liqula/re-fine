module Refine.Backend.Natural where

import qualified Control.Natural     as CN
import qualified Servant.Utils.Enter as SN



cnToSn :: (a CN.:~> b) -> (a SN.:~> b)
cnToSn (CN.Nat n) = SN.Nat n
