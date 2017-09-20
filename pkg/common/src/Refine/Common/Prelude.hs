{-# LANGUAGE CPP #-}
#include "language_common.hs"
module Refine.Common.Prelude (module P, (.:=), camelToUnderscore, underscoreToCaml, underscoreToCaml') where

import Data.Aeson as P hiding ((.=))
import Data.Aeson.Types as P hiding ((.=))
import Data.Int as P
import Data.Char (isLower, isUpper, toLower, toUpper)
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


camelToUnderscore :: ST -> ST
camelToUnderscore = cs . go True . cs
  where
    go :: Bool -> String -> String
    go _ "" = ""
    go isfirst (x:xs)
      | isUpper x && isfirst = toLower x       : go False xs
      | isUpper x            = '_' : toLower x : go False xs
      | otherwise            = x               : go False xs

underscoreToCaml :: Bool -> ST -> ST
underscoreToCaml = underscoreToCaml' ['_']

underscoreToCaml' :: [Char] -> Bool -> ST -> ST
underscoreToCaml' underscores firstIsUpper = cs . go firstIsUpper . cs
  where
    go :: Bool -> String -> String
    go _ "" = ""
    go _ (x : xs) | x `elem` underscores = go True xs
    go nextIsUpper (x : xs) = (if nextIsUpper then toUpper x else x) : go False xs
