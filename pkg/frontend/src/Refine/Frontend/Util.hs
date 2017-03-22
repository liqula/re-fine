{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Refine.Frontend.Util
where

import           Data.List (nub)
import qualified Data.JSString as JSS
import           Data.String.Conversions
import qualified Data.Text as ST
import           React.Flux

-- | See also:
-- https://bitbucket.org/wuzzeb/react-flux/pull-requests/11/clarify-diversify-classnames-helper/diff
classNamesAny :: [(ST, Bool)] -> PropertyOrHandler handler
classNamesAny xs = "className" @= ST.unwords names
  where
    names = nub $ fst <$> filter snd xs

toClasses :: s ~ JSS.JSString => [s] -> s
toClasses = JSS.unwords . filter (not . JSS.null)
