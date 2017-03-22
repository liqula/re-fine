{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}

module Refine.Frontend.Util
where

import           Data.List (nub)
import           Data.String.Conversions
import qualified Data.Text as ST
import           React.Flux

-- | See also:
-- https://bitbucket.org/wuzzeb/react-flux/pull-requests/11/clarify-diversify-classnames-helper/diff
classNamesAny :: [(ST, Bool)] -> PropertyOrHandler handler
classNamesAny xs = "className" @= ST.unwords names
  where
    names = nub $ fst <$> filter snd xs

toClasses :: [String] -> String
toClasses = cs . unwords . filter (not . null) . fmap cs
