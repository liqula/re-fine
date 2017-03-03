{-# LANGUAGE OverloadedStrings #-}

module Refine.Common.Translations where

import Data.String.Conversions (ST)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


-- | Translation Key
type TKey = ST

-- * Contribution

add_a_comment :: TKey
add_a_comment = "add_a_comment"

