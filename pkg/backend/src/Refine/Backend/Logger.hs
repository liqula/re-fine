{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Backend.Logger where

import Refine.Backend.Prelude

newtype Logger = Logger { unLogger :: String -> IO () }
