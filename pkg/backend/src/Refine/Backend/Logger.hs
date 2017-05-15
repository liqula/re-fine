{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Backend.Logger where


newtype Logger = Logger { unLogger :: String -> IO () }
