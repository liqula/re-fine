{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Backend.Logger where

import Refine.Backend.Prelude
import Refine.Backend.Config

newtype Logger = Logger { unLogger :: String -> IO () }

defaultLogger :: Config -> Logger
defaultLogger cfg = Logger $ case cfg ^. cfgLogger . logCfgTarget of
  LogCfgFile file -> appendFile file . (<> "\n")
  LogCfgStdOut    -> \msg -> putStrLn msg >> hFlush stdout
  LogCfgDevNull   -> const $ pure ()
