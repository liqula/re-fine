{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.Logger where
#include "import_backend.hs"

import Refine.Backend.Config

newtype Logger = Logger { unLogger :: LogLevel -> String -> (forall m. MonadIO m => m ()) }

mkLogger :: Config -> Logger
mkLogger cfg = Logger $ \level -> do
  let limit = cfg ^. cfgLogger . logCfgLevel
  if level <= limit
    then case cfg ^. cfgLogger . logCfgTarget of
      LogCfgFile file -> liftIO . appendFile file . (<> "\n")
      LogCfgStdOut    -> \msg -> liftIO $ putStrLn msg >> hFlush stdout
      LogCfgDevNull   -> const $ pure ()
    else
      \_ -> pure ()
