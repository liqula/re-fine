{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.Logger (mkLogChan, LogChan) where
#include "import_backend.hs"

import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)

import Refine.Backend.Config


type LogChan = Chan (LogLevel, String)

-- | Returns a log channel and a destroy function.
--
-- FIXME: use fastlogger instead of appendFile.  it has more output channels and is *fast*!  this
-- would be a refactoring internal to 'mkLogger'.  this may also give us a more trustworthy destroy
-- function.
mkLogChan :: Config -> IO (LogChan, IO ())
mkLogChan cfg = do
  chan <- newChan
  let limit :: LogLevel
      limit = cfg ^. cfgLogger . logCfgLevel

      logf :: LogLevel -> String -> IO ()
      logf level = if level <= limit
        then case cfg ^. cfgLogger . logCfgTarget of
          LogCfgFile file -> liftIO . appendFile file . (<> "\n")
          LogCfgStdOut    -> \msg -> liftIO $ putStrLn msg >> hFlush stdout
          LogCfgDevNull   -> const $ pure ()
        else
          \_ -> pure ()

  tid <- forkIO . forever $ uncurry logf =<< readChan chan
  pure (chan, killThread tid)
