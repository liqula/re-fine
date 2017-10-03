{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.Logger (mkLogChan, LogChan) where
#include "import_backend.hs"

import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Log.FastLogger as FL

import Refine.Backend.Config


type LogChan = Chan (LogLevel, String)

-- | Returns a log channel and a destroy function.
mkLogChan :: Config -> IO (LogChan, IO ())
mkLogChan cfg = do
  chan <- newChan

  (logger, freelogger) <- do
    timecache <- FL.newTimeCache FL.simpleTimeFormat
    FL.newTimedFastLogger timecache $
      case cfg ^. cfgLogger . logCfgTarget of
        LogCfgFile file  -> FL.LogFile (FL.FileLogSpec file (9000 * 1000) 99) FL.defaultBufSize
        LogCfgStdOut     -> FL.LogStdout FL.defaultBufSize
        LogCfgDevNull    -> FL.LogNone

  let limit :: LogLevel
      limit = cfg ^. cfgLogger . logCfgLevel

      logf :: LogLevel -> String -> IO ()
      logf level msg = if level <= limit
        then logger (\t -> FL.toLogStr t <> FL.toLogStr (" " :: ST) <> FL.toLogStr msg)
        else pure ()

  tid <- forkIO . forever $ uncurry logf =<< readChan chan
  pure (chan, killThread tid >> freelogger)
