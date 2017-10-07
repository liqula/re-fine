{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.Logger (LogChan, mkLogChan, asyncChanPipe, asyncChanPipe') where
#include "import_backend.hs"

import System.IO.Unsafe (unsafePerformIO)
import qualified System.Log.FastLogger as FL

import Refine.Backend.Config


type LogChan = TChan (LogLevel, String)

-- | Returns a log channel and a destroy function.  The log channel leads to disk, stdout, etc. via
-- fast-logger.
mkLogChan :: Config -> IO (LogChan, IO ())
mkLogChan cfg = do
  chan <- newTChanIO

  (logger, freelogger) <- do
    timecache <- FL.newTimeCache FL.simpleTimeFormat
    FL.newTimedFastLogger timecache $
      case cfg ^. cfgLogger . logCfgTarget of
        LogCfgFile file  -> FL.LogFile (FL.FileLogSpec file (9000 * 1000) 99) FL.defaultBufSize
        LogCfgStdOut     -> FL.LogStdout FL.defaultBufSize
        LogCfgDevNull    -> FL.LogNone

  let limit :: LogLevel
      limit = cfg ^. cfgLogger . logCfgLevel

      formatmsg ::  String -> FL.FormattedTime -> FL.LogStr
      formatmsg msg time = mconcat
        [ FL.toLogStr time
        , FL.toLogStr (" " :: ST)
        , FL.toLogStr msg
        , FL.toLogStr ("\n" :: ST)
        ]

      logf :: LogLevel -> String -> IO ()
      logf level msg = if level <= limit
        then logger (formatmsg msg)
        else pure ()

  as <- asyncChanPipe chan (uncurry logf)
  pure (chan, cancel as >> freelogger)


-- | Take a source 'TChan', a sink function, and plug the former into the latter.  If the resulting
-- 'Async' thread gets cancelled, the source gets flushed one more time.
asyncChanPipe :: TChan a -> (a -> IO ()) -> IO (Async ())
asyncChanPipe source sink = asyncChanPipe' source sink (pure ())

-- | Variant of 'asyncChanPipe' that takes an extra cleanup action.
asyncChanPipe' :: TChan a -> (a -> IO ()) -> IO () -> IO (Async ())
asyncChanPipe' source sink cleanup = do
  let pushone = sink =<< atomically (readTChan source)
      finallyflush = atomically (tryReadTChan source) >>= \case
        Nothing -> cleanup
        Just e  -> sink e >> finallyflush
  async $ forever pushone `finally` finallyflush
