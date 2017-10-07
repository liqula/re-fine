{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Test.AppRunner (provideAppRunner, throwApiErrors, monadicApp) where
#include "import_backend.hs"

import Test.Hspec
import Test.QuickCheck
import Control.Exception.Base (evaluate)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Server
import Refine.Backend.Test.Util
import Refine.Common.Types


-- | App actions run in god mode.  See 'unsafeAsGod'.
provideAppRunner :: ActionWith (AppM DB a -> ExceptT ApiError IO a) -> IO ()
provideAppRunner action = withTempCurrentDirectory $ do
  (runner, destroy) <- createAppRunner
  action runner
  destroy

createAppRunner :: forall a. IO (AppM DB a -> ExceptT ApiError IO a, IO ())
createAppRunner = do
  let cfg = Config
        { _cfgLogger        = LogCfg LogCfgDevNull LogWarning
        , _cfgDBKind        = Sqlite3OnDisk "./test.db"
        , _cfgPoolSize      = 5
        , _cfgFileServeRoot = "."
        , _cfgWarpSettings  = def
        , _cfgSessionLength = TimespanSecs 30
        , _cfgPoFilesRoot   = "."
        , _cfgSmtp          = Nothing
        , _cfgClient        = def
        , _cfgWSPingPeriod  = TimespanSecs 1
        , _cfgAllAreGods    = True
        , _cfgAppMLimit     = TimespanSecs 1
        }

  (tbe, destroytbe) <- mkBackend False cfg
  (clientChan, destroyClientChan) <- clientChanSink (tbe ^. backendLogChan)

  let runner :: forall b. AppM DB b -> ExceptT ApiError IO b
      runner = runApp (tbe ^. backendRunApp) clientChan

  void . throwApiErrors . runner $ do
    migrateDB cfg
    unsafeAsGod $ initializeDB [CliCreateGroup $ CreateGroup "Universe" "The group that contains everything" [] [] mempty Nothing]

  pure (runner, destroytbe >> destroyClientChan)


throwApiErrors :: forall a. ExceptT ApiError IO a -> IO a
throwApiErrors = (>>= either (throwIO . ErrorCall . show) pure) . runExceptT

monadicApp :: (AppM DB Property -> IO Property) -> AppM DB Property -> Property
monadicApp p = ioProperty . p
