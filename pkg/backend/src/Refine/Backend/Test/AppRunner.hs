{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Test.AppRunner where
#include "import_backend.hs"

import Test.Hspec
import Test.QuickCheck
import Control.Exception.Base (evaluate)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Test.Util
import Refine.Common.Rest
import Refine.Common.Types


-- | App actions run in god mode.  See 'unsafeAsGod'.
provideAppRunner :: ActionWith (AppM DB a -> ExceptT ApiError IO a) -> IO ()
provideAppRunner action = withTempCurrentDirectory $ do
  (runner, destroy) <- createAppRunner
  action runner
  destroy

-- | App actions run in god mode.  See 'unsafeAsGod'.
createAppRunner :: forall a. IO (AppM DB a -> ExceptT ApiError IO a, IO ())
createAppRunner = do
  let dbFilePath = "./test.db"
      cfg = Config
        { _cfgLogger        = LogCfg LogCfgDevNull LogWarning
        , _cfgDBKind        = DBOnDisk dbFilePath
        , _cfgPoolSize      = 5
        , _cfgFileServeRoot = Nothing
        , _cfgWarpSettings  = def
        , _cfgCsrfSecret    = "CSRF-SECRET"
        , _cfgSessionLength = TimespanSecs 30
        , _cfgPoFilesRoot   = "."
        , _cfgSmtp          = Nothing
        , _cfgClient        = def
        , _cfgWSPingPeriod  = TimespanSecs 1
        , _cfgAllAreGods    = True
        }

  (dbRunner, dbNat, destroy) <- createDBNat cfg
  let guardWithGodhood = if cfg ^. cfgAllAreGods then unsafeAsGod else id
      logger = Logger . const $ pure ()
      runner :: forall a. AppM DB a -> ExceptT ApiError IO a
      runner m = ((runApp dbNat dbRunner logger cfg) $$ guardWithGodhood m)
        >>= liftIO . evaluate -- without evaluate we have issue #389

  void . throwApiErrors . runner $ do
    migrateDB cfg
    unsafeAsGod $ initializeDB [CliCreateGroup $ CreateGroup "Universe" "The group that contains everything" [] [] mempty]
  pure (runner, destroy)

throwApiErrors :: forall a. ExceptT ApiError IO a -> IO a
throwApiErrors = (>>= either (throwIO . ErrorCall . show) pure) . runExceptT

monadicApp :: (AppM DB Property -> IO Property) -> AppM DB Property -> Property
monadicApp p = ioProperty . p
