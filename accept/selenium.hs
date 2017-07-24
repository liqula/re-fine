{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | FUTUREWORK:
-- - error handling of the services is not very good.
-- - remove pid files when services are down.
-- - check pid file validity in status (is the process really running?).
-- - make sure log files mention shutdown, somehow.  (currently, at least selenium looks like the process is still running.)
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens hiding ((.=))
import           Control.Monad
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromMaybe)
import           Data.String.Conversions
import           Data.Yaml ((.=), object, toJSON)
import qualified Data.Yaml as Yaml
import           Filesystem.Path.CurrentOS hiding (empty, null)
import           GHC.Stack (HasCallStack)
import qualified Network.Wreq as Wreq
import           Prelude hiding (FilePath)
import           System.Directory (getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing, doesFileExist, canonicalizePath)
import           System.Environment (lookupEnv, getArgs, getProgName)
import           System.Exit (ExitCode(ExitSuccess))
import qualified System.FilePath
import           System.IO.Unsafe (unsafePerformIO)
import           System.Posix.Daemon (runDetached, isRunning, killAndWait, Redirection(ToFile))
import           System.Posix.Signals (installHandler, sigQUIT, Handler(Catch))
import qualified System.Process
import           Text.Read (readMaybe)
import           Turtle hiding (f, o, x, s, d, e)


data Config = Config
  { cfgVerbose          :: !Bool
  , cfgSeleniumVerbose  :: !Bool
  , cfgDisplay          :: !String  -- ^ if this is running, use it; if not, start xvfb on it.
  , cfgDisplayDims      :: !String
  , cfgSeleniumHubPort  :: !Int
  , cfgSeleniumNodePort :: !Int
  , cfgAppPort          :: !Int
  , cfgBackendDbPath    :: String
  }
  deriving (Eq)

instance Yaml.ToJSON Config where
  toJSON cfg = object
    [ "cfgVerbose" .= toJSON (cfgVerbose cfg)
    , "cfgSeleniumVerbose" .= toJSON (cfgSeleniumVerbose cfg)
    , "cfgDisplay" .= toJSON (cfgDisplay cfg)
    , "cfgDisplayDims" .= toJSON (cfgDisplayDims cfg)
    , "cfgSeleniumHubPort" .= toJSON (cfgSeleniumHubPort cfg)
    , "cfgSeleniumNodePort" .= toJSON (cfgSeleniumNodePort cfg)
    , "cfgAppPort" .= toJSON (cfgAppPort cfg)
    , "cfgBackendDbPath" .= toJSON (cfgBackendDbPath cfg)
    ]

config :: Config
config = Config
  { cfgVerbose          = True
  , cfgSeleniumVerbose  = False
  , cfgDisplay          = fromMaybe ":0" $ getPortConfigRaw "DISPLAY"
  , cfgDisplayDims      = "1280x1024x8"  -- if this is too narrow horizontally, selenium *may* fail
                                         -- to find the stuff to the right of the visible screen!
  , cfgSeleniumHubPort  = getPortConfig "SELENIUM_HUB_PORT"
  , cfgSeleniumNodePort = getPortConfig "SELENIUM_NODE_PORT"
  , cfgAppPort          = getPortConfig "REFINE_APP_PORT"
  , cfgBackendDbPath    = "backend.db"
  }

getPortConfig :: HasCallStack => String -> Int
getPortConfig var = fromMaybe (error var) . (>>= readMaybe) $ getPortConfigRaw var

getPortConfigRaw :: HasCallStack => String -> Maybe String
getPortConfigRaw = unsafePerformIO . lookupEnv

main :: IO ()
main = sh $ do
  _ <- initCurrentDirectory
  liftIO . SBS.putStr . (<> "\n") $ Yaml.encode config
  args <- liftIO getArgs
  pnam <- liftIO getProgName
  case args of
    ["install"]         -> mainInstall
    ["start"]           -> mainStart  [minBound..]
    ["start", service]  -> mainStart  [readService service]
    ["stop"]            -> mainStop   $ reverse [minBound..]
    ["stop", service]   -> mainStop   [readService service]
    ["clean"]           -> mainClean  [minBound..]
    ["status"]          -> mainStatus [minBound..]
    ["status", service] -> mainStatus [readService service]
    bad                 -> error $ "bad arguments: " <> show (pnam, bad)


-- * install

mainInstall :: MonadIO m => m ()
mainInstall = liftIO $ do
  echo "installing..."
  downloadFileIfMissingOrBroken
    "https://selenium-release.storage.googleapis.com/3.4/selenium-server-standalone-3.4.0.jar"
    "c38679230f1836e77e6b4791539768864f575d96f219495ead94bab98b3b02737faad7620dd48bb1c289fb5d1f43d43fae0f8b3a8fba7e2dc867ad22c09cc02f"
  downloadFileIfMissingOrBroken
    "https://github.com/mozilla/geckodriver/releases/download/v0.17.0/geckodriver-v0.17.0-linux64.tar.gz"
    "7c7edc0294d246dec246cbb37b6f93ecfe5c0e9fede61a125602e59c997883c79f2d7509360c8c0001a9877781019474a397f944c240f64bbc621c6e951ecd9b"
  ExitSuccess <- shell "tar xpzf ./geckodriver-v0.17.0-linux64.tar.gz" mempty
  echo "ok."

mkAndSetCurrentDirectory :: System.FilePath.FilePath -> IO ()
mkAndSetCurrentDirectory d = do createDirectoryIfMissing True d; setCurrentDirectory d

downloadFileIfMissingOrBroken :: String -> String -> MonadIO m => m ()
downloadFileIfMissingOrBroken url@(System.FilePath.splitFileName -> (_, file)) sha512 = liftIO $ do
  doesit <- doesFileExist file
  unless doesit $ do
    resp <- Wreq.get url
    LBS.writeFile file $ resp ^. Wreq.responseBody
  sha512' <- take 128 <$> System.Process.readProcess "/usr/bin/sha512sum" [file] mempty
  unless (sha512 == sha512') $ do
    putStrLn $ "*** check sums differ: " <> show (file, sha512, sha512')


-- * data types

data Service = Selenium Role | Backend | Xvfb
  deriving (Eq, Ord)

instance Bounded Service where
  minBound = Selenium minBound
  maxBound = Xvfb

instance Enum Service where
  toEnum 0 = Selenium Hub
  toEnum 1 = Selenium Node
  toEnum 2 = Backend
  toEnum 3 = Xvfb
  toEnum i = error $ "Enum Service: " <> show i

  fromEnum (Selenium Hub)  = 0
  fromEnum (Selenium Node) = 1
  fromEnum Backend         = 2
  fromEnum Xvfb            = 3

  enumFrom s = [s..maxBound]  -- (why is this not the default?)

instance Show Service where
  show (Selenium Hub)  = "selenium-hub"
  show (Selenium Node) = "selenium-node"
  show Backend         = "backend"
  show Xvfb            = "xvfb"

readService :: String -> Service
readService s = fromMaybe (error $ "readService: " <> show s)
              . lookup s
              $ (\service -> (show service, service))
            <$> [minBound..]

data Role = Hub | Node
  deriving (Eq, Ord, Show, Bounded, Enum)


data Suffix = Pid | Log | LogLck | OutErr
  deriving (Eq, Ord, Bounded, Enum)

instance Show Suffix where
  show Pid    = "pid"
  show Log    = "log"
  show LogLck = "log.lck"
  show OutErr = "outerr"

serviceFile :: Service -> Suffix -> String
serviceFile service suf = show service <> "." <> show suf


-- * liveness tests

testTcpPort :: String -> Int -> MonadIO m => m ()
testTcpPort msg port = testLiveness 12 probe >>= \case
  True  -> pure ()
  False -> liftIO . throwIO . ErrorCall $ msg <> ": testTcpPort timed out"
  where
    probe = do
      code <- shell' ["nc", "-z", "localhost", show port] mempty
      pure $ code == ExitSuccess

testDisplay :: Bool -> String -> MonadIO m => m Bool
testDisplay fast display = testLiveness (if fast then 1 else 12) probe
  where
    probe = do
      code <- shell' [ "xdpyinfo", "-display", display
                     , ">/dev/null", "2>&1"
                     ]
                     mempty
      pure $ code == ExitSuccess

testLiveness :: forall m. MonadIO m => Int -> m Bool -> m Bool
testLiveness rounds probe = step rounds 901000{- microsecs delay between rounds -}
  where
    step :: Int -> Int -> m Bool
    step i _ | i <= 0 = pure False
    step tries delay = probe >>= \case
      True  -> pure True
      False -> liftIO (threadDelay delay) >> step (tries - 1) delay


-- * start

-- | Run detached selenium process
runSelenium :: Role -> MonadIO m => m ()
runSelenium role = do
  detachedShell "." (Just $ serviceFile (Selenium role) Pid) (ToFile $ serviceFile (Selenium role) OutErr)
    "/usr/bin/java" (rts <> cmd)
  testTcpPort (show $ Selenium role) $ case role of
                                         Hub  -> cfgSeleniumHubPort config
                                         Node -> cfgSeleniumNodePort config
  where
    selRole :: Role -> a -> a -> a
    selRole Hub  h _n = h
    selRole Node _h n = n

    rts, cmd :: [String]
    rts = [ "-Dwebdriver.gecko.driver=./geckodriver" | role == Node ]
    cmd = [ "-jar", "selenium-server-standalone-3.4.0.jar"
          , "-role", selRole role "hub" "node"
          , "-host", "localhost"
          , "-port", show $ selRole role (cfgSeleniumHubPort config) (cfgSeleniumNodePort config)
          , "-debug", if cfgSeleniumVerbose config then "true" else "false"
          , "-log", serviceFile (Selenium role) Log
          ]

-- | Assume backend is already built (with webdriver minimization, or firefox will choke on it; see
-- accept rule in ../Build.hs).
runBackend :: IO ()
runBackend = do
  backendConfigPath <- canonicalizePath "server.conf"
  SBS.writeFile backendConfigPath . Yaml.encode $ mkBackendConfig (cfgAppPort config)
  detachedShell "../../pkg/backend" (Just $ serviceFile Backend Pid) (ToFile $ serviceFile Backend OutErr)
    "/usr/bin/stack" ["exec", "--", "refine", backendConfigPath]
  testTcpPort (show Backend) (cfgAppPort config)

mkBackendConfig :: Int -> Yaml.Value
mkBackendConfig port = Yaml.object
  [ "_cfgSessionLength" .= Yaml.object ["TimespanHours" .= (72 :: Int)]
  , "_cfgShouldLog"     .= True
  , "_cfgWarpSettings"  .= Yaml.object ["_warpSettingsPort" .= port, "_warpSettingsHost" .= ("HostIPv4" :: String)]
  , "_cfgFileServeRoot" .= ("../frontend/js-build" :: String)
  , "_cfgPoFilesRoot"   .= ("../../po" :: String)
  , "_cfgCsrfSecret"    .= ("CSRF-SECRET" :: String)
  , "_cfgDBKind"        .= Yaml.object [ "tag" .= ("DBOnDisk" :: String)
                                       , "contents" .= ("../../accept/.selenium/" <> cfgBackendDbPath config)
                                         -- i tried DBInMemory, but got vague and fatal database errors in the backend.
                                       ]
  , "_cfgPoolSize"      .= (8 :: Int)
  , "_cfgShouldMigrate" .= True
  , "_cfgDevMode"       .= False
  ]

runXvfb :: MonadIO m => m ()
runXvfb = do
  let disp = cfgDisplay config
  alreadyRunning <- testDisplay True disp
  if alreadyRunning
    then do
      echo $ "using X server running on " <> cs disp
    else do
      detachedShell "." (Just $ serviceFile Xvfb Pid) (ToFile $ serviceFile Xvfb OutErr)
        "/usr/bin/Xvfb" [disp, "-screen", "0", cfgDisplayDims config]
      runningNow <- testDisplay False disp
      unless runningNow $ do
        liftIO . throwIO. ErrorCall $ "runXvfb: could not start on display " <> show disp

mainStart :: [Service] -> MonadIO m => m ()
mainStart services = forM_ services $ \service -> do
  echo $ "starting " <> cs (show service) <> "..."
  case service of
    Selenium role -> runSelenium role
    Backend       -> liftIO runBackend
    Xvfb          -> runXvfb
  echo "ok."


-- * stop

mainStop :: [Service] -> MonadIO m => m ()
mainStop services = do
  when (any (`elem` (Selenium <$> [minBound..])) services) $ do
    echo "the selenium jobs do not always respond to QUIT, you sometimes have to be more stern.  (see selenium.hs for details.)"
    _ <- shell' ["killall", "-9", "java", ">/dev/null", "2>&1"] mempty
    _ <- shell' ["killall", "-9", "geckodriver", ">/dev/null", "2>&1"] mempty
    pure ()

  liftIO . forM_ services $ \service -> do
    let pidfile = serviceFile service Pid
    running <- isRunning pidfile
    if running
      then do
        echo $ "killing " <> cs (show service) <> "..."
        killAndWait pidfile
        echo "ok."
      else do
        echo $ "not running: " <> cs (show service) <> " (doing nothing)"


-- * clean

mainClean :: [Service] -> MonadIO m => m ()
mainClean services = liftIO . forM_ services $ \service -> do
  let rm' file = do
        exists <- testfile (cs file)
        when exists $ do
          echo $ "removing " <> cs (show file) <> "."
          rm (cs file)

  rm' (cfgBackendDbPath config)
  rm' `mapM_` (serviceFile service <$> [minBound..])


-- * status

-- | Show pid file contents and end of logfile (or, if not available, end of stdout/stderr).
mainStatus :: [Service] -> MonadIO m => m ()
mainStatus services = liftIO . forM_ services $ \service -> do
  echoCS $ "\n\n>>>>>>>>>>>>>>>>>>>> " <> show service <> "\n"
  let pidfile = serviceFile service Pid
      logfile = serviceFile service Log
      outfile = serviceFile service OutErr

  existsPid <- testfile (cs pidfile)
  if existsPid
    then readFile pidfile >>= putStrLn . ("pid: " <>)
    else echo "no pid file."

  existsLog <- testfile (cs logfile)
  if existsLog
    then void . System.Process.system $ "cat " <> logfile
    else do
      echo "no log file."
      existsOut <- testfile (cs outfile)
      if existsOut
        then void . System.Process.system $ "cat " <> outfile
        else echo "no out/err file."


-- * amendments to turtle

initCurrentDirectory :: MonadIO m => m Turtle.FilePath
initCurrentDirectory = liftIO $ do
  mkAndSetCurrentDirectory "./.selenium"
  dir <- canonicalizePath =<< getCurrentDirectory
  progName <- getProgName
  putStrLn $ progName <> ": working directory is " <> dir
  pure $ cs dir

shell' :: [String] -> Shell Line -> MonadIO m => m ExitCode
shell' args = liftIO . shell (cs $ unwords args)

-- | It took way to long to write this function.  I tried to build it on top of 'Turtle.shell', but
-- failed, without understanding why.  It is important to know that SIGQUIT (a) can not be caught
-- with an exception handler, and (b) will not kill any forked processes (whether with
-- "System.Process" or with "Turtle").
--
-- FIXME: make sure there are no fd leaks.
-- http://hackage.haskell.org/package/daemons-0.2.1/docs/System-Posix-Daemon.html#v:runDetached
detachedShell :: String -> Maybe String -> Redirection -> String -> [String] -> MonadIO m => m ()
detachedShell workingdir pid redir cmd args = liftIO . runDetached pid redir $ do
  ph <- System.Process.runProcess cmd args (Just workingdir) Nothing Nothing Nothing Nothing
  _ <- installHandler sigQUIT (Catch $ System.Process.terminateProcess ph) Nothing
  _ <- System.Process.waitForProcess ph
  pure ()

instance ConvertibleStrings ST Line where
  convertString = unsafeTextToLine

instance ConvertibleStrings Line ST where
  convertString = lineToText

instance ConvertibleStrings String Line where
  convertString = unsafeTextToLine . cs

instance ConvertibleStrings Line String where
  convertString = cs . lineToText

instance ConvertibleStrings ST Filesystem.Path.CurrentOS.FilePath where
  convertString = Filesystem.Path.CurrentOS.fromText

instance ConvertibleStrings Filesystem.Path.CurrentOS.FilePath ST where
  convertString = either (error . show) id . Filesystem.Path.CurrentOS.toText

instance ConvertibleStrings String Filesystem.Path.CurrentOS.FilePath where
  convertString = cs @ST . cs

instance ConvertibleStrings Filesystem.Path.CurrentOS.FilePath String where
  convertString = cs @ST . cs


echoShow :: (MonadIO m, Show a) => a -> m ()
echoShow = echo . cs . show

echoCS :: (MonadIO m, ConvertibleStrings s Line) => s -> m ()
echoCS = echo . cs

debugLog :: MonadIO m => ConvertibleStrings s Line => s -> m ()
debugLog = when (cfgVerbose config) . liftIO . sh . echo . cs
