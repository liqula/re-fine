{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Refine.Backend.Test.AppServer where
#include "import_backend.hs"

import           Network.HTTP.Types (Method, Header, methodGet, methodPut, methodDelete)
import qualified Network.HTTP.Types as HTTP
import           Network.HTTP.Types.Status (Status(statusCode))
import           Network.URI (URI, uriToString)
import           Network.Wai (requestMethod, requestHeaders, defaultRequest)
import           Network.Wai.Test (SRequest(..), SResponse(..))
import qualified Network.Wai.Test as Wai
import qualified Network.Wai.Test.Internal as Wai
import           Test.Hspec
import           System.Timeout

import           Refine.Backend.App as App hiding (getEdit)
import           Refine.Backend.Config
import           Refine.Backend.Database (DB)
import           Refine.Backend.Server
import           Refine.Backend.Test.Util (withTempCurrentDirectory)
import           Refine.Common.Types as Common
import           Refine.Common.Test.Samples

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- | This type carries a 'Backend' (which contains of an 'AppM' and an 'Application'), plus login
-- state.
--
-- FUTUREWORK: "Refine.Backend.Test.AppRunner" and the code here could benefit from some refactoring
-- to reduce code duplication.
data TestBackend = TestBackend
  { _testBackend            :: Backend DB
  , _testBackendCurrentUser :: MVar (Maybe (Username, Password))
  }

makeLenses ''TestBackend

-- | Create session via 'mkProdBackend'.  Note that there is no network listener and 'WarpSettings'
-- are meaningless; the session only creates an 'AppM' runner and an 'Application'.
createTestSession :: (TestBackend -> IO ()) -> IO ()
createTestSession = createTestSession' False testBackendCfg

testBackendCfg :: Config
testBackendCfg = def
        & cfgSmtp       .~ Nothing
        & cfgAllAreGods .~ True

createTestSession' :: Bool -> Config -> (TestBackend -> IO ()) -> IO ()
createTestSession' plainWebSocks cfg action = withTempCurrentDirectory $ do
  (backend, destroyBackend) <- mkBackend plainWebSocks cfg
  let schaffolding = do
        gs <- App.getGroups
        when (null gs) $ do
          void . App.addGroup $ CreateGroup "default" "default group" [] [] mempty Nothing

  (clientChan, destroyClientChan) <- clientChanSink (backend ^. backendLogChan)
  runApp' (backend ^. backendRunApp) clientChan (unsafeAsGod schaffolding)

  () <- action =<< (TestBackend backend <$> newMVar Nothing)
  destroyBackend >> destroyClientChan

createTestSessionWith :: (TestBackend -> IO ()) -> (TestBackend -> IO ()) -> IO ()
createTestSessionWith initAction action = createTestSession $ \sess -> do
  () <- initAction sess
  () <- action sess
  pure ()

-- | Call 'runDB'' and crash on 'Left'.
runDB :: TestBackend -> AppM DB a -> IO a
runDB sess = errorOnLeft . runDB' sess

-- | Call an 'App' action.
runDB' :: TestBackend -> AppM DB a -> IO (Either ApiError a)
runDB' sess action = do
  testlogin :: AppM DB () <- do
    readMVar (sess ^. testBackendCurrentUser) >>= pure . \case
      Nothing -> pure ()
      Just (u, p) -> void $ login (Login u p)
  (clientChan, destroyClientChan) <- clientChanSink (sess ^. testBackend . backendLogChan)
  result <- runExceptT . runApp (sess ^. testBackend . backendRunApp) clientChan $ testlogin >> action
  destroyClientChan
  pure result

errorOnLeft :: Show e => IO (Either e a) -> IO a
errorOnLeft action = either (throwIO . ErrorCall . show') pure =<< action
  where
    show' x = "errorOnLeft: " <> show x


-- | Log into a 'TestBackend'.  See 'runDB'' for an explanation why this is difficult.
testBackendLogin :: TestBackend -> Username -> Password -> IO ()
testBackendLogin (view testBackendCurrentUser -> mvar) u p = modifyMVar mvar (\_ -> pure (Just (u, p), ()))

-- | Log out of a 'TestBackend'.  See 'runDB'' for an explanation why this is difficult.
testBackendLogout :: TestBackend -> IO ()
testBackendLogout (view testBackendCurrentUser -> mvar) = modifyMVar mvar (\_ -> pure (Nothing, ()))


-- * testing and logs

-- | Test if all strings are substrings of the log file contents.  The contents is flushed, and the
-- next call will not see it again.
shouldHaveLogged :: HasCallStack => TestBackend -> [String] -> Expectation
shouldHaveLogged = shouldHaveLoggedOrNot True

-- | See 'shouldHaveLogged'.
shouldNotHaveLogged :: HasCallStack => TestBackend -> [String] -> Expectation
shouldNotHaveLogged = shouldHaveLoggedOrNot False

-- | See 'shouldHaveLogged'.
shouldHaveLoggedOrNot :: Bool -> TestBackend -> [String] -> Expectation
shouldHaveLoggedOrNot should tbe substrs = do
  logs <- getTChanContents' (tbe ^. testBackend . backendLogChan)
  expect (mconcat (snd <$> logs)) `mapM_` substrs
  where
    expect hay needle = if should
      then hay `shouldContain` needle
      else hay `shouldNotContain` needle

    getTChanContents' chan = atomically (tryReadTChan chan) >>= \case
      Nothing -> pure []
      Just e  -> (e:) <$> getTChanContents' chan

-- | call 'dumpLog' in a loop every 100ms.  this makes hspec test cases hang, it takes a timeout
-- after which it stops gracefully.  useful when playing with tests interactively.
--
-- FIXME: this still blocks?!  even if nothing else happens in the test case?  'dumpLog' seems to
-- work fine.  very strange.
streamLog :: HasCallStack => Timespan -> TestBackend -> IO ()
streamLog timespan tbe = void . forkIO . void . timeout (timespanUs timespan) . forever $
  dumpLog tbe >> threadDelay (100 * 1000)

-- | useful when playing with tests interactively.
dumpLog :: HasCallStack => TestBackend -> IO ()
dumpLog = flushLog >=> putStr

-- | useful when playing with tests interactively.
flushLog :: HasCallStack => TestBackend -> IO String
flushLog tbe = unlines . fmap show <$> loop
  where
    loop = atomically (tryReadTChan chan) >>= \case
      Nothing -> pure mempty
      Just e  -> (e:) <$> loop
    chan = tbe ^. testBackend . backendLogChan


-- * user handling

addUserAndLogin :: TestBackend -> Username -> Email -> Password -> IO (ID User)
addUserAndLogin sess username useremail userpass = do
  user :: User <- runDB sess $ do
    _ <- unsafeAsGod $ createUser (CreateUser username useremail userpass Nothing "")
    login (Login username userpass)
  testBackendLogin sess username userpass
  pure (user ^. userID)

addTestUserAndLogin :: TestBackend -> IO ()
addTestUserAndLogin sess = void $ addUserAndLogin sess testUsername testUserEmail testPassword

mkTestUserAndEditAndLogin :: TestBackend -> IO (ID Edit)
mkTestUserAndEditAndLogin sess = addTestUserAndLogin sess >> mkEdit sess


mkCVDoc :: TestBackend -> CreateVDoc -> IO VDoc
mkCVDoc sess vdoc = runDB sess $ createVDoc vdoc

mkEdit :: TestBackend -> IO (ID Edit)
mkEdit = fmap (^. vdocHeadEdit) . (`mkCVDoc` sampleCreateVDoc0)
