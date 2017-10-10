{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.WebSocketSpec where
#include "import_backend.hs"

import Data.Set (findMin)
import Network.Wai.Handler.Warp (withApplication, testWithApplication)
import Network.WebSockets
import System.Random
import System.Timeout
import Test.Hspec

import Refine.Backend.App.Access
import Refine.Backend.Server.WebSocket (mkWSSessionId)
import Refine.Backend.App.MigrateDB (initializeDB)
import Refine.Backend.Config
import Refine.Backend.Server (backendServer)
import Refine.Backend.Test.AppServer
import Refine.Common.Test
import Refine.Common.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- * config

verbose_ :: Bool
verbose_ = False

-- | Do not start server in 'wsBackend', but reference one that is already running elsewhere,
-- outside the test suide.  Could be a staging or even production system that we want to stress
-- test, for instance.
useExternalBackend :: Bool
useExternalBackend = False

timeoutVal :: Timespan
timeoutVal = TimespanSecs 3


-- * primitives

sendMessage :: (HasCallStack, ToJSON msg) => Connection -> msg -> IO ()
sendMessage conn msg = sendTextData conn (cs $ encode msg :: ST)

receiveMessage :: (HasCallStack, FromJSON msg) => Connection -> IO msg
receiveMessage conn = receiveData conn >>= either (throwIO . ErrorCall . show) pure . eitherDecode

askQuestion :: (HasCallStack, Show tosrv, ToJSON tosrv, FromJSON toclnt) => Connection -> tosrv -> IO toclnt
askQuestion conn question = sendMessage conn question >> receiveMessage conn

timeout' :: Timespan -> IO a -> IO (Maybe a)
timeout' = timeout . timespanUs

timeout'' :: Timespan -> IO () -> IO ()
timeout'' timespan action = do
  Just () <- timeout' timespan action
  pure ()

runWS :: HasCallStack => Int -> (Connection -> IO a) -> IO a
runWS port action = do
  let host :: String = "localhost"
  runClient host port "/" action

-- | Like @createTestSession' wsBackendCfg@, but runs the 'Application' on a free port, and provides
-- a WS 'Connection' to it.
--
-- FIXME: builtinServer is broken.  you need to start a backend on port 3000 before running these
-- tests.
--
-- FUTUREWORK: a) integrate this with the code in "Refine.Backend.Test.AppServer"; b) multiple
-- connections for multiple users.
wsBackend :: (WSBackend -> IO ()) -> IO ()
wsBackend = timeout'' timeoutVal
          . if useExternalBackend then externalServer 3000 else builtinServer
  where
    builtinServer = wsBackend'
    externalServer port act = runWS port $ \conn -> act $ WSBackend Nothing port (Just conn)

wsBackend' :: (WSBackend -> IO ()) -> IO ()
wsBackend' action = do
    let cfg = wsBackendCfg
        plainWebSocks = False

    createTestSession' plainWebSocks cfg $ \(tbe :: TestBackend) -> do
      let runner :: Int -> IO ()
          runner port = do
            runWS port $ \conn -> do
              runDB tbe . unsafeAsGod $ initializeDB sampleContent
              action $ WSBackend (Just tbe) port (Just conn)
      if plainWebSocks
        then runner $ cfg ^. cfgWarpSettings . warpSettingsPort
        else testWithApplication (pure $ tbe ^?! testBackend . backendServer . _Right) runner

wsBackendCfg :: Config
wsBackendCfg = testBackendCfg
        & cfgLogger .~ LogCfg LogCfgDevNull minBound
        & cfgAllAreGods .~ False

data WSBackend = WSBackend
  { _testWSBackend :: Maybe TestBackend
  , _testWSPort    :: Int
  , _testWSConn    :: Maybe Connection
  }


-- * combinators

-- ** stress testing

throttle :: IO ()
throttle = threadDelay =<< randomRIO (1, 100 * 1000)

stresser :: Int -> Int -> IO (Async (Either String WSSessionId))
stresser port rounds = async . (`catch` (\(SomeException e) -> pure . Left $ show e)) $ do
  HandshakeToClientAcceptNew cid <- runWS port $ \conn -> askQuestion conn HandshakeToServerSyn
  forM_ [0.. (rounds - 1)] . const . runWS port $ \conn -> do
    when verbose_ $ hPutStr stderr "."
    HandshakeToClientAccept cid' _ <- askQuestion conn $ HandshakeToServerSynWith cid
    cid' `shouldBe` cid
    throttle
    uresp :: ToClient <- askQuestion conn $ TSCreateUser sampleCreateUser
    show uresp `shouldNotBe` mempty  -- (uresp will be 'TCError' in all but the first call by the first agent.)
  pure $ Right cid

stressers :: Int -> Int -> Int -> IO ()
stressers port agents rounds = do
  as :: [Async (Either String WSSessionId)] <- forM [0.. (agents - 1)] . const $ stresser port rounds
  result <- wait `mapM` as
  filter isLeft result `shouldSatisfy` null

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False


-- * test suite

spec :: Spec
spec = do
  -- specStories  -- FIXME: #458.
  -- specErrors   -- FIXME: #458.
  pure ()

specStories :: Spec
specStories = describe "stories" . around wsBackend $ do
  it "connect & handshake" $
    \(WSBackend _ port _) -> do
      cid <- runWS port $ \conn -> do
        HandshakeToClientAcceptNew cid <- askQuestion conn HandshakeToServerSyn
        pure cid
      runWS port $ \conn -> do
        HandshakeToClientAccept cid' [] <- askQuestion conn (HandshakeToServerSynWith cid)
        cid' `shouldBe` cid
      runWS port $ \conn -> do
        let cid'' = WSSessionId "<invalid>"
        HandshakeToClientAcceptNew cid' <- askQuestion conn (HandshakeToServerSynWith cid'')
        cid' `shouldNotBe` cid''

  it "disconnect" $ do
    \_ -> pendingWith "how do you test this?"

  it "works with many concurrent, high-volume sessions" $ do
    \(WSBackend _ port _) -> do
      stressers port 100 3
      -- FIXME: this hangs.  implement the same test with some js toolkit and run it on a different
      -- machine against a production backend?

  it "generating session ids is not too expensive" $ \_ -> do
    -- generating 10k session ids takes <2secs in ghci.  shrinking the getEntropy parameter or not
    -- calling 'wsBackend'' seems to have little effect on this.
    let n = 100
    when verbose_ $ print =<< getCurrentTime
    sids <- replicateM n mkWSSessionId
    when verbose_ $ print =<< getCurrentTime
    length sids `shouldBe` n

  it "cache invalidation propagation" $ do
    \_ -> pendingWith "how do you test this?"

  it "story 1" $ do
    \(WSBackend _ _ (Just conn)) -> do
      HandshakeToClientAcceptNew _ <- askQuestion conn HandshakeToServerSyn
      respLogin :: ToClient <- askQuestion conn $ TSLogin (Login "admin" "pass")
      show respLogin `shouldContain` "(Right (User {_userMetaID = MetaID {_miID = ID 1, _miMeta = MetaInfo {_metaCreatedBy = Anonymous"

      -- create document
      TCServerCache sc1 <- askQuestion conn $ TSMissing [CacheKeyGroupIds]
      let Just (gid :: ID Group) = sc1 ^? scGroupIds . _Just . to findMin
          cvdoc = sampleCreateVDoc & createVDocGroup .~ gid
      TCCreatedVDoc vid <- askQuestion conn $ TSAddVDoc cvdoc

      -- create edit
      TCServerCache sc2 <- askQuestion conn $ TSMissing [CacheKeyVDoc vid]
      let Just (vdoc :: VDoc) = sc2 ^? scVDocs . at vid . _Just
      TCServerCache _ <- askQuestion conn $ TSMissing [CacheKeyEdit (vdoc ^. vdocHeadEdit)]
      let cedit = CreateEdit "description" sampleRawContent2 Meaning
      TCInvalidateKeys [CacheKeyEdit _] <- askQuestion conn $ TSAddEdit (vdoc ^. vdocHeadEdit) cedit

      -- create discussion
      TCServerCache _ <- askQuestion conn $ TSMissing [CacheKeyEdit (vdoc ^. vdocHeadEdit)]
                         -- just to get notified when our edit is created.
      let cdisc = CreateDiscussion "statement" Nothing False
      TCInvalidateKeys [CacheKeyEdit _] <- askQuestion conn $ TSAddDiscussion (vdoc ^. vdocHeadEdit) cdisc

      -- reply to initial statement
      -- (remark: after adding a discussion, we have no access to the id we just created (except
      -- through creation time and the info from the creation request).  this is due to the
      -- disconnect between req and resp, which is nice for performance otherwise.)
      TCServerCache sc3 <- askQuestion conn $ TSMissing [CacheKeyEdit (vdoc ^. vdocHeadEdit)]
      let Just hdedit = sc3 ^? scEdits . at (vdoc ^. vdocHeadEdit) . _Just
          ((did :: ID Discussion) : _) = Map.keys $ hdedit ^. editDiscussions'
      TCServerCache sc4 <- askQuestion conn $ TSMissing [CacheKeyDiscussion did]
      let Just (disc :: Discussion) = sc4 ^? scDiscussions . at did . _Just
      TCInvalidateKeys [CacheKeyDiscussion did'] <- askQuestion conn $ TSAddStatement
        (disc ^. discussionTree . to Tree.rootLabel . statementID) (CreateStatement "not true!")
      did' `shouldBe` did

      -- update original statement
      TCServerCache sc5 <- askQuestion conn $ TSMissing
        [CacheKeyEdit (vdoc ^. vdocHeadEdit), CacheKeyDiscussion did]
      let Just (disc' :: Discussion) = sc5 ^? scDiscussions . at did . _Just
      TCInvalidateKeys [CacheKeyDiscussion did''] <- askQuestion conn $ TSUpdateStatement
        (disc' ^. discussionTree . to Tree.rootLabel . statementID) (CreateStatement "statement (yes true!)")
      did'' `shouldBe` did

  it "story 2" $ \_ -> do
    pending

    -- create two more edits
    -- merge all three edits
    -- extract combinators from the above test (?)
    -- votes
    -- ranges

    -- make sure every 'ToServer' constructor is at least called once.


specErrors :: Spec
specErrors = describe "errors" . around wsBackend $ do
  it "database lookup failure" $ \(WSBackend mtbe _ (Just conn)) -> do
    HandshakeToClientAcceptNew _ <- askQuestion conn HandshakeToServerSyn
    respLogin :: ToClient <- askQuestion conn $ TSLogin (Login "admin" "pass")
    show respLogin `shouldContain` "(Right (User {_userMetaID = MetaID {_miID = ID 1, _miMeta = MetaInfo {_metaCreatedBy = Anonymous"
    let noSuchGroup = ID 98691
    TCError _ <- askQuestion conn $ TSMissing [CacheKeyGroup noSuchGroup]
    case mtbe of
      Just tbe -> tbe `shouldHaveLogged` ["AppDBError (DBNotFound", show (noSuchGroup ^. unID)]
      Nothing  -> pendingWith "no test backend to check log in."


-- TODO: race condition: a) cache item gets updated in DB; b) somebody loads it; c) invalidation
-- gets sent.  this is just a performance issue, and not too likely (i think).  are there other race
-- conditions?


-- TODO: test gc (how?)


-- TODO: take all timespan values fixed in Server.WebSocket to Config (for easier testing in the
-- future).  some of them depend on each other.
