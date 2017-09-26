{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.WebSocketSpec where
#include "import_backend.hs"

import Control.Concurrent
import Control.Concurrent.Async
import Data.Set (findMin)
import Network.Wai.Handler.Warp
import Network.WebSockets
import System.Random
import System.Timeout
import Test.Hspec

import Refine.Backend.App.Access
import Refine.Backend.App.Cache (mkWSSessionId)
import Refine.Backend.App.MigrateDB (initializeDB)
import Refine.Backend.Config
import Refine.Backend.Server (backendServer)
import Refine.Backend.Test.AppServer
import Refine.Common.Rest
import Refine.Common.Test
import Refine.Common.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- * config

verbose_ :: Bool
verbose_ = True


-- * primitives

sendMessage :: HasCallStack => Connection -> ToServer -> IO ()
sendMessage conn msg = sendTextData conn (cs $ encode msg :: ST)

receiveMessage :: HasCallStack => Connection -> IO ToClient
receiveMessage conn = receiveData conn <&> fromMaybe (error "websockets json decoding error") . decode

askQuestion :: HasCallStack => Connection -> ToServer -> IO ToClient
askQuestion conn question
  = fromMaybe (error $ "askQuestion timed out: " <> show question)
    <$> timeout' (TimespanSecs 1) (sendMessage conn question >> receiveMessage conn)

timeout' :: Timespan -> IO a -> IO (Maybe a)
timeout' = timeout . timespanUs

runWS :: HasCallStack => Int -> (Connection -> IO a) -> IO a
runWS port action = do
  let host :: String = "localhost"
  runClient host port "/" action

-- | Like @createTestSession' wsBackendCfg@, but runs the 'Application' on a free port, and provides
-- a WS 'Connection' to it.
--
-- FUTUREWORK: a) integrate this with the code in "Refine.Backend.Test.AppServer"; b) multiple
-- connections for multiple users.
wsBackend :: (WSBackend -> IO ()) -> IO ()
wsBackend = wsBackend' (TimespanSecs 3)

wsBackend' :: Timespan -> (WSBackend -> IO ()) -> IO ()
wsBackend' timespan action = do
  Just () <- timeout' timespan $ do
    let cfg = wsBackendCfg
    createTestSession' cfg $ \(tbe :: TestBackend) -> do
      testWithApplication (pure . backendServer . view testBackend $ tbe) $ \port -> do
        runWS port $ \conn -> do
          runDB tbe . unsafeAsGod $ initializeDB sampleContent
          action $ WSBackend tbe port conn
  pure ()

wsBackendCfg :: Config
wsBackendCfg = testBackendCfg
        & cfgLogger .~ LogCfg (LogCfgFile testLogfilePath) LogDebug
        & cfgAllAreGods .~ False

data WSBackend = WSBackend
  { _testWSBackend :: TestBackend
  , _testWSPort    :: Int
  , _testWSConn    :: Connection
  }


-- * combinators

-- ** stress testing

throttle :: IO ()
throttle = threadDelay =<< randomRIO (1, 1000000)

stresser :: Int -> Int -> IO (Async WSSessionId)
stresser port rounds = async $ do
  TCGreeting cid <- runWS port $ \conn ->
    sendMessage conn (TSGreeting Nothing) >> receiveMessage conn
  forM_ [0..rounds] $ \_ -> runWS port $ \conn -> do
    when verbose_ $ hPutStrLn stderr ("<" <> show cid <> ">")
    sendMessage conn (TSGreeting (Just cid))
    throttle
    TCRestrictKeys [] <- receiveMessage conn
    throttle
    sendMessage conn (TSLogin (Login "nobody" mempty))
    _ <- receiveMessage conn
    when verbose_ $ hPutStrLn stderr ("</" <> show cid <> ">")
  pure cid

stressers :: Int -> Int -> Int -> IO ()
stressers port agents rounds = do
  as :: [Async WSSessionId] <- forM [0..agents] . const $ stresser port rounds
  result <- show <$> wait `mapM` as
  when verbose_ $ hPutStrLn stderr result
  length result `shouldNotBe` 0


-- * test suite

spec :: Spec
spec = do
  -- specStories  # FIXME: #458
  specErrors

specStories :: Spec
specStories = describe "stories" . around wsBackend $ do
  it "works with many concurrent, high-volume sessions" $ do
    \(WSBackend _ port _) -> do
      pendingWith "this test is broken."
      stressers port 100 3

  it "generating session ids is not too expensive" $ \_ -> do
    -- generating 10k session ids takes <2secs in ghci.  shrinking the getEntropy parameter has little effect on this.
    let n = 100
    () <- print =<< getCurrentTime
    sids <- replicateM n mkWSSessionId
    () <- print =<< getCurrentTime
    length sids `shouldBe` n

  it "story 1" $ do
    \(WSBackend _ _ conn) -> do
      TCGreeting _ <- askQuestion conn $ TSGreeting Nothing
      respLogin <- askQuestion conn $ TSLogin (Login "admin" "pass")
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


  -- run test server from within these tests.  perhaps for speedup, run it beforeall, and make the
  -- tests self-contained.  or whichever is faster to implement and maintain.



{-

make sure we've called all of these at least once:

  | TSClearCache
  | TSUpdateVDoc (ID VDoc) UpdateVDoc
  | TSUpdateEdit (ID Edit) CreateEdit
  | TSUpdateGroup (ID Group) CreateGroup
  | TSMergeEdit (ID Edit)
  | TSToggleVote ContributionID Vote
  | TSDeleteVote (ID Edit) -- not used yet
  | TSAddGroup CreateGroup
  | TSAddEditAndMerge (ID Edit){-parent-} CreateEdit
  | TSCreateUser CreateUser
  | TSLogout
  | TSGetTranslations GetTranslations

-}



  -- do all this with examples, no arbitrary!


specErrors :: Spec
specErrors = describe "errors" . around wsBackend $ do
  it "database lookup failure" $ \(WSBackend _ _ conn) -> do
    TCGreeting _ <- askQuestion conn $ TSGreeting Nothing
    respLogin <- askQuestion conn $ TSLogin (Login "admin" "pass")
    show respLogin `shouldContain` "(Right (User {_userMetaID = MetaID {_miID = ID 1, _miMeta = MetaInfo {_metaCreatedBy = Anonymous"
    let noSuchGroup = ID 98691
    TCError (ApiDBError (ApiDBNotFound "not found: ID 98691 :: ID Group")) <- askQuestion conn $ TSMissing [CacheKeyGroup noSuchGroup]
    logShouldContain `mapM_` ["AppDBError (DBNotFound", show (noSuchGroup ^. unID)]


logShouldContain :: String -> Expectation
logShouldContain substr = readTestLogfile >>= (`shouldContain` substr)
