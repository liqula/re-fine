{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Backend.WebSocketSpec where

import Refine.Backend.Prelude

import Control.Concurrent
import Control.Concurrent.Async
import Data.Set (findMin)
import qualified Data.Tree as Tree
import qualified Data.Set as Set
import Network.WebSockets
import System.Random
import Test.Hspec

import Refine.Common.Types as Common
import Refine.Common.Test.Samples


-- * config

verbose :: Bool
verbose = True


-- * primitives

sendMessage :: HasCallStack => Connection -> ToServer -> IO ()
sendMessage conn msg = sendTextData conn (cs $ encode msg :: ST)

receiveMessage :: HasCallStack => Connection -> IO ToClient
receiveMessage conn = receiveData conn <&> fromMaybe (error "websockets json decoding error") . decode

askQuestion :: HasCallStack => Connection -> ToServer -> IO ToClient
askQuestion conn question = sendMessage conn question >> receiveMessage conn

runWS :: HasCallStack => (Connection -> IO a) -> IO a
runWS action = do
  let host :: String = "localhost"
      port :: Int    = 3000
  runClient host port "/" action


-- * combinators

-- ** stress testing

throttle :: IO ()
throttle = threadDelay =<< randomRIO (1, 1000000)

stresser :: Int -> IO (Async Int)
stresser rounds = async $ do
  TCGreeting cid <- runWS $ \conn ->
    sendMessage conn (TSGreeting Nothing) >> receiveMessage conn
  forM_ [0..rounds] $ \_ -> runWS $ \conn -> do
    when verbose $ hPutStrLn stderr ("<" <> show cid <> ">")
    sendMessage conn (TSGreeting (Just cid))
    throttle
    TCRestrictKeys [] <- receiveMessage conn
    throttle
    sendMessage conn (TSLogin (Login "nobody" mempty))
    _ <- receiveMessage conn
    when verbose $ hPutStrLn stderr ("</" <> show cid <> ">")
  pure cid

stressers :: Int -> Int -> IO ()
stressers agents rounds = do
  as :: [Async Int] <- forM [0..agents] . const $ stresser rounds
  result <- show <$> wait `mapM` as
  when verbose $ hPutStrLn stderr result
  length result `shouldNotBe` 0


-- * test suite

{- before these tests work, run this somewhere else:

cd pkg/backend/
stack build --fast && rm -rf .backend-data/ && stack exec refine -- --init sample-content.yaml server.conf && stack exec refine -- server.conf

server.conf:

_cfgPoolSize: 5
_cfgSessionLength:
  TimespanHours: 72
_cfgCsrfSecret: CSRF-SECRET
_cfgSmtp:
  _smtpSendmailArgs:
  - -t
  _smtpSenderEmail: postmaster@localhost
  _smtpDefaultRecipient: postmaster@localhost
  _smtpSendmailPath: /usr/sbin/sendmail
  _smtpSenderName: Re-fine Notification System
_cfgFileServeRoot: ../frontend/js-build
_cfgPoFilesRoot: ./po
_cfgLogger:
  _logCfgLevel: LogDebug
  _logCfgTarget: LogCfgStdOut
_cfgWarpSettings:
  _warpSettingsPort: 3000
  _warpSettingsHost: HostIPv4
_cfgDBKind:
  DBOnDisk: ./.backend-data/refine.db
  # DBInMemory
_cfgClient:
  _clientCfgWSHost: localhost
  _clientCfgWSPort: 3000
  _clientCfgWSSSL: False
_cfgWSPingPeriod:
  TimespanSecs: 14
_cfgAllAreGods: False


sample-content.yaml:

- CliCreateGroup:
    _createGroupTitle: Universe
    _createGroupDesc: The group that contains everything
    _createGroupParents: []
    _createGroupChildren: []
- CliCreateGroup:
    _createGroupTitle: Greek Party
    _createGroupDesc: Something about ethnics and politics
    _createGroupParents: []
    _createGroupChildren: []
- CliCreateUser:
  - _cuName: admin
    _cuPwd: pass
    _cuMail: admin@localhost
  - []
  - - GlobalAdmin
- CliCreateUser:
  - _cuName: edna
    _cuPwd: pass
    _cuMail: edna@localhost
  - - - GroupMember
      - Edna's home group
    - - GroupMember
      - Greek Party
  - []
- CliCreateUser:
  - _cuName: joe
    _cuPwd: pass
    _cuMail: joe@localhost
  - - - GroupMember
      - Joe's home group
  - []

-}


spec :: Spec
spec = describe "..." $ do
  it "works with many concurrent, high-volume sessions" $ do
    pendingWith "this only works if you run the backend server separately outside the test suite."  -- TODO
    stressers 100 3

  it "story 1" $ do
    pendingWith "this only works if you run the backend server separately outside the test suite."  -- TODO
    runWS $ \conn -> do
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
          ((did :: ID Discussion) : _) = Set.toList $ hdedit ^. editDiscussions'
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

  it "story 2" $ do
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
