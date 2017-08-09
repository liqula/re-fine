{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.ServerSpec where

import Refine.Backend.Prelude hiding (Header)

import           Control.Concurrent.MVar
import qualified Data.ByteString as SBS
import qualified Data.Map as Map
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Network.HTTP.Types (Method, Header, methodGet, methodPut, methodDelete)
import           Network.HTTP.Types.Status (Status(statusCode))
import           Network.URI (URI, uriToString)
import           Network.Wai (requestMethod, requestHeaders, defaultRequest)
import           Network.Wai.Test (SRequest(..), SResponse(..))
import qualified Network.Wai.Test as Wai
import qualified Network.Wai.Test.Internal as Wai
import           Test.Hspec
import qualified Web.Users.Types as Users

import Refine.Backend.App as App hiding (getEdit)
import Refine.Backend.Config
import Refine.Backend.Database.Class as DB
import Refine.Backend.Database (DB)
import Refine.Backend.Database.Entity (toUserID)
import Refine.Backend.Server
import Refine.Backend.Test.Util (withTempCurrentDirectory, sampleMetaID)
import Refine.Common.OT hiding (Edit)
import Refine.Common.ChangeAPI
import Refine.Common.Rest
import Refine.Common.Types as Common

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- * machine room

-- | This type carries a 'Backend' (which contains of an 'AppM' and an 'Application'), plus a wai
-- test client.  (It is only used in this module, so it does not need to go to
-- "Refine.Backend.Test.AppRunner", where an 'AppM' tester is provided.)
--
-- FUTUREWORK: actually, i think that "Refine.Backend.Test.AppRunner" and the code here could
-- benefit from some refactoring to reduce code duplication.
data TestBackend = TestBackend
  { _testBackend      :: Backend DB
  , _testBackendState :: MVar Wai.ClientState
  }

makeLenses ''TestBackend

runWai :: TestBackend -> Wai.Session a -> IO a
runWai sess m = do
  st <- takeMVar (sess ^. testBackendState)
  (a, st') <- Wai.runSessionWith st m (backendServer (sess ^. testBackend))
  putMVar (sess ^. testBackendState) st'
  pure a

-- | Call 'runWai' and throw an error if the expected type cannot be read, discard the
-- response, keep only the body.
runWaiJSON :: FromJSON a => TestBackend -> Wai.Session SResponse -> IO a
runWaiJSON sess m = do
  resp <- runWai sess m
  case eitherDecode $ simpleBody resp of
    Left err -> throwIO . ErrorCall $ unwords [show err, show (simpleHeaders resp), cs (simpleBody resp)]
    Right x  -> pure x

-- | Call 'runDB'' and crash on 'Left'.
runDB :: TestBackend -> AppM DB a -> IO a
runDB sess = errorOnLeft . runDB' sess

-- | Call an 'App' action.
--
-- FIXME: This does not share session state with the rest api
-- interface in @backendServer (sess ^. testBackend)@.  the functions
-- to establish that are *almost* done, but to finish them we need to
-- understand how servant-cookie-session uses vault to store data in
-- cookies, and either emulate that or (preferably) call the
-- resp. functions.
runDB' :: TestBackend -> AppM DB a -> IO (Either AppError a)
runDB' sess = runExceptT . unwrapNT (backendRunApp (sess ^. testBackend))
{-
  ... $ do
    storeAppState sess
    action
    recoverAppState sess
  where
    storeAppState :: TestBackend uh -> AppM db uh ()
    storeAppState sess = do
      st :: AppState <- appIO $ parseCookies <$> readMVar (sess ^. testBackendState)
      State.put st
      where
        parseCookies :: Wai.ClientState -> AppState
        parseCookies _ = _  -- AppState Nothing UserLoggedOut

    recoverAppState :: TestBackend uh -> AppM db uh ()
    recoverAppState sess = do
      st' :: AppState <- State.get
      appIO $ putMVar (sess ^. testBackendState) (resetCookies st')
      where
        resetCookies :: AppState -> Wai.ClientState
        resetCookies _ = _  -- Wai.ClientState mempty
-}

errorOnLeft :: Show e => IO (Either e a) -> IO a
errorOnLeft action = either (throwIO . ErrorCall . show') pure =<< action
  where
    show' x = "errorOnLeft: " <> show x

testLogfilePath :: FilePath
testLogfilePath = "logfile"

readTestLogfile :: IO String
readTestLogfile = readFile testLogfilePath

-- | Create session via 'mkProdBackend' (using 'UH').
createTestSession :: (TestBackend -> IO ()) -> IO ()
createTestSession action = withTempCurrentDirectory $ do
  let cfg = def
        & cfgLogger .~ LogCfgFile testLogfilePath
        & cfgSmtp .~ Nothing
  void $ action =<< (TestBackend <$> mkProdBackend cfg <*> newMVar Wai.initState)

createTestSessionWith :: (TestBackend -> IO ()) -> (TestBackend -> IO ()) -> IO ()
createTestSessionWith initAction action = createTestSession $ \sess -> do
  () <- initAction sess
  () <- action sess
  pure ()


-- * test helpers

-- (see also: https://hackage.haskell.org/package/wai-extra/docs/Network-Wai-Test.html)

sampleCreateVDoc :: CreateVDoc
sampleCreateVDoc = CreateVDoc
  (Title "[title]")
  (Abstract "[abstract]")
  sampleCreateVDocE0
  defaultGroupID

sampleCreateVDocE0 :: RawContent
sampleCreateVDocE0 = mkRawContent $ mkBlock "[versioned content]" :| []

sampleCreateVDocE1 :: RawContent
sampleCreateVDocE1 = mkRawContent $ mkBlock "[versioned content, edit1]" :| []

sampleCreateVDocE2 :: RawContent
sampleCreateVDocE2 = mkRawContent $ mkBlock "[versioned, edit2, content]" :| []

respCode :: SResponse -> Int
respCode = statusCode . simpleStatus

wget :: SBS -> Wai.Session SResponse
wget path = request methodGet path [] ""

wput :: SBS -> Wai.Session SResponse
wput path = request methodPut path [] ""

wdel :: SBS -> Wai.Session SResponse
wdel path = request methodDelete path [] ""

post :: (ToJSON a) => SBS -> a -> Wai.Session SResponse
post path js = request "POST" path [("Content-Type", "application/json")] (encode js)

putJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> a -> Wai.Session b
putJSON = rqJSON "PUT"

postJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> a -> Wai.Session b
postJSON = rqJSON "POST"

rqJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> SBS -> a -> Wai.Session b
rqJSON method path js = do
  resp <- request method path [("Content-Type", "application/json")] (encode js)
  liftIO $ case eitherDecode $ simpleBody resp of
    Left err -> throwIO . ErrorCall $ unlines [cs path, show (typeOf js), show resp, show err]
    Right x  -> pure x

-- | This is from hspec-wai, but we modified it to work on 'Wai.Session' directly.  'WaiSession'
-- does not keep the cookies in the 'Session' between requests.
request :: Method -> SBS -> [Header] -> LBS -> Wai.Session SResponse
request method path headers body = Wai.srequest $ SRequest req body
  where
    req = Wai.setPath defaultRequest {requestMethod = method, requestHeaders = headers} path


mkCVDoc :: TestBackend -> CreateVDoc -> IO CompositeVDoc
mkCVDoc sess vdoc = runWai sess $ postJSON createVDocUri vdoc

mkEdit :: TestBackend -> IO (ID Edit)
mkEdit = fmap (^. compositeVDocThisEditID) . (`mkCVDoc` sampleCreateVDoc)


testUsername :: Username
testUsername = "testUsername"

testUserEmail :: Username
testUserEmail = "testUsername@email.com"

testPassword :: Password
testPassword = "testPassword"

addUserAndLogin :: TestBackend -> Username -> ST -> IO ()
addUserAndLogin sess username useremail = runWai sess $ do
  r1 <- post createUserUri $ CreateUser username useremail testPassword
  r2 <- post loginUri $ Login username testPassword
  if any ((>= 300) . respCode) [r1, r2]
    then error $ "addUserAndLogin: " <> show (username, [r1, r2])
    else pure ()

addTestUserAndLogin :: TestBackend -> IO ()
addTestUserAndLogin sess = addUserAndLogin sess testUsername testUserEmail

mkTestUserAndEditAndLogin :: TestBackend -> IO (ID Edit)
mkTestUserAndEditAndLogin sess = addTestUserAndLogin sess >> mkEdit sess

-- | we're just hoping this is the ID of the default group that is created in 'mkProdBackend'.  if
-- this fails, we need to be smarter about constructing the test cases here.
defaultGroupID :: ID Group
defaultGroupID = ID 1


-- * endpoints

uriStr :: URI -> SBS
uriStr u =  cs $ "/" <> uriToString id u ""

getVDocUri :: ID VDoc -> SBS
getVDocUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetVDoc)

createVDocUri :: SBS
createVDocUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SCreateVDoc)

updateVDocUri :: ID VDoc -> SBS
updateVDocUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SUpdateVDoc)

addEditUri :: ID Edit -> SBS
addEditUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddEdit)

updateEditUri :: ID Edit -> SBS
updateEditUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SUpdateEdit)

addNoteUri :: ID Edit -> SBS
addNoteUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddNote)

addDiscussionUri :: ID Edit -> SBS
addDiscussionUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddDiscussion)

createUserUri :: SBS
createUserUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SCreateUser)

loginUri :: SBS
loginUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SLogin)

logoutUri :: SBS
logoutUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SLogout)

changeRoleUri :: SBS
changeRoleUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SChangeRole)

putVoteUri :: ID Edit -> Vote -> SBS
putVoteUri eid v = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SPutSimpleVoteOnEdit) eid v

deleteVoteUri :: ID Edit ->  SBS
deleteVoteUri eid = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SDeleteSimpleVoteOnEdit) eid

getVotesUri :: ID Edit -> SBS
getVotesUri eid = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetSimpleVotesOnEdit) eid


-- * test cases

spec :: Spec
spec = do -- FUTUREWORK: mark this as 'parallel' (needs some work)
  specMockedLogin
  specUserHandling
  specVoting
  specSmtp

specMockedLogin :: Spec
specMockedLogin = around (createTestSessionWith addTestUserAndLogin) $ do
  describe "sGetVDoc" $ do
    it "retrieves a vdoc" $ \sess -> do
      vdoc <- runDB sess $ App.createVDoc sampleCreateVDoc
      resp <- runWai sess . wget $ getVDocUri (vdoc ^. vdocID)
      respCode resp `shouldBe` 200

  describe "sCreateVDoc" $ do
    it "stores a vdoc in the db" $ \sess -> do
      fe :: CompositeVDoc <- runWai sess $ postJSON createVDocUri sampleCreateVDoc
      be :: CompositeVDoc <- runDB  sess $ getCompositeVDocOnHead (fe ^. compositeVDoc . vdocID)
      fe `shouldBe` be

  describe "sUpdateVDoc" $ do
    it "stores new title, abstract in vdoc in the db" $ \sess -> do
      bef :: CompositeVDoc <- runWai sess $ postJSON createVDocUri sampleCreateVDoc
      let vid = bef ^. compositeVDoc . vdocID
          newabstract = Abstract "newabs"
          newtitle = Title "newtitle"

      after1 :: VDoc <- runWai sess $ putJSON (updateVDocUri vid) (UpdateVDoc newtitle newabstract)
      after2  :: CompositeVDoc <- runDB  sess $ getCompositeVDocOnHead vid

      (after1 ^. vdocTitle)                    `shouldBe` newtitle
      (after1 ^. vdocAbstract)                 `shouldBe` newabstract
      (after2 ^. compositeVDoc . vdocTitle)    `shouldBe` newtitle
      (after2 ^. compositeVDoc . vdocAbstract) `shouldBe` newabstract

  describe "sAddNote" $ do
    it "stores note with full-document chunk range" $ \sess -> do
      runWai sess $ do
        un :: User <- postJSON loginUri $ Login testUsername testPassword
        liftIO $ (un ^. userName) `shouldBe` testUsername
        fe_ :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc
        let cp1 = Position (BlockIndex 0 $ BlockKey "0") 0
            cp2 = Position (BlockIndex 0 $ BlockKey "0") 1
        fn_ :: Note          <- postJSON
            (addNoteUri (fe_ ^. compositeVDoc . vdocHeadEdit))
            (CreateNote "[note]" True (Range cp1 cp2))
        liftIO $ do
          be :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (fe_ ^. compositeVDoc . vdocID)
          be ^. compositeVDocApplicableNotes . to Map.elems `shouldContain` [fn_]

    it "stores note with non-trivial valid chunk range" $ \sess -> do
      runWai sess $ do
        un :: User <- postJSON loginUri $ Login testUsername testPassword
        liftIO $ (un ^. userName) `shouldBe` testUsername
        fe_ :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc
        let cp1 = Position (BlockIndex 1 $ BlockKey "1") 0
            cp2 = Position (BlockIndex 1 $ BlockKey "1") 1
        fn_ :: Note <- postJSON
          (addNoteUri (fe_ ^. compositeVDoc . vdocHeadEdit))
          (CreateNote "[note]" True (Range cp1 cp2))

        liftIO $ do
          be :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (fe_ ^. compositeVDoc . vdocID)
          be ^. compositeVDocApplicableNotes . to Map.elems `shouldContain` [fn_]

    it "fails with error on non-trivial *invalid* chunk range" $ \sess -> do
      vdoc :: CompositeVDoc <- runWai sess $ postJSON createVDocUri sampleCreateVDoc
      resp :: SResponse <- runWai sess $
        let cp1, cp2 :: Position
            cp1 = Position (BlockIndex 1 $ BlockKey "1") 0
            cp2 = Position (BlockIndex 100 $ BlockKey "100") 100
        in post
          (addNoteUri (vdoc ^. compositeVDoc . vdocHeadEdit))
          (CreateNote "[note]" True (Range cp1 cp2))

      pendingWith "'validateCreateChunkRange' is not implemented yet."

      respCode resp `shouldBe` 409
      cs (simpleBody resp) `shouldContain` ("ChunkRangeBadDataUID" :: String)

      vdoc' :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (vdoc ^. compositeVDoc . vdocID)
      vdoc' ^. compositeVDocApplicableNotes `shouldBe` mempty

  describe "sAddDiscussion" $ do
    it "stores discussion with no ranges" $ \sess -> do
      runWai sess $ do
        un :: User <- postJSON loginUri $ Login testUsername testPassword
        liftIO $ (un ^. userName) `shouldBe` testUsername
        fe_ :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc
        let cp1 = Position (BlockIndex 0 $ BlockKey "1") 0
            cp2 = Position (BlockIndex 0 $ BlockKey "1") 1
        fn_ :: Discussion <-
          postJSON
            (addDiscussionUri (fe_ ^. compositeVDoc . vdocHeadEdit))
            (CreateDiscussion "[discussion initial statement]" True (Range cp1 cp2))

        liftIO $ do
          be :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (fe_ ^. compositeVDoc . vdocID)
          be ^. compositeVDocApplicableDiscussions . to Map.elems `shouldContain` [fn_]

  describe "sAddStatement" $ do
    it "stores statement for given discussion" $ \_sess -> do
      pendingWith "this test case shouldn't be too hard to write, and should be working already."

  describe "sAddEdit, sUpdateEdit" $ do
    let samplevdoc = mkRawContent $ mkBlock "[new vdoc version]" :| []
    let setup sess = do
         group <- fmap (^. groupID) . runDB sess $ App.addGroup (CreateGroup "title" "desc" [] [])
         runWai sess $ do
          _l :: User <- postJSON loginUri (Login testUsername testPassword)
          fc :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc

          userId <- liftIO . runDB sess $ do
            (Just loginId) <- dbUsersCmd $ \db_ -> Users.getUserIdByName db_ testUsername
            pure (toUserID loginId)

          () <- postJSON changeRoleUri AssignRole
                  { _crGroupRef = group
                  , _crUser     = userId
                  , _crRole     = Member
                  }

          fe :: Edit <-
            postJSON
              (addEditUri (fc ^. compositeVDoc . vdocHeadEdit))
              (CreateEdit
                "new edit"
                samplevdoc
                Grammar)
          pure (fc, fe)

    context "on edit without ranges" $ do
      it "stores an edit and returns its version" $ \sess -> do
        (_, fp) <- setup sess
        be' :: RawContent <- runDB sess . db . getVersion $ fp ^. editID
        be' `shouldBe` samplevdoc

      it "stores an edit and returns it in the list of edits applicable to its base" $ \sess -> do
        pendingWith "applicableEdits is not implemented."
        (fe, fp) <- setup sess
        be :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (fe ^. compositeVDoc . vdocID)
        be ^. compositeVDocApplicableEdits . to Map.elems`shouldContain` [fp]

    describe "sUpdateEdit" $ do
      it "works" $ \sess -> do
        (_, fp) <- setup sess

        let d = mkRawContent $ mkBlock "1234567890" :| []
        _ :: Edit <- runWai sess $
            putJSON
              (updateEditUri (fp ^. editID))
              (CreateEdit
                "updated edit"
                d
                Meaning)

        edit <- runDB sess . db . getEdit $ fp ^. editID
        edit ^. editVDocVersion `shouldBe` d
        edit ^. editKind `shouldBe` Meaning
        edit ^. editDesc `shouldBe` "updated edit"
        length (edit ^. editSource . unEditSource) `shouldBe` 1
        fst (head $ edit ^. editSource . unEditSource) `shouldBe`
          [ ERawContent
            [ ENonEmpty $ EditItem 0
              [ EditSecond (SegmentListEdit (InsertItem 0 ((Atom Nothing, mempty),NonEmptyST "[new vdoc version]")))
              , EditSecond (SegmentListEdit (DeleteRange 1 1))
              ]
            ]
          , ERawContent
            [ ENonEmpty $ EditItem 0
              [ EditSecond (SegmentListEdit (InsertItem 0 ((Atom Nothing, mempty),NonEmptyST "1234567890")))
              , EditSecond (SegmentListEdit (DeleteRange 1 1))
              ]
            ]
          ]

      it "update merged edit" $ \_sess -> do
        pending

      it "check that modification time is updated on edit update" $ \_sess -> do
        pending

specUserHandling :: Spec
specUserHandling = around createTestSession $ do
  describe "User handling" $ do
    let doCreate = post createUserUri (CreateUser testUsername testUserEmail testPassword)
        doLogin = post loginUri
        doLogout = post logoutUri ()

        checkCookie resp = simpleHeaders resp `shouldSatisfy`
            any (\(k, v) -> k == "Set-Cookie" && refineCookieName `SBS.isPrefixOf` v)

    describe "create" $ do
      it "works" $ \sess -> do
        timeBefore <- getCurrentTimestamp
        u :: User <- runWaiJSON sess doCreate
        let timeThen = u ^. userMetaID . miMeta . metaCreatedAt
        timeAfter <- getCurrentTimestamp
        u ^. userID `shouldBe` sampleMetaID ^. miID
        timeBefore `shouldSatisfy` (< timeThen)
        timeThen   `shouldSatisfy` (< timeAfter)

      it "is secure" $ \_ -> do
        pendingWith "needs design & implementation: what makes a create requests legit?"

    describe "login" $ do
      context "with valid credentials" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogin (Login testUsername testPassword)
          respCode resp `shouldBe` 200
          checkCookie resp

          pendingWith "see fixme in runDB'"
          user <- runDB sess App.currentUser
          user `shouldSatisfy` isJust

      context "with invalid credentials" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogin (Login testUsername "")
          respCode resp `shouldBe` 404
          checkCookie resp

    describe "logout" $ do
      context "logged in" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogin (Login testUsername testPassword) >> doLogout
          respCode resp `shouldBe` 200
          checkCookie resp

      context "logged out" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogout
          respCode resp `shouldBe` 200
          checkCookie resp

specVoting :: Spec
specVoting = around createTestSession $ do
  describe "SPutSimpleVoteOnEdit" $ do
    context "user is not logged in" $ do
      it "request is rejected" $ \sess -> do
        eid <- mkTestUserAndEditAndLogin sess
        _ <- runWai sess $ post logoutUri ()
        resp <- runWai sess . wput $ putVoteUri eid Yeay
        respCode resp `shouldSatisfy` (>= 400)

    context "if current user *HAS NOT* voted on the edit before" $ do
      it "adds the current user's vote (and does nothing else)" $ \sess -> do
        eid <- mkTestUserAndEditAndLogin sess
        resp :: SResponse <- runWai sess . wput $ putVoteUri eid Yeay
        respCode resp `shouldBe` 200
        -- votes <- runDB sess $ App.getSimpleVotesOnEdit eid  -- see FIXME at 'runDB''
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Yeay, 1)]

    context "if current user *HAS* voted on the edit before" $ do
      it "adds the current user's vote (and does nothing else)" $ \sess -> do
        eid <- mkTestUserAndEditAndLogin sess
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        resp :: SResponse <- runWai sess . wput $ putVoteUri eid Nay
        respCode resp `shouldBe` 200
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Nay, 1)]

  describe "SDeleteSimpleVoteOnEdit" $ do
    context "user is not logged in" $ do
      it "request is rejected" $ \sess -> do
        eid <- mkTestUserAndEditAndLogin sess
        _ <- runWai sess $ post logoutUri ()
        resp <- runWai sess . wdel $ deleteVoteUri eid
        respCode resp `shouldSatisfy` (>= 400)

    context "if there is such a vote" $ do
      it "removes that vote (and does nothing else)" $ \sess -> do
        eid <- mkTestUserAndEditAndLogin sess
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        resp :: SResponse <- runWai sess . wdel $ deleteVoteUri eid
        respCode resp `shouldBe` 200
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList []

    context "if there is no such vote" $ do
      it "does nothing" $ \sess -> do
        eid <- mkTestUserAndEditAndLogin sess
        resp :: SResponse <- runWai sess . wdel $ deleteVoteUri eid
        respCode resp `shouldBe` 200
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList []

  describe "SGetSimpleVotesOnEdit" $ do
    context "with two Yeays and one Nay" $ do
      it "returns (2, 1)" $ \sess -> do
        eid <- mkEdit sess
        addUserAndLogin sess "userA" "userA@email.com"
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        addUserAndLogin sess "userB" "userB@email.com"
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        addUserAndLogin sess "userC" "userC@email.com"
        _ <- runWai sess . wput $ putVoteUri eid Nay
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Yeay, 2), (Nay, 1)]

    context "with two Yeays and one Nay, and after changing one Yeay into a Nay" $ do
      it "returns (1, 2)" $ \sess -> do
        eid <- mkEdit sess
        addUserAndLogin sess "userA" "userA@email.com"
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        addUserAndLogin sess "userB" "userB@email.com"
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        _ <- runWai sess . wput $ putVoteUri eid Nay
        addUserAndLogin sess "userC" "userC@email.com"
        _ <- runWai sess . wput $ putVoteUri eid Nay
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Yeay, 1), (Nay, 2)]

  describe "merging and rebasing" $ do
    it "works if two edits are present and one is merged" $ \sess -> do
      addUserAndLogin sess "userA" "userA@email.com"

      let blocks = mkBlock <$> ["first line", "second line", "third line"]
          vdoc ~(b:bs) = mkRawContent $ b :| bs

      cvdoc <- mkCVDoc sess $ CreateVDoc (Title "[title]") (Abstract "[abstract]") (vdoc blocks) defaultGroupID

      let ce1 :: CreateEdit = CreateEdit "description" (vdoc [head blocks, blocks !! 1]) Grammar
          ce2 :: CreateEdit = CreateEdit "description" (vdoc [head blocks, blocks !! 2]) Grammar

      (e1,  e2) <- runWai sess $ do
        [e1_, e2_] <- postJSON (addEditUri (cvdoc ^. compositeVDoc . vdocHeadEdit)) `mapM` [ce1, ce2]
        pure (e1_, e2_)

      resp <- runWai sess . wput $ putVoteUri (e1 ^. editID) Yeay
      respCode resp `shouldSatisfy` (< 400)

      -- composite vdoc should point to e1
      cvdoc' :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (cvdoc ^. compositeVDoc . vdocID)
      cvdoc' ^. compositeVDocThisEdit . editID `shouldBe` e1 ^. editID

      -- e2 should be re-based onto e1
      let rebasedEdits = Map.elems (cvdoc' ^. compositeVDocApplicableEdits)
      length rebasedEdits `shouldBe` 1
      head rebasedEdits ^. editID   `shouldNotBe` e2 ^. editID  -- (rebase is immutable)
      head rebasedEdits ^. editDesc `shouldBe` "description"
      head rebasedEdits ^. editKind `shouldBe` Grammar
      -- (compare versions, too?  that will probably break once we get fancier merge heuristics, though.)

specSmtp :: Spec
specSmtp = describe "smtp" . around (createTestSessionWith addTestUserAndLogin) $ do
  it "sendMailTo sends emails" $ \sess -> do
    let msg = EmailMessage (Address (Just "yourname") "you@example.com") emailSubject emailBody
        emailSubject = "363afea9ec84d430c"
        emailBody = "969046a5ba584948471218672256dafbcb5986e3964ec49"
    () <- runDB sess $ App.sendMailTo msg
    logs <- readTestLogfile
    logs `shouldContain` "sendMailTo:"
    logs `shouldContain` cs emailSubject
    logs `shouldContain` cs emailBody
    logs `shouldContain` show msg

  -- this is pretty close to an acceptance test: given one user on the system, create a doc and
  -- two edits.  upvote one edit, which triggers a merge.  then check that we get an email about
  -- the other edit getting rebased.
  let itNotifiesOnRebase msg trigger = it msg $ \sess -> do
        oldHead :: ID Edit
          <- mkEdit sess  -- saves sampleCreateVDoc
        firstEdit :: Edit
          <- runWai sess $ postJSON (addEditUri oldHead) (CreateEdit "1st" sampleCreateVDocE1 Grammar)
        () <- trigger sess oldHead

        notyet <- readTestLogfile
        notyet `shouldNotContain` "your stuff has changed."

        () <- runWai sess $ putJSON (putVoteUri (firstEdit ^. editID) Yeay) ()

        butnow <- drop (length notyet) <$> readTestLogfile
        butnow `shouldContain` "your stuff has changed."
        butnow `shouldContain` cs testUserEmail
        butnow `shouldContain` cs testUsername

  itNotifiesOnRebase "when my edit gets rebased, i get an email" $ \sess base -> do
    _secondEditEdit :: Edit
      <- runWai sess $ postJSON (addEditUri base) (CreateEdit "1st" sampleCreateVDocE1 Grammar)
    pure ()

  itNotifiesOnRebase "when my discussion gets rebased, i get an email" $ \sess base -> do
    let cp1 = Position (BlockIndex 0 $ BlockKey "1") 0
        cp2 = Position (BlockIndex 0 $ BlockKey "1") 1
    _firstDiscussion :: Discussion
      <- runWai sess $ postJSON (addDiscussionUri base)
            (CreateDiscussion "[discussion initial statement]" True (Range cp1 cp2))
    pure ()
