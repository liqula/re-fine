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

import Refine.Backend.App as App
import Refine.Backend.App.MigrateDB (initializeDB)
import Refine.Backend.Config
import Refine.Backend.Database.Class as DB
import Refine.Backend.Database (DB)
import Refine.Backend.Natural
import Refine.Backend.Server
import Refine.Backend.Test.Util (withTempCurrentDirectory, sampleMetaID)
import Refine.Backend.User
import Refine.Common.ChangeAPI
import Refine.Common.Rest
import Refine.Common.Types as Common
import Refine.Common.VDoc.Draft

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- * machine room

data TestBackend uh = TestBackend
  { _testBackend      :: Backend DB uh
  , _testBackendState :: MVar Wai.ClientState
  }

makeLenses ''TestBackend

runWai :: TestBackend uh -> Wai.Session a -> IO a
runWai sess m = do
  st <- takeMVar (sess ^. testBackendState)
  (a, st') <- Wai.runSessionWith st m (backendServer (sess ^. testBackend))
  putMVar (sess ^. testBackendState) st'
  pure a

-- | Call 'runWai' and throw an error if the expected type cannot be read, discard the
-- response, keep only the body.
runWaiJSON :: FromJSON a => TestBackend uh -> Wai.Session SResponse -> IO a
runWaiJSON sess m = do
  resp <- runWai sess m
  case eitherDecode $ simpleBody resp of
    Left err -> throwIO . ErrorCall $ unwords [show err, show (simpleHeaders resp), cs (simpleBody resp)]
    Right x  -> pure x

-- | Call 'runDB'' and crash on 'Left'.
runDB :: TestBackend uh -> AppM DB uh a -> IO a
runDB sess = errorOnLeft . runDB' sess

-- | Call an 'App' action.
--
-- FIXME: This does not share session state with the rest api
-- interface in @backendServer (sess ^. testBackend)@.  the functions
-- to establish that are *almost* done, but to finish them we need to
-- understand how servant-cookie-session uses vault to store data in
-- cookies, and either emulate that or (preferably) call the
-- resp. functions.
runDB' :: TestBackend uh -> AppM DB uh a -> IO (Either AppError a)
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


-- | Create session via 'mkDevModeBackend' (using 'MockUH_'), and
-- create universal group before running the action.
createDevModeTestSession :: ActionWith (TestBackend FreeUH) -> IO ()
createDevModeTestSession action = withTempCurrentDirectory $ do
  backend :: Backend DB FreeUH <- mkDevModeBackend (def & cfgShouldLog .~ False) mockLogin
  (natThrowError . backendRunApp backend) $$ initializeDB
  action . TestBackend backend =<< newMVar Wai.initState

-- | Create session via 'mkProdBackend' (using 'UH').
createTestSession :: ActionWith (TestBackend UH) -> IO ()
createTestSession action = withTempCurrentDirectory $ do
  void $ action =<< (TestBackend <$> mkProdBackend (def & cfgShouldLog .~ False) <*> newMVar Wai.initState)


-- * test helpers

-- (see also: https://hackage.haskell.org/package/wai-extra/docs/Network-Wai-Test.html)

sampleCreateVDoc :: CreateVDoc
sampleCreateVDoc = CreateVDoc
  (Title "[title]")
  (Abstract "[abstract]")
  (rawContentToVDocVersion . mkRawContent $ mkBlock "[versioned content]" :| [])

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

postJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> a -> Wai.Session b
postJSON path js = do
  resp <- request "POST" path [("Content-Type", "application/json")] (encode js)
  liftIO $ case eitherDecode $ simpleBody resp of
    Left err -> throwIO . ErrorCall $ unlines [cs path, show (typeOf js), show resp, show err]
    Right x  -> pure x

-- | This is from hspec-wai, but we modified it to work on 'Wai.Session' directly.  'WaiSession'
-- does not keep the cookies in the 'Session' between requests.
request :: Method -> SBS -> [Header] -> LBS -> Wai.Session SResponse
request method path headers body = Wai.srequest $ SRequest req body
  where
    req = Wai.setPath defaultRequest {requestMethod = method, requestHeaders = headers} path


addUserAndLogin :: TestBackend uh -> Username -> IO ()
addUserAndLogin sess username = runWai sess $ do
  void . post createUserUri $ CreateUser username (username <> "@email.com") "password"
  void . post loginUri $ Login username "password"

mkCVDoc :: TestBackend uh -> CreateVDoc -> IO CompositeVDoc
mkCVDoc sess vdoc = runWai sess $ postJSON createVDocUri vdoc

mkEdit :: TestBackend uh -> IO (ID Edit)
mkEdit = fmap (^. compositeVDocThisEditID) . (`mkCVDoc` sampleCreateVDoc)

mkUserAndEdit :: TestBackend uh -> IO (ID Edit)
mkUserAndEdit sess = do
  addUserAndLogin sess "username"
  mkEdit sess


-- * endpoints

uriStr :: URI -> SBS
uriStr u =  cs $ "/" <> uriToString id u ""

listVDocsUri :: SBS
listVDocsUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SListVDocs)

getVDocUri :: ID VDoc -> SBS
getVDocUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetVDoc)

createVDocUri :: SBS
createVDocUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SCreateVDoc)

addEditUri :: ID Edit -> SBS
addEditUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddEdit)

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

addProcessUri :: SBS
addProcessUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddProcess)

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

specMockedLogin :: Spec
specMockedLogin = around createDevModeTestSession $ do
  describe "sListVDocs" $ do
    it "returns a vdocs list with HTTP status 200" $ \sess -> do
      resp :: SResponse <- runWai sess $ wget listVDocsUri
      respCode resp `shouldBe` 200

    it "yields the same vdocs list as the db" $ \sess -> do
      vdocsRest :: SResponse <- runWai sess $ wget listVDocsUri
      vdocsDB   :: [VDoc]    <- runDB  sess   App.listVDocs
      decode (simpleBody vdocsRest) `shouldBe` Just vdocsDB

    it "if a vdoc is created, it shows in the output" $ \sess -> do
      bef <- runWai sess $ wget listVDocsUri
      _   <- runDB  sess $ App.createVDoc sampleCreateVDoc
      aft <- runWai sess $ wget listVDocsUri
      let test :: SResponse -> Int
          test resp = either (\msg -> error $ unwords [show msg, cs $ simpleBody resp]) length
                       $ eitherDecode @[VDoc] (simpleBody resp)
      test aft `shouldBe` (test bef + 1)

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

  describe "sAddNote" $ do
    it "stores note with full-document chunk range" $ \sess -> do
      runWai sess $ do
        un :: Username <- postJSON loginUri $ Login "username" "password"
        liftIO $ un `shouldBe` "username"
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
        un :: Username <- postJSON loginUri $ Login "username" "password"
        liftIO $ un `shouldBe` "username"
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
        un :: Username <- postJSON loginUri $ Login "username" "password"
        liftIO $ un `shouldBe` "username"
        fe_ :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc
        let cp1 = Position (BlockIndex 0 $ BlockKey "1") 0
            cp2 = Position (BlockIndex 0 $ BlockKey "1") 1
        fn_ :: CompositeDiscussion <-
          postJSON
            (addDiscussionUri (fe_ ^. compositeVDoc . vdocHeadEdit))
            (CreateDiscussion "[discussion initial statement]" True (Range cp1 cp2))

        liftIO $ do
          be :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (fe_ ^. compositeVDoc . vdocID)
          be ^. compositeVDocApplicableDiscussions . to Map.elems `shouldContain` [fn_]

  describe "sAddStatement" $ do
    it "stores statement for given discussion" $ \_sess -> do
      pendingWith "this test case shouldn't be too hard to write, and should be working already."

  describe "sAddEdit" $ do
    let samplevdoc = rawContentToVDocVersion . mkRawContent $ mkBlock "[new vdoc version]" :| []
    let setup sess = runWai sess $ do
          let group = UniversalGroup
          _l :: Username <- postJSON loginUri (Login devModeUser devModePass)
          (CreatedCollabEditProcess _fp fc) :: CreatedProcess <-
            postJSON addProcessUri
              (AddCollabEditProcess CreateCollabEditProcess
                { _createCollabEditProcessPhase = CollaborativeEditOnlyPhase
                , _createCollabEditProcessGroup = group
                , _createCollabEditProcessVDoc  = sampleCreateVDoc
                })

          userId <- liftIO . runDB sess $ do
            (Just loginId) <- userHandle $ getUserIdByName devModeUser
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
        be' :: VDocVersion <- runDB sess . db . getVersion $ fp ^. editID
        be' `shouldBe` samplevdoc

      it "stores an edit and returns it in the list of edits applicable to its base" $ \sess -> do
        pendingWith "applicableEdits is not implemented."
        (fe, fp) <- setup sess
        be :: CompositeVDoc <- runDB sess $ getCompositeVDocOnHead (fe ^. compositeVDoc . vdocID)
        be ^. compositeVDocApplicableEdits . to Map.elems`shouldContain` [fp]

specUserHandling :: Spec
specUserHandling = around createTestSession $ do
  describe "User handling" $ do
    let doCreate = post createUserUri (CreateUser userName "mail@email.com" userPass)
        doLogin = post loginUri
        doLogout = post logoutUri ()

        checkCookie resp = simpleHeaders resp `shouldSatisfy`
            any (\(k, v) -> k == "Set-Cookie" && refineCookieName `SBS.isPrefixOf` v)

        userName = "user"
        userPass = "password"

    describe "create" $ do
      it "works" $ \sess -> do

        pendingWith "#291 (this happens probabilistically, do not un-pend just because it worked a few times for you!)"

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

          pendingWith "#291 (this happens probabilistically, do not un-pend just because it worked a few times for you!)"

          resp <- runWai sess $ doCreate >> doLogin (Login userName userPass)
          respCode resp `shouldBe` 200
          checkCookie resp

          pendingWith "see fixme in runDB'"
          user <- runDB sess App.currentUser
          user `shouldSatisfy` isJust

      context "with invalid credentials" $ do
        it "works (and returns the cookie)" $ \sess -> do

          pendingWith "#291 (this happens probabilistically, do not un-pend just because it worked a few times for you!)"

          resp <- runWai sess $ doCreate >> doLogin (Login userName "")
          respCode resp `shouldBe` 404
          checkCookie resp

    describe "logout" $ do
      context "logged in" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogin (Login userName userPass) >> doLogout
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
        eid <- mkUserAndEdit sess
        _ <- runWai sess $ post logoutUri ()
        resp <- runWai sess . wput $ putVoteUri eid Yeay
        respCode resp `shouldSatisfy` (>= 400)

    context "if current user *HAS NOT* voted on the edit before" $ do
      it "adds the current user's vote (and does nothing else)" $ \sess -> do
        eid <- mkUserAndEdit sess
        resp :: SResponse <- runWai sess . wput $ putVoteUri eid Yeay
        respCode resp `shouldBe` 200
        -- votes <- runDB sess $ App.getSimpleVotesOnEdit eid  -- see FIXME at 'runDB''
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Yeay, 1)]

    context "if current user *HAS* voted on the edit before" $ do
      it "adds the current user's vote (and does nothing else)" $ \sess -> do
        eid <- mkUserAndEdit sess
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        resp :: SResponse <- runWai sess . wput $ putVoteUri eid Nay
        respCode resp `shouldBe` 200
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Nay, 1)]

  describe "SDeleteSimpleVoteOnEdit" $ do
    context "user is not logged in" $ do
      it "request is rejected" $ \sess -> do
        eid <- mkUserAndEdit sess
        _ <- runWai sess $ post logoutUri ()
        resp <- runWai sess . wdel $ deleteVoteUri eid
        respCode resp `shouldSatisfy` (>= 400)

    context "if there is such a vote" $ do
      it "removes that vote (and does nothing else)" $ \sess -> do
        eid <- mkUserAndEdit sess
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        resp :: SResponse <- runWai sess . wdel $ deleteVoteUri eid
        respCode resp `shouldBe` 200
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList []

    context "if there is no such vote" $ do
      it "does nothing" $ \sess -> do
        eid <- mkUserAndEdit sess
        resp :: SResponse <- runWai sess . wdel $ deleteVoteUri eid
        respCode resp `shouldBe` 200
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList []

  describe "SGetSimpleVotesOnEdit" $ do
    context "with two Yeays and one Nay" $ do
      it "returns (2, 1)" $ \sess -> do
        eid <- mkEdit sess
        addUserAndLogin sess "userA"
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        addUserAndLogin sess "userB"
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        addUserAndLogin sess "userC"
        _ <- runWai sess . wput $ putVoteUri eid Nay
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Yeay, 2), (Nay, 1)]

  describe "merging and rebasing" $ do
    it "### works if two edits are present and one is merged" $ \sess -> do
      addUserAndLogin sess "userA"

      let blocks = mkBlock <$> ["first line", "second line", "third line"]
          vdoc ~(b:bs) = rawContentToVDocVersion . mkRawContent $ b :| bs

      cvdoc <- mkCVDoc sess $ CreateVDoc (Title "[title]") (Abstract "[abstract]") (vdoc blocks)

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
      head rebasedEdits ^. editDesc `shouldBe` ce2 ^. createEditDesc
      head rebasedEdits ^. editKind `shouldBe` ce2 ^. createEditKind
      -- (compare versions, too?  that will probably break once we get fancier merge heuristics, though.)
