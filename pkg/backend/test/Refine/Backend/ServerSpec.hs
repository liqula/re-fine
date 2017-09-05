{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.ServerSpec where
#include "import_backend.hs"

import qualified Data.ByteString as SBS
import           Network.Wai.Test (SResponse(..))
import           Test.Hspec
import qualified Web.Users.Types as Users

import           Refine.Backend.App hiding (getEdit)
import qualified Refine.Backend.App as App
import           Refine.Backend.Database.Class as DB hiding (getVDoc)
import           Refine.Backend.Database.Entity (toUserID)
import           Refine.Backend.Server
import           Refine.Backend.Test.AppServer
import           Refine.Backend.Test.Util (sampleMetaID)
import           Refine.Common.OT hiding (Edit)
import           Refine.Common.ChangeAPI
import           Refine.Common.Types as Common

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


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
      vdoc <- runDB sess $ App.createVDoc sampleCreateVDoc0
      resp <- runWai sess . wget $ getVDocUri (vdoc ^. vdocID)
      respCode resp `shouldBe` 200

  describe "sCreateVDoc" $ do
    it "stores a vdoc in the db" $ \sess -> do
      fe :: VDoc <- runWai sess $ postJSON createVDocUri sampleCreateVDoc0
      be :: VDoc <- runDB  sess $ getVDoc (fe ^. vdocID)
      fe `shouldBe` be

  describe "sUpdateVDoc" $ do
    it "stores new title, abstract in vdoc in the db" $ \sess -> do
      bef :: VDoc <- runWai sess $ postJSON createVDocUri sampleCreateVDoc0
      let vid = bef ^. vdocID
          newabstract = Abstract "newabs"
          newtitle = Title "newtitle"

      after1 :: VDoc <- runWai sess $ putJSON (updateVDocUri vid) (UpdateVDoc newtitle newabstract)
      after2  :: VDoc <- runDB  sess $ getVDoc vid

      (after1 ^. vdocTitle)                    `shouldBe` newtitle
      (after1 ^. vdocAbstract)                 `shouldBe` newabstract
      (after2 ^. vdocTitle)                    `shouldBe` newtitle
      (after2 ^. vdocAbstract)                 `shouldBe` newabstract

  describe "sAddNote" $ do

    it "stores note with full-document chunk range" $ \sess -> do
      runWai sess $ do
        un :: User <- postJSON loginUri $ Login testUsername testPassword
        liftIO $ (un ^. userName) `shouldBe` testUsername
        fe_ :: VDoc <- postJSON createVDocUri sampleCreateVDoc0
        let cp1 = Position (BlockIndex 0 $ BlockKey "0") 0
            cp2 = Position (BlockIndex 0 $ BlockKey "0") 1
        fn_ :: Discussion          <- postJSON
            (addDiscussionUri (fe_ ^. vdocHeadEdit))
            (CreateDiscussion "[note]" (Just $ Range cp1 cp2) True :: CreateDiscussion (Maybe (Range Position)))
        liftIO $ do
          be :: Edit <- runDB sess $ App.getEdit (fe_ ^. vdocHeadEdit)
          be ^. editDiscussions' . to Map.keys `shouldContain` [fn_ ^. discussionID]

    it "stores note with non-trivial valid chunk range" $ \sess -> do
      runWai sess $ do
        un :: User <- postJSON loginUri $ Login testUsername testPassword
        liftIO $ (un ^. userName) `shouldBe` testUsername
        fe_ :: VDoc <- postJSON createVDocUri sampleCreateVDoc0
        let cp1 = Position (BlockIndex 1 $ BlockKey "1") 0
            cp2 = Position (BlockIndex 1 $ BlockKey "1") 1
        fn_ :: Discussion <- postJSON
          (addDiscussionUri (fe_ ^. vdocHeadEdit))
          (CreateDiscussion "[note]" (Just $ Range cp1 cp2) True :: CreateDiscussion (Maybe (Range Position)))

        liftIO $ do
          be :: Edit <- runDB sess $ App.getEdit (fe_ ^. vdocHeadEdit)
          be ^. editDiscussions' . to Map.keys `shouldContain` [fn_ ^. discussionID]

    it "fails with error on non-trivial *invalid* chunk range" $ \sess -> do
      vdoc :: VDoc <- runWai sess $ postJSON createVDocUri sampleCreateVDoc0
      resp :: SResponse <- runWai sess $
        let cp1, cp2 :: Position
            cp1 = Position (BlockIndex 1 $ BlockKey "1") 0
            cp2 = Position (BlockIndex 100 $ BlockKey "100") 100
        in post
          (addDiscussionUri (vdoc ^. vdocHeadEdit))
          (CreateDiscussion "[note]" (Just $ Range cp1 cp2) True :: CreateDiscussion (Maybe (Range Position)))

      pendingWith "'validateCreateChunkRange' is not implemented yet."

      respCode resp `shouldBe` 409
      cs (simpleBody resp) `shouldContain` ("ChunkRangeBadDataUID" :: String)

      vdoc' :: Edit <- runDB sess $ App.getEdit (vdoc ^. vdocHeadEdit)
      vdoc' ^. editDiscussions' `shouldBe` mempty

  describe "sAddDiscussion" $ do
    it "stores discussion with no ranges" $ \sess -> do
      runWai sess $ do
        un :: User <- postJSON loginUri $ Login testUsername testPassword
        liftIO $ (un ^. userName) `shouldBe` testUsername
        fe_ :: VDoc <- postJSON createVDocUri sampleCreateVDoc0
        let cp1 = Position (BlockIndex 0 $ BlockKey "1") (0 :: Int)
            cp2 = Position (BlockIndex 0 $ BlockKey "1") 1
        fn_ :: Discussion <-
          postJSON
            (addDiscussionUri (fe_ ^. vdocHeadEdit))
            (CreateDiscussion "[discussion initial statement]" (Just (Range cp1 cp2)) False)

        liftIO $ do
          be :: Edit <- runDB sess $ App.getEdit (fe_ ^. vdocHeadEdit)
          be ^. editDiscussions' . to Map.keys `shouldContain` [fn_ ^. discussionID]

  describe "sAddStatement" $ do
    it "stores statement for given discussion" $ \_sess -> do
      pendingWith "this test case shouldn't be too hard to write, and should be working already."

  describe "sAddEdit, sUpdateEdit" $ do
    let samplevdoc = mkRawContent $ mkBlock "[new vdoc version]" :| []
    let setup sess = do
         group <- fmap (^. groupID) . runDB sess $ App.addGroup (CreateGroup "title" "desc" [] [] mempty)
         runWai sess $ do
          _l :: User <- postJSON loginUri (Login testUsername testPassword)
          fc :: VDoc <- postJSON createVDocUri sampleCreateVDoc0

          userId <- liftIO . runDB sess $ do
            (Just loginId) <- dbUsersCmd $ \db_ -> Users.getUserIdByName db_ testUsername
            pure (toUserID loginId)

          () <- postJSON changeRoleUri AssignGroupRole
                  { _crGroupRef  = group
                  , _crUser      = userId
                  , _crGroupRole = GroupMember
                  }

          fe :: Edit <-
            postJSON
              (addEditUri (fc ^. vdocHeadEdit))
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
        be :: Edit <- runDB sess $ App.getEdit (fe ^. vdocHeadEdit)
        be ^. editChildren . to Set.elems `shouldContain` [fp ^. editID]

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
    let doCreate = post createUserUri (CreateUser testUsername testUserEmail testPassword Nothing)
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
        _ <- addUserAndLogin sess "userA" "userA@email.com" testPassword
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        _ <- addUserAndLogin sess "userB" "userB@email.com" testPassword
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        _ <- addUserAndLogin sess "userC" "userC@email.com" testPassword
        _ <- runWai sess . wput $ putVoteUri eid Nay
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Yeay, 2), (Nay, 1)]

    context "with two Yeays and one Nay, and after changing one Yeay into a Nay" $ do
      it "returns (1, 2)" $ \sess -> do
        eid <- mkEdit sess
        _ <- addUserAndLogin sess "userA" "userA@email.com" testPassword
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        _ <- addUserAndLogin sess "userB" "userB@email.com" testPassword
        _ <- runWai sess . wput $ putVoteUri eid Yeay
        _ <- runWai sess . wput $ putVoteUri eid Nay
        _ <- addUserAndLogin sess "userC" "userC@email.com" testPassword
        _ <- runWai sess . wput $ putVoteUri eid Nay
        votes :: VoteCount <- runWaiJSON sess . wget $ getVotesUri eid
        votes `shouldBe` Map.fromList [(Yeay, 1), (Nay, 2)]

  describe "merging and rebasing" $ do
    it "works if two edits are present and one is merged" $ \sess -> do
      _ <- addUserAndLogin sess "userA" "userA@email.com" testPassword

      let blocks = mkBlock <$> ["first line", "second line", "third line"]
          vdoc ~(b:bs) = mkRawContent $ b :| bs

      cvdoc <- mkCVDoc sess $ CreateVDoc (Title "[title]") (Abstract "[abstract]") (vdoc blocks) defaultGroupID

      let ce1 :: CreateEdit = CreateEdit "description" (vdoc [head blocks, blocks !! 1]) Grammar
          ce2 :: CreateEdit = CreateEdit "description" (vdoc [head blocks, blocks !! 2]) Grammar

      (e1,  e2) <- runWai sess $ do
        [e1_, e2_] <- postJSON (addEditUri (cvdoc ^. vdocHeadEdit)) `mapM` [ce1, ce2]
        pure (e1_, e2_)

      resp <- runWai sess . wput $ putVoteUri (e1 ^. editID) Yeay
      respCode resp `shouldSatisfy` (< 400)

      -- composite vdoc should point to e1
      cvdoc' :: VDoc <- runDB sess $ getVDoc (cvdoc ^. vdocID)
      ce :: Edit <- runDB sess $ App.getEdit (cvdoc' ^. vdocHeadEdit)
      ce ^. editID `shouldBe` e1 ^. editID

      -- e2 should be re-based onto e1
      let rebasedEdits = Set.elems (ce ^. editChildren)
      length rebasedEdits `shouldBe` 1
      head rebasedEdits   `shouldNotBe` e2 ^. editID  -- (rebase is immutable)
      --head rebasedEdits ^. editDesc `shouldBe` "description"
      --head rebasedEdits ^. editKind `shouldBe` Grammar
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
          <- mkEdit sess  -- saves sampleCreateVDoc0
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
    let cp1 = Position (BlockIndex 0 $ BlockKey "1") (0 :: Int)
        cp2 = Position (BlockIndex 0 $ BlockKey "1") 1
    _firstDiscussion :: Discussion
      <- runWai sess $ postJSON (addDiscussionUri base)
            (CreateDiscussion "[discussion initial statement]" (Just (Range cp1 cp2)) False)
    pure ()
