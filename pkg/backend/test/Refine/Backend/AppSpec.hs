{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.AppSpec where
#include "import_backend.hs"

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic as QuickCheck

import Refine.Backend.App as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Common.Test
import Refine.Common.Types


data Cmd where
  AddVDoc :: CreateVDoc        -> Cmd
  GetVDoc :: Int -> CreateVDoc -> Cmd
  AddEditToHead :: Int -> CreateEdit -> Cmd

deriving instance Show Cmd

data VDocs = VDocs
  { _vdocMap  :: !(Map Int (ID VDoc))
  , _vdocLast :: !Int
  }

initVDocs :: VDocs
initVDocs = VDocs Map.empty 0

makeLenses ''VDocs

isActiveUser :: AppUserState -> Bool
isActiveUser (UserLoggedIn _ _) = True
isActiveUser UserLoggedOut      = False


-- Parallel run is not an option here, it could make the build fail at the cleanup stage.
spec :: Spec
spec = do
  describe "VDoc" . around provideAppRunner $ do
    it "Random program" $ \(runner :: AppM DB Property -> ExceptT ApiError IO Property) -> forAll sampleProgram $ \program ->
      monadic (monadicApp (throwApiErrors . runner)) (runProgram program `evalStateT` initVDocs)

  describe "User handling" . around provideAppRunner $ do
    -- FUTUREWORK: Use the Cmd dsl for this test
    it "Create/login/logout" $ \(runner :: AppM DB () -> ExceptT ApiError IO ()) -> do
      throwApiErrors . runner $ do
        void $ App.createUser (CreateUser "user" "user@example.com" "password" Nothing "")
        userState0 <- gets (view appUserState)
        liftIO $ userState0 `shouldBe` UserLoggedOut

      throwApiErrors . runner $ do
        void $ App.login (Login "user" "password")
        userState1 <- gets (view appUserState)
        liftIO $ userState1 `shouldSatisfy` isActiveUser

        void App.logout
        userState2 <- gets (view appUserState)
        liftIO $ userState2 `shouldBe` UserLoggedOut

  describe "Database handling" . around provideAppRunner $ do
    it "one app is one transaction (and rolls back on AppError)." $ \(runner :: AppM DB () -> ExceptT ApiError IO ()) -> do
      mem :: MVar (ID Group) <- newEmptyMVar
      let nosuchgid = ID 834791
          transaction1 = do
            liftIO . putMVar mem . view groupID =<< addGroup (CreateGroup mempty mempty mempty mempty mempty Nothing)
            Group{} <- App.getGroup nosuchgid
            pure ()
          transaction2 gid = do
            Group{} <- App.getGroup gid
            pure ()
          groupException gid = Left . ApiDBError . ApiDBNotFound $ "not found: " <> show gid <> " :: ID Group"

      runExceptT (runner transaction1) `shouldReturn` groupException nosuchgid
      gid <- takeMVar mem
      runExceptT (runner (transaction2 gid)) `shouldReturn` groupException gid

    it "db (or dbWithFilters) can be called twice inside the same AppM" $ \(runner :: AppM DB () -> ExceptT ApiError IO ()) -> do
      throwApiErrors . runner $ do
        void $ do
          let createGroup1 = CreateGroup "group1" "desc1" [] [] mempty Nothing
              createGroup2 = CreateGroup "group2" "desc2" [] [] mempty Nothing
              sameGroupInfo cgrp grp = and
                [ cgrp ^. createGroupTitle     == grp ^. groupTitle
                , cgrp ^. createGroupDesc      == grp ^. groupDesc
                , cgrp ^. createGroupParents   == grp ^. groupParents
                , cgrp ^. createGroupChildren  == grp ^. groupChildren
                ]
          grp1 <- App.addGroup createGroup1
          grp2 <- App.addGroup createGroup2

          liftIO $ grp1 `shouldSatisfy` sameGroupInfo createGroup1
          liftIO $ grp2 `shouldSatisfy` sameGroupInfo createGroup2

  describe "Regression" . around provideAppRunner $ do
    it "Regression test program" $ \(runner :: AppM DB () -> ExceptT ApiError IO ()) -> do
      let program =
            [ AddVDoc (CreateVDoc (Title "title...") (Abstract "abstract...") sampleRawContent1 defaultGroupID Nothing)
            , AddEditToHead 0 sampleCreateEdit1
            ]
      throwApiErrors . runner . runIdentityT $ runProgram program `evalStateT` initVDocs

  describe "merging" . around provideAppRunner $ do
    let vdoc = mkRawContent . NEL.fromList . map mkBlock

        docWithEdits v0 vs = do
          doc <- App.createVDoc $ CreateVDoc (Title "title...") (Abstract "abstract...") (vdoc v0) defaultGroupID Nothing
          let eid = doc ^. vdocHeadEdit
              vid = doc ^. vdocID
          es <- forM vs $ \v -> fmap (^. editID) . App.addEdit eid $ CreateEdit "..." (vdoc v) Meaning
          pure (vid, eid, es)

        addUserAndLogin username = do
          _ <- App.createUser $ CreateUser username (username <> "@email.com") "password" Nothing ""
          login $ Login username "password"

    it "merge two edits" $ \(runner :: AppM DB () -> ExceptT ApiError IO ()) -> do
      throwApiErrors . runner $ do
        (_, base, [eid1, eid2]) <- docWithEdits ["abc", "def"] [["a.c", "def"], ["abc", "d.f"]]
        eidm <- (^. editID) <$> App.addMerge base eid1 eid2
        doc' <- App.getVDocVersion eidm
        liftIO $ doc' `shouldBe` vdoc ["a.c","d.f"]

    it "rebase one edit to two other edits" $ \(runner :: AppM DB () -> ExceptT ApiError IO ()) -> do
      throwApiErrors . runner $ do
        (vid, _, [eid1, _, _]) <- docWithEdits ["abc", "def"] [["a.c", "def"], ["abc", "d.f"], ["abX", "def"]]
        App.mergeEditAndRebaseAllSiblings eid1
        d <- App.getVDoc vid
        let hid = d ^. vdocHeadEdit
        liftIO $ hid `shouldBe` eid1
        cv <- App.getEdit hid
        liftIO $ cv ^. editVDocVersion `shouldBe` vdoc ["a.c", "def"]
        liftIO $ Set.size (cv ^. editChildren) `shouldBe` 2
        let [ee0, ee1] = Set.toList $ cv ^. editChildren
        docA <- App.getVDocVersion ee0
        liftIO $ docA `shouldBe` vdoc ["a.c","d.f"]
        docB <- App.getVDocVersion ee1
        liftIO $ docB `shouldBe` vdoc ["aX.","def"]   -- FIXME: the merge result is strange

    it "upvoting an edit triggers rebase" $ \(runner :: AppM DB () -> ExceptT ApiError IO ()) ->
      throwApiErrors . runner $ do
        (vid, _, [eid]) <- docWithEdits ["abc", "def"] [["a.c", "def"]]
        void $ addUserAndLogin "user"
        _ <- toggleSimpleVoteOnEdit eid Yeay
        d <- App.getVDoc vid
        let hid = d ^. vdocHeadEdit
        liftIO $ hid `shouldBe` eid


-- * Program Runner interface

class MonadTrans pr => ProgramRunner pr where
  invariant :: Monad m => Bool -> pr m ()

instance ProgramRunner PropertyM where
  invariant = QuickCheck.assert

instance ProgramRunner IdentityT where
  invariant x = unless x $ error "instance ProgramRunner IdentityT: Assertion failed."


-- * monadic property

runProgram
  :: (ProgramRunner m, Monad (m (AppM DB)))
  => [Cmd] -> StateT VDocs (m (AppM DB)) ()
runProgram = foldl (>>) (pure ()) . map runCmd

runCmd
  :: (ProgramRunner m, Monad (m (AppM DB)))
  => Cmd -> StateT VDocs (m (AppM DB)) ()
runCmd (AddVDoc cv) = do
  vdoc   <- lift . lift $ App.createVDoc cv
  lastId <- gets $ view vdocLast
  vdocMap  %= Map.insert lastId (vdoc ^. vdocID)
  vdocLast %= (+1)
  lift . invariant $
    (vdoc ^. vdocTitle    == cv ^. createVDocTitle) &&
    (vdoc ^. vdocAbstract == cv ^. createVDocAbstract)

runCmd (GetVDoc v cv) = do
  Just vid <- gets . view $ vdocMap . at v
  vdoc <- lift . lift $ App.getVDoc vid
  lift . invariant $
    (vdoc ^. vdocTitle    == cv ^. createVDocTitle) &&
    (vdoc ^. vdocAbstract == cv ^. createVDocAbstract)

runCmd (AddEditToHead v cedit) = do
  Just vid <- gets . view $ vdocMap . at v
  cvdoc <- lift . lift $ App.getVDoc vid
  let eid = cvdoc ^. vdocHeadEdit
  edit   :: Edit <- lift . lift $ App.addEdit eid cedit
  cvdoc' :: VDoc <- lift . lift $ App.getVDoc vid
  edit_  :: Edit <- lift . lift $ App.getEdit (cvdoc' ^. vdocHeadEdit)
  lift . invariant $ Set.member (edit ^. editID) (edit_ ^. editChildren)
  edit'  :: Edit <- lift . lift $ App.getEdit (edit ^. editID)
  lift . invariant $
    (edit                 == edit') &&
    (edit ^. editDesc     == cedit ^. createEditDesc) &&
    (edit ^. editKind     == cedit ^. createEditKind)


-- * generators

word :: (ConvertibleStrings String s) => Gen s
word = cs <$> listOf (elements ['a' .. 'z'])

arbitraryCreateVDoc :: Gen CreateVDoc
arbitraryCreateVDoc =
  CreateVDoc
    <$> (Title <$> word)
    <*> (Abstract . mconcat <$> listOf word)
    <*> arbitrary @RawContent
    <*> pure defaultGroupID
    <*> pure Nothing

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- arbitraryCreateVDoc
    pure [AddVDoc v, GetVDoc i v]


-- * loud samples

sampleCreateEdit1 :: CreateEdit
sampleCreateEdit1 = CreateEdit {_createEditDesc = "...", _createEditVDocVersion = sampleRawContent1, _createEditKind = Grammar}
