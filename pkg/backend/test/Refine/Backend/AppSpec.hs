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

module Refine.Backend.AppSpec where

import Refine.Backend.Prelude hiding (assert, check)

import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NEL
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Refine.Backend.App as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Common.Test.Arbitrary ()
import Refine.Common.Test.Samples (sampleVDocVersion)
import Refine.Common.Types
import Refine.Common.VDoc.Draft


data Cmd where
  AddVDoc :: Create VDoc        -> Cmd
  GetVDoc :: Int -> Create VDoc -> Cmd
  AddEditToHead :: Int -> Create Edit -> Cmd

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
    it "Random program" $ \(runner :: AppM DB Property -> IO Property) -> forAll sampleProgram $ \program ->
      monadic (monadicApp runner) (runProgram program `evalStateT` initVDocs)

  describe "User handling" . around provideAppRunner $ do
    -- FUTUREWORK: Use the Cmd dsl for this test
    it "Create/login/logout" $ \(runner :: AppM DB () -> IO ()) -> do

      pendingWith "FIXME: #291"

      runner $ do
        void $ App.createUser (CreateUser "user" "user@example.com" "password")
        userState0 <- gets (view appUserState)
        appIO $ userState0 `shouldBe` UserLoggedOut

      runner $ do
        void $ App.login (Login "user" "password")
        userState1 <- gets (view appUserState)
        appIO $ userState1 `shouldSatisfy` isActiveUser

        void App.logout
        userState2 <- gets (view appUserState)
        appIO $ userState2 `shouldBe` UserLoggedOut

  describe "Database handling" . around provideAppRunner $ do
    it "db (or dbWithFilters) can be called twice inside the same AppM" $ \(runner :: AppM DB () -> IO ()) -> do
      runner $ do
        void $ do
          let createGroup1 = CreateGroup "group1" "desc1" [] [] False
              createGroup2 = CreateGroup "group2" "desc2" [] [] False
              sameGroupInfo cgrp grp = and
                [ cgrp ^. createGroupTitle     == grp ^. groupTitle
                , cgrp ^. createGroupDesc      == grp ^. groupDesc
                , cgrp ^. createGroupParents   == grp ^. groupParents
                , cgrp ^. createGroupChildren  == grp ^. groupChildren
                , cgrp ^. createGroupUniversal == grp ^. groupUniversal
                ]
          grp1 <- App.addGroup createGroup1
          grp2 <- App.addGroup createGroup2

          appIO $ grp1 `shouldSatisfy` sameGroupInfo createGroup1
          appIO $ grp2 `shouldSatisfy` sameGroupInfo createGroup2

  describe "Regression" . around provideAppRunner $ do
    it "Regression test program" $ \(runner :: AppM DB () -> IO ()) -> do
      let program =
            [ AddVDoc (CreateVDoc (Title "title...") (Abstract "abstract...") sampleVDocVersion)
            , AddEditToHead 0 sampleCreateEdit1
            ]
      runner . runIdentityT $ runProgram program `evalStateT` initVDocs

  describe "merging" . around provideAppRunner $ do
    let vdoc = rawContentToVDocVersion . mkRawContent . NEL.fromList . map mkBlock

        docWithEdits v0 vs = do
          doc <- App.createVDoc . CreateVDoc (Title "title...") (Abstract "abstract...") $ vdoc v0
          let eid = doc ^. vdocHeadEdit
              vid = doc ^. vdocID
          es <- forM vs $ \v -> fmap (^. editID) . App.addEdit eid $ CreateEdit "..." (vdoc v) Meaning
          pure (vid, eid, es)

        addUserAndLogin username = do
          _ <- App.createUser $ CreateUser username (username <> "@email.com") "password"
          login $ Login username "password"

    it "merge two edits" $ \(runner :: AppM DB () -> IO ()) -> do
      runner $ do
        (_, base, [eid1, eid2]) <- docWithEdits ["abc", "def"] [["a.c", "def"], ["abc", "d.f"]]
        eidm <- (^. editID) <$> App.addMerge base eid1 eid2
        doc' <- App.getVDocVersion eidm
        appIO $ doc' `shouldBe` vdoc ["a.c","d.f"]

    it "rebase one edit to two other edits" $ \(runner :: AppM DB () -> IO ()) -> do
      runner $ do
        (vid, _, [eid1, _, _]) <- docWithEdits ["abc", "def"] [["a.c", "def"], ["abc", "d.f"], ["abX", "def"]]
        App.rebaseHeadToEdit eid1
        d <- App.getVDoc vid
        let hid = d ^. vdocHeadEdit
        appIO $ hid `shouldBe` eid1
        cv <- App.getCompositeVDoc vid hid
        appIO $ cv ^. compositeVDocThisVersion `shouldBe` vdoc ["a.c", "def"]
        appIO $ Map.size (cv ^. compositeVDocApplicableEdits) `shouldBe` 2
        let [ee0, ee1] = Map.keys $ cv ^. compositeVDocApplicableEdits
        docA <- App.getVDocVersion ee0
        appIO $ docA `shouldBe` vdoc ["a.c","d.f"]
        docB <- App.getVDocVersion ee1
        appIO $ docB `shouldBe` vdoc ["aX.","def"]   -- FIXME: the merge result is strange

    it "upvoting an edit triggers rebase" $ \(runner :: AppM DB () -> IO ()) -> runner $ do

        appIO $ pendingWith "#291 (probably)"

        (vid, _, [eid]) <- docWithEdits ["abc", "def"] [["a.c", "def"]]
        void $ addUserAndLogin "user"
        putSimpleVoteOnEdit eid Yeay
        d <- App.getVDoc vid
        let hid = d ^. vdocHeadEdit
        appIO $ hid `shouldBe` eid


-- * Program Runner interface

class MonadTrans pr => ProgramRunner pr where
  check :: Monad m => Bool -> pr m ()

instance ProgramRunner PropertyM where
  check = assert

instance ProgramRunner IdentityT where
  check x = unless x $ error "Assertion has failed."

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
  lift . check $
    (vdoc ^. vdocTitle    == cv ^. createVDocTitle) &&
    (vdoc ^. vdocAbstract == cv ^. createVDocAbstract)

runCmd (GetVDoc v cv) = do
  Just vid <- gets . view $ vdocMap . at v
  vdoc <- lift . lift $ App.getVDoc vid
  lift . check $
    (vdoc ^. vdocTitle    == cv ^. createVDocTitle) &&
    (vdoc ^. vdocAbstract == cv ^. createVDocAbstract)

runCmd (AddEditToHead v cedit) = do
  Just vid <- gets . view $ vdocMap . at v
  cvdoc <- lift . lift $ App.getCompositeVDocOnHead vid
  let eid = cvdoc ^. compositeVDocThisEditID
  edit   :: Edit          <- lift . lift $ App.addEdit eid cedit
  cvdoc' :: CompositeVDoc <- lift . lift $ App.getCompositeVDocOnHead vid
  let edit' = cvdoc' ^?! compositeVDocApplicableEdits . at (edit ^. editID) . _Just
  lift . check $
    (edit                 == edit') &&
    (edit ^. editDesc     == cedit ^. createEditDesc) &&
    (edit ^. editKind     == cedit ^. createEditKind)


-- * generators

word :: (ConvertibleStrings String s) => Gen s
word = cs <$> listOf (elements ['a' .. 'z'])

arbitraryCreateVDoc :: Gen (Create VDoc)
arbitraryCreateVDoc =
  CreateVDoc
    <$> (Title <$> word)
    <*> (Abstract . mconcat <$> listOf word)
    <*> (rawContentToVDocVersion <$> arbitrary @RawContent)

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- arbitraryCreateVDoc
    pure [AddVDoc v, GetVDoc i v]


-- * loud samples

sampleCreateEdit1 :: CreateEdit
sampleCreateEdit1 = CreateEdit {_createEditDesc = "...", _createEditVDocVersion = sampleVDocVersion, _createEditKind = Grammar}
