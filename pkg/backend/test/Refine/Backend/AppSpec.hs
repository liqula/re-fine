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
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Refine.Backend.App as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Backend.Test.Util (forceEval)
import Refine.Backend.User
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

-- Parallel run is not an option here, it could make fail the build at the cleanup stage.
spec :: Spec
spec = do
  describe "VDoc" . around provideAppRunner $ do
    it "Random program" $ \(runner :: AppM DB UH Property -> IO Property) -> forAll sampleProgram $ \program ->
      monadic (monadicApp runner) (runProgram program `evalStateT` initVDocs)

  describe "User handling" . around provideAppRunner $ do
    -- FUTUREWORK: Use the Cmd dsl for this test
    it "Create/login/logout" $ \(runner :: AppM DB UH () -> IO ()) -> do

      pendingWith "TODO: #291"

      forceEval . runner $ do
        void $ App.createUser (CreateUser "user" "user@example.com" "password")
        userState0 <- gets (view appUserState)
        appIO $ userState0 `shouldBe` UserLoggedOut

      forceEval . runner $ do
        void $ App.login (Login "user" "password")
        userState1 <- gets (view appUserState)
        appIO $ userState1 `shouldSatisfy` isActiveUser

        void App.logout
        userState2 <- gets (view appUserState)
        appIO $ userState2 `shouldBe` UserLoggedOut

  describe "Database handling" . around provideAppRunner $ do
    it "db (or dbWithFilters) can be called twice inside the same AppM" $ \(runner :: AppM DB UH () -> IO ()) -> do
      forceEval . runner $ do
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
    it "Regression test program" $ \(runner :: AppM DB UH () -> IO ()) -> do
      let program =
            [ AddVDoc (CreateVDoc (Title "title...") (Abstract "abstract...") sampleVDocVersion)
            , AddEditToHead 0 sampleCreateEdit1
            ]
      runner . runIdentityT $ runProgram program `evalStateT` initVDocs

-- * Program Runner interface

class MonadTrans pr => ProgramRunner pr where
  check :: Monad m => Bool -> pr m ()

instance ProgramRunner PropertyM where
  check = assert

instance ProgramRunner IdentityT where
  check x = unless x $ error "Assertion has failed."

-- * monadic property

runProgram
  :: (ProgramRunner m, Monad (m (AppM DB UH)))
  => [Cmd] -> StateT VDocs (m (AppM DB UH)) ()
runProgram = foldl (>>) (pure ()) . map runCmd

runCmd
  :: (ProgramRunner m, Monad (m (AppM DB UH)))
  => Cmd -> StateT VDocs (m (AppM DB UH)) ()
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
  cvdoc <- lift . lift $ App.getCompositeVDoc vid
  let eid = cvdoc ^. compositeVDocEditID
  edit   :: Edit          <- lift . lift $ App.addEdit eid cedit
  cvdoc' :: CompositeVDoc <- lift . lift $ App.getCompositeVDoc vid
  let edit' = cvdoc' ^?! compositeVDocEdits . at (edit ^. editID) . _Just
  lift . check $
    (edit                 == edit') &&
    (edit ^. editDesc     == cedit ^. createEditDesc) &&
    (edit ^. editRange    == cedit ^. createEditRange) &&
    (edit ^. editKind     == cedit ^. createEditKind) &&
    (edit ^. editMotiv    == cedit ^. createEditMotiv)


-- * generators

word :: (ConvertibleStrings String s) => Gen s
word = cs <$> listOf (elements ['a' .. 'z'])

arbitraryCreateVDoc :: Gen (Create VDoc)
arbitraryCreateVDoc =
  CreateVDoc
    <$> (Title <$> word)
    <*> (Abstract . mconcat <$> listOf word)
    <*> (VDocVersion . cs . show <$> arbitrary @RawContent)

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- arbitraryCreateVDoc
    pure [AddVDoc v, GetVDoc i v]


-- * loud samples

sampleCreateEdit1 :: CreateEdit
sampleCreateEdit1 = CreateEdit {_createEditDesc = "...", _createEditRange = ChunkRange {_chunkRangeBegin = Nothing, _chunkRangeEnd = Nothing}, _createEditVDoc = sampleVDocVersion, _createEditKind = Grammar, _createEditMotiv = "..."}
