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

import           Control.Category ((.))
import           Control.Lens ((^.), (%=), at, to, makeLenses, view)
import           Control.Monad.State
import           Control.Natural (($$))
import           Data.Default (def)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid (mconcat)
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Prelude hiding ((.))
import           System.Directory
                    ( createDirectoryIfMissing
                    , removeDirectoryRecursive
                    , removeFile
                    )
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Refine.Backend.App         as App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Config
import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Backend.Test.Util (withTempCurrentDirectory)
import Refine.Backend.Types
import Refine.Common.Types.Prelude
import Refine.Common.Types.User
import Refine.Common.Types.VDoc
import Refine.Common.Test.Arbitrary (arbitraryRawVDocVersion)


data Cmd where
  AddVDoc :: Create VDoc        -> Cmd
  GetVDoc :: Int -> Create VDoc -> Cmd

deriving instance Show Cmd

data VDocs = VDocs
  { _vdocMap  :: !(Map Int (ID VDoc))
  , _vdocLast :: !Int
  }

initVDocs :: VDocs
initVDocs = VDocs Map.empty 0

makeLenses ''VDocs

isActiveUser :: AppUserState -> Bool
isActiveUser (UserLoggedIn _) = True
isActiveUser UserLoggedOut    = False

-- Parallel run is not an option here, it could make fail the build at the cleanup stage.
spec :: Spec
spec = do
  describe "VDoc" . around provideAppRunner $ do
    it "Random program" $ \(runner :: App DB Property -> IO Property) -> forAll sampleProgram $ \program ->
      monadic (monadicApp runner) (runProgram program `evalStateT` initVDocs)

  describe "User handling" . around provideAppRunner $ do
    it "Create/login/logout" $ \(runner :: App DB () -> IO ()) -> do
      -- NOTE: Pattern match on the result will trigger the evaluation
      -- of the term under test.
      () <- runner $ do

        void $ App.createUser (CreateUser "user" "user@example.com" "password")
        userState0 <- gets (view appUserState)
        appIO $ userState0 `shouldBe` UserLoggedOut

        void $ App.login (Login "user" "password")
        userState1 <- gets (view appUserState)
        appIO $ userState1 `shouldSatisfy` isActiveUser

        void $ App.logout Logout
        userState2 <- gets (view appUserState)
        appIO $ userState2 `shouldBe` UserLoggedOut

      pure ()

-- * Helpers

provideAppRunner :: ActionWith (App DB a -> IO a) -> IO ()
provideAppRunner action = withTempCurrentDirectory $ do
  (runner, testDb, reposRoot) <- createAppRunner
  action runner
  removeFile testDb
  removeDirectoryRecursive reposRoot

createAppRunner :: forall a . IO (App DB a -> IO a, FilePath, FilePath)
createAppRunner = do
  let testDb    = "test.db"
      reposRoot = "./repos"

      cfg = Config
        { _cfgShouldMigrate = False  -- (this is ignored here)
        , _cfgShouldLog     = False  -- (this is ignored here)
        , _cfgReposRoot     = reposRoot
        , _cfgDBKind        = DBOnDisk testDb
        , _cfgPoolSize      = 5
        , _cfgFileServeRoot = Nothing
        , _cfgWarpSettings  = def
        , _cfgCsrfSecret    = "CSRF-SECRET"
        }

  createDirectoryIfMissing True $ cfg ^. cfgReposRoot
  (runDb, userHandler) <- createDBRunner cfg
  runDRepo <- createRunRepo cfg
  let logger = Logger . const $ pure ()
      runner :: forall b . App DB b -> IO b
      runner m = (natThrowError . runApp runDb runDRepo logger userHandler (cfg ^. cfgCsrfSecret . to CsrfSecret)) $$ m

  void $ runner migrateDB
  pure (runner, testDb, reposRoot)

monadicApp :: (App DB Property -> IO Property) -> App DB Property -> Property
monadicApp p = ioProperty . p


-- * monadic property

runProgram :: [Cmd] -> StateT VDocs (PropertyM (App DB)) ()
runProgram = foldl (>>) (pure ()) . map runCmd

runCmd :: Cmd -> StateT VDocs (PropertyM (App DB)) ()
runCmd (AddVDoc cv) = do
  vdoc   <- lift . run $ App.createVDoc cv
  lastId <- gets $ view vdocLast
  vdocMap  %= Map.insert lastId (vdoc ^. vdocID)
  vdocLast %= (+1)
  lift . assert $
    (vdoc ^. vdocTitle    == cv ^. createVDocTitle) &&
    (vdoc ^. vdocAbstract == cv ^. createVDocAbstract)

runCmd (GetVDoc v cv) = do
  Just vid <- gets . view $ vdocMap . at v
  vdoc <- lift . run $ App.getVDoc vid
  lift . assert $
    (vdoc ^. vdocTitle    == cv ^. createVDocTitle) &&
    (vdoc ^. vdocAbstract == cv ^. createVDocAbstract)


-- * generators

word :: (ConvertibleStrings String s) => Gen s
word = cs <$> listOf (elements ['a' .. 'z'])

arbitraryCreateVDoc :: Gen (Create VDoc)
arbitraryCreateVDoc =
  CreateVDoc
    <$> (Title <$> word)
    <*> (Abstract . mconcat <$> listOf word)
    <*> arbitraryRawVDocVersion

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- arbitraryCreateVDoc
    pure [AddVDoc v, GetVDoc i v]
