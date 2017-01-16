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
import           Control.Lens ((^.), (%=), at, makeLenses, view)
import           Control.Monad.State
import           Control.Natural (($$))
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid (mconcat)
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Prelude hiding ((.))
import           System.IO.Temp (withSystemTempDirectory)
import           System.Directory
                    ( createDirectory
                    , removeDirectoryRecursive
                    , removeFile
                    , withCurrentDirectory
                    )
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Refine.Backend.App as App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc


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


spec :: Spec
spec = do
  describe "VDoc" . around provideAppRunner $ do
    it "Random program" $ \runner -> forAll sampleProgram $ \program ->
      monadic (monadicApp runner) (runProgram program `evalStateT` initVDocs)


-- * Helpers

withTempCurrentDirectory :: IO a -> IO a
withTempCurrentDirectory action = withSystemTempDirectory "" (`withCurrentDirectory` action)

provideAppRunner :: ActionWith (App DB Property -> IO Property) -> IO ()
provideAppRunner action = withTempCurrentDirectory $ do
  (runner, testDb, reposRoot) <- createAppRunner
  action runner
  removeFile testDb
  removeDirectoryRecursive reposRoot

createAppRunner :: forall a . IO (App DB a -> IO a, String, String)
createAppRunner = do
  let testDb    = "test.db"
      reposRoot = "repos"
  createDirectory reposRoot
  runDb      <- createDBRunner $ DBOnDisk testDb
  runDocRepo <- createRunRepo reposRoot
  let logger = Logger . const $ pure ()
      runner :: forall b . App DB b -> IO b
      runner m = (natThrowError . runApp runDb runDocRepo logger) $$ m

  void $ runner migrateDB
  pure (runner, testDb, reposRoot)

monadicApp :: (App DB Property -> IO Property) -> App DB Property -> Property
monadicApp p = ioProperty . p


-- * monadic property

runProgram :: [Cmd] -> StateT VDocs (PropertyM (App DB)) ()
runProgram = foldl (>>) (pure ()) . map runCmd

runCmd :: Cmd -> StateT VDocs (PropertyM (App DB)) ()
runCmd (AddVDoc pv) = do
  vdoc   <- lift . run $ App.createVDoc pv
  lastId <- gets $ view vdocLast
  vdocMap  %= Map.insert lastId (vdoc ^. vdocID)
  vdocLast %= (+1)
  lift . assert $
    (vdoc ^. vdocTitle    == pv ^. protoVDocTitle) &&
    (vdoc ^. vdocAbstract == pv ^. protoVDocAbstract)

runCmd (GetVDoc v pv) = do
  Just vid <- gets . view $ vdocMap . at v
  vdoc <- lift . run $ App.getVDoc vid
  lift . assert $
    (vdoc ^. vdocTitle    == pv ^. protoVDocTitle) &&
    (vdoc ^. vdocAbstract == pv ^. protoVDocAbstract)


-- * generators

word :: (ConvertibleStrings String s) => Gen s
word = cs <$> listOf (elements ['a' .. 'z'])

-- TODO: the pattern here is 'arbitrary<type> :: Gen <type>' should be defined in
-- tests/Arbitrary.hs.  also, we need to make a package refine-test
version :: Gen (VDocVersion 'HTMLRaw)
version = pure $ VDocVersion ""

protoVDoc :: Gen (Create VDoc)
protoVDoc =
  CreateVDoc
    <$> (Title <$> word)
    <*> (Abstract . mconcat <$> listOf word)
    <*> version

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- protoVDoc
    pure [AddVDoc v, GetVDoc i v]
