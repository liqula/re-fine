{-# LANGUAGE FlexibleContexts #-}
module Refine.Backend.AppSpec where

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Lens ((^.), (%=), at, makeLenses, view)
import Control.Monad.State
import Control.Natural (($$))
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mconcat)
import Data.String.Conversions (ConvertibleStrings, cs)
import System.Directory (removeFile)

import Refine.Backend.App
import Refine.Backend.App.MigrateDB
import Refine.Backend.Database
import Refine.Backend.DocRepo
import Refine.Backend.Logger
import Refine.Backend.Natural
import Refine.Common.Prelude
import Refine.Common.VDoc

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic


data Cmd where
  AddVDoc :: Proto VDoc        -> Cmd
  GetVDoc :: Int -> Proto VDoc -> Cmd

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

  describe "VDoc" . beforeAll createAppRunner . afterAll removeTestDB $ do
    it "Random program" $ \(runner, _) -> forAll sampleProgram $ \program ->
      monadic (monadicApp runner) (runProgram program `evalStateT` initVDocs)


-- * Helpers

createAppRunner :: forall a . IO (App DB a -> IO a, String)
createAppRunner = do
  let testDb = "test.db"
  runDb      <- createDBRunner $ DBOnDisk testDb
  runDocRepo <- createRunRepo
  let logger = Logger . const $ pure ()
      runner :: forall b . App DB b -> IO b
      runner m = (natThrowError . runApp runDb runDocRepo logger) $$ m

  void $ runner migrateDB
  pure (runner, testDb)

removeTestDB :: forall a . (App DB a -> IO a, String) -> IO ()
removeTestDB (_, testDb) = removeFile testDb

monadicApp :: (App DB Property -> IO Property) -> App DB Property -> Property
monadicApp p = ioProperty . p


-- * monadic property

runProgram :: [Cmd] -> StateT VDocs (PropertyM (App DB)) ()
runProgram = foldl (>>) (pure ()) . map runCmd

runCmd :: Cmd -> StateT VDocs (PropertyM (App DB)) ()
runCmd (AddVDoc pv) = do
  vdoc   <- lift . run $ addVDoc pv
  lastId <- gets $ view vdocLast
  vdocMap  %= Map.insert lastId (vdoc ^. vdocId)
  vdocLast %= (+1)
  lift . assert $
    (vdoc ^. vdocTitle       == pv ^. protoVDocTitle) &&
    (vdoc ^. vdocDescription == pv ^. protoVDocDescription)

runCmd (GetVDoc v pv) = do
  Just vid <- gets . view $ vdocMap . at v
  vdoc <- lift . run $ getVDoc vid
  lift . assert $
    (vdoc ^. vdocTitle       == pv ^. protoVDocTitle) &&
    (vdoc ^. vdocDescription == pv ^. protoVDocDescription)


-- * generators

word :: (ConvertibleStrings String s) => Gen s
word = cs <$> listOf (elements ['a' .. 'z'])

version :: Gen Version
version = pure Version

protoVDoc :: Gen (Proto VDoc)
protoVDoc =
  ProtoVDoc
    <$> word
    <*> (mconcat <$> listOf word)
    <*> version

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- protoVDoc
    pure [AddVDoc v, GetVDoc i v]
