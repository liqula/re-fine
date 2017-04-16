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
import           Control.Lens hiding (elements)
import           Control.Monad.Trans.Identity
import           Control.Monad.State
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid (mconcat)
import           Data.String.Conversions (ConvertibleStrings, cs)
import           Data.Tree
import           Prelude hiding ((.))
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.HTML.Parser

import Refine.Backend.App         as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Backend.Test.Util (forceEval)
import Refine.Backend.User
import Refine.Common.Test.Arbitrary (arbitraryRawVDocVersion)
import Refine.Common.Types


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
    -- FIXME: Use the Cmd type instead
    it "Create/login/logout" $ \(runner :: AppM DB UH () -> IO ()) -> do
      forceEval . runner $ do
        void $ App.createUser (CreateUser "user" "user@example.com" "password")
        userState0 <- gets (view appUserState)
        appIO $ userState0 `shouldBe` UserLoggedOut

      -- The user creation should happen in a different session, than
      -- the login.
      forceEval . runner $ do
        void $ App.login (Login "user" "password")
        userState1 <- gets (view appUserState)
        appIO $ userState1 `shouldSatisfy` isActiveUser

        void App.logout
        userState2 <- gets (view appUserState)
        appIO $ userState2 `shouldBe` UserLoggedOut

  describe "Database handling" . around provideAppRunner $ do
    it "Calls db combinator twice" $ \(runner :: AppM DB UH () -> IO ()) -> do
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
            [ AddVDoc (CreateVDoc (Title "title...") (Abstract "abstract...") sampleVDocVersion1)
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
    <*> arbitraryRawVDocVersion

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- arbitraryCreateVDoc
    pure [AddVDoc v, GetVDoc i v]


-- * loud samples

sampleCreateEdit1 :: CreateEdit
sampleCreateEdit1 = CreateEdit {_createEditDesc = "...", _createEditRange = ChunkRange {_chunkRangeBegin = Nothing, _chunkRangeEnd = Nothing}, _createEditVDoc = sampleVDocVersion1, _createEditKind = Grammar, _createEditMotiv = "..."}

sampleVDocVersion1 :: VDocVersion 'HTMLRaw
sampleVDocVersion1 = VDocVersion {_unVDocVersion = [Node {rootLabel = TagOpen "p" [Attr "data-uid" "1"], subForest = [Node {rootLabel = ContentText "Donec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\nIn\nenim\njusto,\nrhoncus\nut,\nimperdiet\na,\nvenenatis\nvitae,\njusto.\nNullam\ndictum\nfelis\neu\npede\nmollis\npretium.\nInteger\ntincidunt.\nCras\ndapibus.\nVivamus\nelementum\nsemper\nnisi.\nAenean\nvulputate\neleifend\ntellus.\nAenean\nleo\nligula,\nporttitor\neu,\nconsequat\nvitae,\neleifend\nac,\nenim.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "2"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h1" [Attr "data-uid" "3"], subForest = [Node {rootLabel = ContentText "Heading\n1", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "4"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h2" [Attr "data-uid" "5"], subForest = [Node {rootLabel = ContentText "Heading\n2", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "6"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h3" [Attr "data-uid" "7"], subForest = [Node {rootLabel = ContentText "Heading\n3", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "8"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h4" [Attr "data-uid" "9"], subForest = [Node {rootLabel = ContentText "Heading\n4", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "10"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h5" [Attr "data-uid" "11"], subForest = [Node {rootLabel = ContentText "Heading\n5", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "12"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h6" [Attr "data-uid" "13"], subForest = [Node {rootLabel = ContentText "Heading\n6", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "14"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "15"], subForest = [Node {rootLabel = ContentText "Lorem\nipsum\ndolor\nsit\namet,\n", subForest = []},Node {rootLabel = TagOpen "strong" [Attr "data-uid" "16"], subForest = [Node {rootLabel = ContentText "consectetuer\nadipiscing\nelit", subForest = []}]},Node {rootLabel = ContentText ".\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.", subForest = []},Node {rootLabel = TagSelfClose "br" [Attr "data-uid" "17"], subForest = []},Node {rootLabel = ContentText "\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\nIn\nenim\njusto,\nrhoncus\nut,\nimperdiet\na,\nvenenatis\nvitae,\njusto.\nNullam\ndictum\nfelis\neu\npede\nmollis\npretium.\nInteger\ntincidunt.\nCras\ndapibus.\nVivamus\nelementum\nsemper\nnisi.\nAenean\nvulputate\neleifend\ntellus.\nAenean\nleo\nligula,\nporttitor\neu,\nconsequat\nvitae,\neleifend\nac,\nenim.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "18"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "19"], subForest = [Node {rootLabel = ContentText "Aliquam\nlorem\nante,\ndapibus\nin,\nviverra\nquis,\nfeugiat\na,\ntellus.\nPhasellus\nviverra\nnulla\nut\nmetus\nvarius\nlaoreet.\nQuisque\nrutrum.\nAenean\nimperdiet.\nEtiam\nultricies\nnisi\nvel\naugue.\nCurabitur\nullamcorper\nultricies\nnisi.\nNam\neget\ndui.\nEtiam\nrhoncus.\nMaecenas\ntempus,\ntellus\neget\ncondimentum\nrhoncus,\nsem\nquam\nsemper\nlibero,\nsit\namet\nadipiscing\nsem\nneque\nsed\nipsum.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "20"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h1" [Attr "data-uid" "21"], subForest = [Node {rootLabel = ContentText "Heading\n1", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "22"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "23"], subForest = [Node {rootLabel = ContentText "Lorem\nipsum\ndolor\nsit\namet,\n", subForest = []},Node {rootLabel = TagOpen "strong" [Attr "data-uid" "24"], subForest = [Node {rootLabel = ContentText "consectetuer\nadipiscing\nelit", subForest = []}]},Node {rootLabel = ContentText ".\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "25"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h2" [Attr "data-uid" "26"], subForest = [Node {rootLabel = ContentText "Heading\n2", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "27"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "28"], subForest = [Node {rootLabel = ContentText "Lorem\nipsum\ndolor\nsit\namet,\n", subForest = []},Node {rootLabel = TagOpen "strong" [Attr "data-uid" "29"], subForest = [Node {rootLabel = ContentText "consectetuer\nadipiscing\nelit", subForest = []}]},Node {rootLabel = ContentText ".\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "30"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h3" [Attr "data-uid" "31"], subForest = [Node {rootLabel = ContentText "Heading\n3", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "32"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "33"], subForest = [Node {rootLabel = ContentText "Lorem\nipsum\ndolor\nsit\namet,\n", subForest = []},Node {rootLabel = TagOpen "strong" [Attr "data-uid" "34"], subForest = [Node {rootLabel = ContentText "consectetuer\nadipiscing\nelit", subForest = []}]},Node {rootLabel = ContentText ".\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "35"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h4" [Attr "data-uid" "36"], subForest = [Node {rootLabel = ContentText "Heading\n4", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "37"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "38"], subForest = [Node {rootLabel = ContentText "Lorem\nipsum\ndolor\nsit\namet,\n", subForest = []},Node {rootLabel = TagOpen "strong" [Attr "data-uid" "39"], subForest = [Node {rootLabel = ContentText "consectetuer\nadipiscing\nelit", subForest = []}]},Node {rootLabel = ContentText ".\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "40"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h5" [Attr "data-uid" "41"], subForest = [Node {rootLabel = ContentText "Heading\n5", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "42"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "43"], subForest = [Node {rootLabel = ContentText "Lorem\nipsum\ndolor\nsit\namet,\n", subForest = []},Node {rootLabel = TagOpen "strong" [Attr "data-uid" "44"], subForest = [Node {rootLabel = ContentText "consectetuer\nadipiscing\nelit", subForest = []}]},Node {rootLabel = ContentText ".\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "45"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "h6" [Attr "data-uid" "46"], subForest = [Node {rootLabel = ContentText "Heading\n6", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "47"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "48"], subForest = [Node {rootLabel = ContentText "Lorem\nipsum\ndolor\nsit\namet,\n", subForest = []},Node {rootLabel = TagOpen "strong" [Attr "data-uid" "49"], subForest = [Node {rootLabel = ContentText "consectetuer\nadipiscing\nelit", subForest = []}]},Node {rootLabel = ContentText ".\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "50"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "51"], subForest = [Node {rootLabel = ContentText "Textlink", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "52"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "p" [Attr "data-uid" "53"], subForest = [Node {rootLabel = ContentText "Table\nHeading\nATable\nHeading\nBTable\nData\nATable\nData\nB", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "54"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "ul" [Attr "data-uid" "55"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "56"], subForest = [Node {rootLabel = ContentText "Eintrag\nA", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "57"], subForest = [Node {rootLabel = ContentText "Eintrag\nB", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "58"], subForest = [Node {rootLabel = ContentText "Eintrag\nC", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "span" [Attr "data-uid" "59"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []}]},Node {rootLabel = TagOpen "ol" [Attr "data-uid" "60"], subForest = [Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "61"], subForest = [Node {rootLabel = ContentText "Eintrag\n1", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "62"], subForest = [Node {rootLabel = ContentText "Eintrag\n2", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "63"], subForest = [Node {rootLabel = ContentText "Eintrag\n3", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "64"], subForest = [Node {rootLabel = ContentText "Eintrag\n4", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "65"], subForest = [Node {rootLabel = ContentText "Eintrag\n5", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "66"], subForest = [Node {rootLabel = ContentText "Eintrag\n6", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "67"], subForest = [Node {rootLabel = ContentText "Eintrag\n7", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "68"], subForest = [Node {rootLabel = ContentText "Eintrag\n8", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "69"], subForest = [Node {rootLabel = ContentText "Eintrag\n9", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "70"], subForest = [Node {rootLabel = ContentText "Eintrag\n10", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "71"], subForest = [Node {rootLabel = ContentText "Eintrag\n11", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []},Node {rootLabel = TagOpen "li" [Attr "data-uid" "72"], subForest = [Node {rootLabel = ContentText "Eintrag\n12", subForest = []}]},Node {rootLabel = ContentText "\n\n", subForest = []}]}]}
