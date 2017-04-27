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
import           Prelude hiding ((.))
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import Refine.Backend.App as App
import Refine.Backend.Database
import Refine.Backend.Test.AppRunner
import Refine.Backend.Test.Util (forceEval)
import Refine.Backend.User
import Refine.Common.Test.Arbitrary ()
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
    <*> (VDocVersion . cs . show <$> arbitrary @RawContent)

sampleProgram :: Gen [Cmd]
sampleProgram = do
  n <- min 0 <$> arbitrary
  fmap concat . forM [0 .. n] $ \i -> do
    v <- arbitraryCreateVDoc
    pure [AddVDoc v, GetVDoc i v]


-- * loud samples

sampleCreateEdit1 :: CreateEdit
sampleCreateEdit1 = CreateEdit {_createEditDesc = "...", _createEditRange = ChunkRange {_chunkRangeBegin = Nothing, _chunkRangeEnd = Nothing}, _createEditVDoc = sampleVDocVersion1, _createEditKind = Grammar, _createEditMotiv = "..."}

-- | (this is just a lot of noise.  we should call arbitrary to construct RawContent values instead.)
sampleVDocVersion1 :: VDocVersion
sampleVDocVersion1 = VDocVersion "{_unVDocVersion = [Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n1\n], subForest = [Node {rootLabel = ContentText \nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\nIn\nenim\njusto,\nrhoncus\nut,\nimperdiet\na,\nvenenatis\nvitae,\njusto.\nNullam\ndictum\nfelis\neu\npede\nmollis\npretium.\nInteger\ntincidunt.\nCras\ndapibus.\nVivamus\nelementum\nsemper\nnisi.\nAenean\nvulputate\neleifend\ntellus.\nAenean\nleo\nligula,\nporttitor\neu,\nconsequat\nvitae,\neleifend\nac,\nenim.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n2\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh1\n [Attr \ndata-uid\n \n3\n], subForest = [Node {rootLabel = ContentText \nHeading\n1\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n4\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh2\n [Attr \ndata-uid\n \n5\n], subForest = [Node {rootLabel = ContentText \nHeading\n2\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n6\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh3\n [Attr \ndata-uid\n \n7\n], subForest = [Node {rootLabel = ContentText \nHeading\n3\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n8\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh4\n [Attr \ndata-uid\n \n9\n], subForest = [Node {rootLabel = ContentText \nHeading\n4\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n10\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh5\n [Attr \ndata-uid\n \n11\n], subForest = [Node {rootLabel = ContentText \nHeading\n5\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n12\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh6\n [Attr \ndata-uid\n \n13\n], subForest = [Node {rootLabel = ContentText \nHeading\n6\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n14\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n15\n], subForest = [Node {rootLabel = ContentText \nLorem\nipsum\ndolor\nsit\namet,\n\n, subForest = []},Node {rootLabel = TagOpen \nstrong\n [Attr \ndata-uid\n \n16\n], subForest = [Node {rootLabel = ContentText \nconsectetuer\nadipiscing\nelit\n, subForest = []}]},Node {rootLabel = ContentText \n.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\n, subForest = []},Node {rootLabel = TagSelfClose \nbr\n [Attr \ndata-uid\n \n17\n], subForest = []},Node {rootLabel = ContentText \n\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\nIn\nenim\njusto,\nrhoncus\nut,\nimperdiet\na,\nvenenatis\nvitae,\njusto.\nNullam\ndictum\nfelis\neu\npede\nmollis\npretium.\nInteger\ntincidunt.\nCras\ndapibus.\nVivamus\nelementum\nsemper\nnisi.\nAenean\nvulputate\neleifend\ntellus.\nAenean\nleo\nligula,\nporttitor\neu,\nconsequat\nvitae,\neleifend\nac,\nenim.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n18\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n19\n], subForest = [Node {rootLabel = ContentText \nAliquam\nlorem\nante,\ndapibus\nin,\nviverra\nquis,\nfeugiat\na,\ntellus.\nPhasellus\nviverra\nnulla\nut\nmetus\nvarius\nlaoreet.\nQuisque\nrutrum.\nAenean\nimperdiet.\nEtiam\nultricies\nnisi\nvel\naugue.\nCurabitur\nullamcorper\nultricies\nnisi.\nNam\neget\ndui.\nEtiam\nrhoncus.\nMaecenas\ntempus,\ntellus\neget\ncondimentum\nrhoncus,\nsem\nquam\nsemper\nlibero,\nsit\namet\nadipiscing\nsem\nneque\nsed\nipsum.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n20\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh1\n [Attr \ndata-uid\n \n21\n], subForest = [Node {rootLabel = ContentText \nHeading\n1\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n22\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n23\n], subForest = [Node {rootLabel = ContentText \nLorem\nipsum\ndolor\nsit\namet,\n\n, subForest = []},Node {rootLabel = TagOpen \nstrong\n [Attr \ndata-uid\n \n24\n], subForest = [Node {rootLabel = ContentText \nconsectetuer\nadipiscing\nelit\n, subForest = []}]},Node {rootLabel = ContentText \n.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n25\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh2\n [Attr \ndata-uid\n \n26\n], subForest = [Node {rootLabel = ContentText \nHeading\n2\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n27\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n28\n], subForest = [Node {rootLabel = ContentText \nLorem\nipsum\ndolor\nsit\namet,\n\n, subForest = []},Node {rootLabel = TagOpen \nstrong\n [Attr \ndata-uid\n \n29\n], subForest = [Node {rootLabel = ContentText \nconsectetuer\nadipiscing\nelit\n, subForest = []}]},Node {rootLabel = ContentText \n.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n30\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh3\n [Attr \ndata-uid\n \n31\n], subForest = [Node {rootLabel = ContentText \nHeading\n3\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n32\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n33\n], subForest = [Node {rootLabel = ContentText \nLorem\nipsum\ndolor\nsit\namet,\n\n, subForest = []},Node {rootLabel = TagOpen \nstrong\n [Attr \ndata-uid\n \n34\n], subForest = [Node {rootLabel = ContentText \nconsectetuer\nadipiscing\nelit\n, subForest = []}]},Node {rootLabel = ContentText \n.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n35\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh4\n [Attr \ndata-uid\n \n36\n], subForest = [Node {rootLabel = ContentText \nHeading\n4\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n37\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n38\n], subForest = [Node {rootLabel = ContentText \nLorem\nipsum\ndolor\nsit\namet,\n\n, subForest = []},Node {rootLabel = TagOpen \nstrong\n [Attr \ndata-uid\n \n39\n], subForest = [Node {rootLabel = ContentText \nconsectetuer\nadipiscing\nelit\n, subForest = []}]},Node {rootLabel = ContentText \n.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n40\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh5\n [Attr \ndata-uid\n \n41\n], subForest = [Node {rootLabel = ContentText \nHeading\n5\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n42\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n43\n], subForest = [Node {rootLabel = ContentText \nLorem\nipsum\ndolor\nsit\namet,\n\n, subForest = []},Node {rootLabel = TagOpen \nstrong\n [Attr \ndata-uid\n \n44\n], subForest = [Node {rootLabel = ContentText \nconsectetuer\nadipiscing\nelit\n, subForest = []}]},Node {rootLabel = ContentText \n.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n45\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nh6\n [Attr \ndata-uid\n \n46\n], subForest = [Node {rootLabel = ContentText \nHeading\n6\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n47\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n48\n], subForest = [Node {rootLabel = ContentText \nLorem\nipsum\ndolor\nsit\namet,\n\n, subForest = []},Node {rootLabel = TagOpen \nstrong\n [Attr \ndata-uid\n \n49\n], subForest = [Node {rootLabel = ContentText \nconsectetuer\nadipiscing\nelit\n, subForest = []}]},Node {rootLabel = ContentText \n.\nAenean\ncommodo\nligula\neget\ndolor.\nAenean\nmassa.\nCum\nsociis\nnatoque\npenatibus\net\nmagnis\ndis\nparturient\nmontes,\nnascetur\nridiculus\nmus.\nDonec\nquam\nfelis,\nultricies\nnec,\npellentesque\neu,\npretium\nquis,\nsem.\nNulla\nconsequat\nmassa\nquis\nenim.\nDonec\npede\njusto,\nfringilla\nvel,\naliquet\nnec,\nvulputate\neget,\narcu.\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n50\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n51\n], subForest = [Node {rootLabel = ContentText \nTextlink\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n52\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \np\n [Attr \ndata-uid\n \n53\n], subForest = [Node {rootLabel = ContentText \nTable\nHeading\nATable\nHeading\nBTable\nData\nATable\nData\nB\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n54\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nul\n [Attr \ndata-uid\n \n55\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n56\n], subForest = [Node {rootLabel = ContentText \nEintrag\nA\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n57\n], subForest = [Node {rootLabel = ContentText \nEintrag\nB\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n58\n], subForest = [Node {rootLabel = ContentText \nEintrag\nC\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nspan\n [Attr \ndata-uid\n \n59\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []}]},Node {rootLabel = TagOpen \nol\n [Attr \ndata-uid\n \n60\n], subForest = [Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n61\n], subForest = [Node {rootLabel = ContentText \nEintrag\n1\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n62\n], subForest = [Node {rootLabel = ContentText \nEintrag\n2\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n63\n], subForest = [Node {rootLabel = ContentText \nEintrag\n3\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n64\n], subForest = [Node {rootLabel = ContentText \nEintrag\n4\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n65\n], subForest = [Node {rootLabel = ContentText \nEintrag\n5\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n66\n], subForest = [Node {rootLabel = ContentText \nEintrag\n6\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n67\n], subForest = [Node {rootLabel = ContentText \nEintrag\n7\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n68\n], subForest = [Node {rootLabel = ContentText \nEintrag\n8\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n69\n], subForest = [Node {rootLabel = ContentText \nEintrag\n9\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n70\n], subForest = [Node {rootLabel = ContentText \nEintrag\n10\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n71\n], subForest = [Node {rootLabel = ContentText \nEintrag\n11\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []},Node {rootLabel = TagOpen \nli\n [Attr \ndata-uid\n \n72\n], subForest = [Node {rootLabel = ContentText \nEintrag\n12\n, subForest = []}]},Node {rootLabel = ContentText \n\n\n\n, subForest = []}]}]}\n"
