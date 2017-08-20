{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.App.AccessSpec where

import Refine.Backend.Prelude

import           Control.Concurrent.MVar
import           Data.Either (isRight)
import qualified Data.Tree as Tree
import qualified Network.Wai.Test.Internal as Wai
import           Test.Hspec
import           System.Process (system)

import Refine.Backend.App
import Refine.Backend.Config
import Refine.Backend.Database (DB)
import qualified Refine.Backend.Database.Class as DB
import Refine.Backend.Test.AppServer
import Refine.Backend.Test.Util (withTempCurrentDirectory)
import Refine.Common.ChangeAPI
import Refine.Common.Test.Util
import Refine.Common.Types
import Refine.Common.VDoc.Draft

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


type SwitchUser = SessUser -> IO ()
type GetUid = SessUser -> ID User
data SessUser = Admin | Alice | Bob
  deriving (Eq, Ord, Enum, Show)

setup :: HasCallStack => ((SwitchUser, GetUid, TestBackend) -> IO ()) -> IO ()
setup action = withTempCurrentDirectory $ do
  let verbose = False
      dbFilePath = "./test.db"
      cfg = def
        & cfgLogger     .~ (if verbose
                            then LogCfg LogCfgStdOut LogDebug
                            else LogCfg (LogCfgFile testLogfilePath) LogError)
        & cfgDBKind     .~ (if verbose
                            then DBOnDisk dbFilePath
                            else DBInMemory)
        & cfgSmtp       .~ Nothing
        & cfgAllAreGods .~ False
  (tbe, destroy) <- mkTestBackend cfg
  testBackendFetchCookie tbe

  adminID      <- addUserAndLogin tbe "admin" "admin@example.com" "pass"
  adminSession <- modifyMVar (tbe ^. testBackendState) (\old -> pure (Wai.ClientState mempty, old))
  testBackendFetchCookie tbe
  aliceID      <- addUserAndLogin tbe "alice" "alice@example.com" "pass"
  aliceSession <- modifyMVar (tbe ^. testBackendState) (\old -> pure (Wai.ClientState mempty, old))
  testBackendFetchCookie tbe
  bobID        <- addUserAndLogin tbe "bob"   "bob@example.com"   "pass"
  bobSession   <- modifyMVar (tbe ^. testBackendState) (\old -> pure (Wai.ClientState mempty, old))
  testBackendFetchCookie tbe

  runDB tbe $ do
    unsafeBeAGod
    changeRole $ AssignGlobalRole adminID GlobalAdmin
    beAMortal

  let switchUser :: SessUser -> IO ()
      switchUser u = modifyMVar (tbe ^. testBackendState) $ \_ -> pure (snd $ userMap u, ())

      getUid :: GetUid
      getUid = fst . userMap

      userMap :: SessUser -> (ID User, Wai.ClientState)
      userMap Admin = (adminID, adminSession)
      userMap Alice = (aliceID, aliceSession)
      userMap Bob = (bobID, bobSession)

  action (switchUser, getUid, tbe)

  -- dump database (for debugging)
  when verbose . void . system $ "echo .dump | sqlite3 " <> dbFilePath

  destroy

setupVDocAsGod :: HasCallStack => [ID User] -> App VDoc
setupVDocAsGod owners = do
  gid <- view groupID . head <$> db DB.getGroups
  vdoc <- db . DB.createVDoc $ CreateVDoc (Title "title") (Abstract "abstract") emptyRawContent gid
  (\owner -> db $ DB.assignGroupRole gid owner GroupMember) `mapM_` owners
  pure vdoc

setupDiscussion :: HasCallStack => Bool -> VDoc -> App Discussion
setupDiscussion asGod vdoc = when asGod unsafeBeAGod *> addDiscussion eid cd <* when asGod beAMortal
  where
    eid = vdoc ^. vdocHeadEdit
    cd = CreateDiscussion "first post!" Nothing


shouldGrant :: (HasCallStack, Show a) => TestBackend -> AppM DB a -> IO ()
shouldGrant sess probe = runDB' sess probe >>= (`shouldSatisfy` isRight)

shouldDeny :: (HasCallStack, Show a) => TestBackend -> AppM DB a -> IO ()
shouldDeny sess probe = runDB' sess probe >>= \case
  Left (AppUnauthorized _) -> passes
  bad -> failsOn bad


spec :: Spec
spec = around setup $ do
  describe "add group" $ do
    it "grant: roles [GlobalAdmin]" $ \(switchUser, _getUid, sess) -> do
      switchUser Admin
      shouldGrant sess . addGroup $ CreateGroup "title" "desc" [] []

    it "deny: roles []" $ \(switchUser, _getUid, sess) -> do
      switchUser Bob
      shouldDeny sess . addGroup $ CreateGroup "title" "desc" [] []


  describe "list all groups" $ do
    it "show only visible groups: roles [(GroupMember, 1), (GroupMember, 2)]; groups [1, 2, 3]" $ \(switchUser, getUid, sess) -> do
      switchUser Admin
      [g1, g2, _g3] <- runDB sess $ do
        g1_ <- addGroup $ CreateGroup "g1" "a" [] []
        g2_ <- addGroup $ CreateGroup "g2" "b" [] []
        g3_ <- addGroup $ CreateGroup "g3" "c" [] []
        changeRole $ AssignGroupRole (getUid Alice) GroupMember (g1_ ^. groupID)
        changeRole $ AssignGroupRole (getUid Alice) GroupMember (g2_ ^. groupID)
        pure [g1_, g2_, g3_]

      switchUser Alice
      aliceGroups <- runDB sess getGroups
      length aliceGroups `shouldBe` 2
      sort (view groupID <$> aliceGroups) `shouldBe` sort [g1 ^. groupID, g2 ^. groupID]

      switchUser Admin
      adminGroups <- runDB sess getGroups
      length adminGroups `shouldBe` 4  -- (there is also the default group)

  describe "add process" $ do
    let getgid sess = view groupID . head <$> runDB sess getGroups
        addproc = createVDoc . CreateVDoc (Title "title") (Abstract "abstract") emptyRawContent

    it "grant: roles [GlobalAdmin]" $ \(switchUser, _getUid, sess) -> do
      switchUser Admin
      gid <- getgid sess
      shouldGrant sess $ addproc gid

    it "grant: roles [(GroupMember, <this group>)]" $ \(switchUser, getUid, sess) -> do
      switchUser Admin
      gid <- getgid sess
      runDB sess . changeRole $ AssignGroupRole (getUid Bob) GroupMember gid
      switchUser Bob
      shouldGrant sess $ addproc gid

    it "deny: roles []" $ \(switchUser, _getUid, sess) -> do
      switchUser Admin
      gid <- getgid sess
      switchUser Bob
      shouldDeny sess $ addproc gid


  describe "vdoc discussion" $ do
    describe "add" $ do
      it "grant: roles [GroupMember]" $ \(switchUser, getUid, sess) -> do
        switchUser Admin
        vdoc <- runDB sess $ setupVDocAsGod [getUid Alice]
        switchUser Alice
        shouldGrant sess $ setupDiscussion False vdoc

      it "deny: roles []" $ \(switchUser, getUid, sess) -> do
        switchUser Admin
        vdoc <- runDB sess $ setupVDocAsGod [getUid Alice]
        switchUser Bob
        shouldDeny sess $ setupDiscussion False vdoc

    describe "show" $ do
      it "grant: roles [GroupMember]" $ \(switchUser, getUid, sess) -> do
        switchUser Admin
        vdoc <- runDB sess $ setupVDocAsGod [getUid Alice]
        disc <- runDB sess $ setupDiscussion True vdoc
        switchUser Alice
        shouldGrant sess $ getDiscussion (disc ^. discussionID)

      it "deny: roles []" $ \(switchUser, getUid, sess) -> do
        switchUser Admin
        vdoc <- runDB sess $ setupVDocAsGod [getUid Alice]
        disc <- runDB sess $ setupDiscussion True vdoc
        switchUser Bob
        shouldDeny sess $ getDiscussion (disc ^. discussionID)

    describe "add statement" $ do
      it "grant: roles [GroupMember]" $ \(switchUser, getUid, sess) -> do
        switchUser Admin
        vdoc <- runDB sess $ setupVDocAsGod [getUid Alice]
        disc <- runDB sess $ setupDiscussion True vdoc
        let st = head . Tree.flatten $ disc ^. discussionTree
        switchUser Alice
        shouldGrant sess $ addStatement (st ^. statementID) (CreateStatement "second post?")
        switchUser Admin
        disc' <- runDB sess $ getDiscussion (disc ^. discussionID)
        length (Tree.flatten $ disc' ^. discussionTree) `shouldBe` 2

      it "deny: roles []" $ \(switchUser, getUid, sess) -> do
        switchUser Admin
        vdoc <- runDB sess $ setupVDocAsGod [getUid Alice]
        disc <- runDB sess $ setupDiscussion True vdoc
        let stmt = head . Tree.flatten $ disc ^. discussionTree
        switchUser Bob
        shouldDeny sess $ addStatement (stmt ^. statementID) (CreateStatement "second post?")

    describe "update statement" $ do
      let setupUpd getUid sess = do
            vdoc <- runDB sess $ setupVDocAsGod [getUid Alice, getUid Bob]
            disc <- runDB sess $ setupDiscussion True vdoc
            pure . view statementID . head . Tree.flatten $ disc ^. discussionTree

      it "grant: roles [GlobalAdmin]" $ \(switchUser, getUid, sess) -> do
        switchUser Alice
        sid <- setupUpd getUid sess
        switchUser Admin
        shouldGrant sess $ updateStatement sid (CreateStatement "second post, definitely...")

      it "grant: owner" $ \(switchUser, getUid, sess) -> do
        switchUser Alice
        sid <- setupUpd getUid sess
        switchUser Alice
        shouldGrant sess $ updateStatement sid (CreateStatement "second post, definitely...")

      it "deny: not owner, not admin, but group member" $ \(switchUser, getUid, sess) -> do
        switchUser Alice
        sid <- setupUpd getUid sess
        switchUser Bob
        shouldDeny sess $ updateStatement sid (CreateStatement "second post, definitely...")

  describe "edit" $ do
    describe "add" $ do
      it "grant: roles [GroupMember]" $ \(_switchUser, _getUid, _sess) -> do
        pending
      it "deny: roles []" $ \(_switchUser, _getUid, _sess) -> do
        pending

    describe "show" $ do
      it "grant: roles [GroupMember]" $ \(_switchUser, _getUid, _sess) -> do
        pending
      it "deny: roles []" $ \(_switchUser, _getUid, _sess) -> do
        pending

    describe "vote" $ do
      it "grant: roles [GroupMember]" $ \(_switchUser, _getUid, _sess) -> do
        pending
      it "deny: roles []" $ \(_switchUser, _getUid, _sess) -> do
        pending

    describe "update statement" $ do
      it "grant: owner" $ \(_switchUser, _getUid, _sess) -> do
        pending
      it "grant: roles [GlobalAdmin]" $ \(_switchUser, _getUid, _sess) -> do
        pending
      it "deny: not owner, not admin" $ \(_switchUser, _getUid, _sess) -> do
        pending
