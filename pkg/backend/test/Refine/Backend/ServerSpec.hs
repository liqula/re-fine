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

module Refine.Backend.ServerSpec where

import           Control.Exception (throwIO, ErrorCall(ErrorCall))
import           Control.Lens ((^.), (.~), (&), to)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad (void)
import           Control.Natural (run)
import           Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import           Data.Default (def)
import           Data.Proxy (Proxy(Proxy))
import           Data.Map (elems)
import           Data.String.Conversions (SBS, cs, (<>))
import           Network.HTTP.Types.Status (Status(statusCode))
import           Network.URI (URI, uriToString)
import           Network.Wai.Test (SResponse(..))
import           Servant.Utils.Links (safeLink)
import           Test.Hspec
                  ( Spec
                  , ActionWith
                  , around, describe, context, it
                  , shouldBe, shouldContain
                  , pendingWith
                  )
import           Test.Hspec.Wai (get, request)
import qualified Test.Hspec.Wai.Internal as Wai

import Refine.Backend.App as App
import Refine.Backend.Test.Util (withTempCurrentDirectory)
import Refine.Backend.Config
import Refine.Backend.Database (DB)
import Refine.Backend.Database.Class as DB
import Refine.Backend.DocRepo as DocRepo
import Refine.Backend.Server
import Refine.Common.Rest
import Refine.Common.Types


-- * machine room

runWai :: Backend -> Wai.WaiSession a -> IO a
runWai sess = Wai.withApplication (backendServer sess)

-- | Call 'runWaiBody'' and crash if the expected type cannot be read.
runWaiBody :: FromJSON a => Backend -> Wai.WaiSession SResponse -> IO a
runWaiBody sess = errorOnLeft . runWaiBody' sess

-- | Run a rest call and parse the body.
runWaiBody' :: FromJSON a => Backend -> Wai.WaiSession SResponse -> IO (Either String a)
runWaiBody' sess m = do
  resp <- Wai.withApplication (backendServer sess) m
  pure $ case eitherDecode $ simpleBody resp of
    Left err -> Left $ show err <> cs (simpleBody resp)
    Right x  -> Right x

-- | Call 'runDB'' and crash on 'Left'.
runDB :: Backend -> App DB a -> IO a
runDB sess = errorOnLeft . runDB' sess

-- | Call an 'App' action.
runDB' :: Backend -> App DB a -> IO (Either AppError a)
runDB' sess = runExceptT . run (backendMonad sess)

errorOnLeft :: Show e => IO (Either e a) -> IO a
errorOnLeft action = either (throwIO . ErrorCall . show) pure =<< action

createTestSession :: ActionWith Backend -> IO ()
createTestSession action = withTempCurrentDirectory $ do
  void $ action =<< mkBackend (def & cfgShouldLog .~ False)


-- * test helpers

-- (see also: https://hackage.haskell.org/package/wai-extra/docs/Network-Wai-Test.html)

sampleCreateVDoc :: CreateVDoc
sampleCreateVDoc = CreateVDoc
  (Title "[title]")
  (Abstract "[abstract]")
  (VDocVersion "[versioned content]")

respCode :: SResponse -> Int
respCode = statusCode . simpleStatus

postJSON :: (ToJSON a) => SBS -> a -> Wai.WaiSession SResponse
postJSON path json = request "POST" path [("Content-Type", "application/json")] (encode json)


-- * endpoints

uriStr :: URI -> SBS
uriStr u =  cs $ "/" <> uriToString id u ""

listVDocsUri :: SBS
listVDocsUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SListVDocs)

getVDocUri :: ID VDoc -> SBS
getVDocUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetVDoc)

createVDocUri :: SBS
createVDocUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SCreateVDoc)

addEditUri :: ID Edit -> SBS
addEditUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddEdit)

addNoteUri :: ID Edit -> SBS
addNoteUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddNote)

addDiscussionUri :: ID Edit -> SBS
addDiscussionUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddDiscussion)


-- * test cases

spec :: Spec
spec = around createTestSession $ do  -- FUTUREWORK: mark this as 'parallel' (needs some work)

  describe "sListVDocs" $ do
    it "returns a vdocs list with HTTP status 200" $ \sess -> do
      resp :: SResponse <- runWai sess $ get listVDocsUri
      respCode resp `shouldBe` 200

    it "yields the same vdocs list as the db" $ \sess -> do
      vdocsRest :: SResponse <- runWai sess $ get listVDocsUri
      vdocsDB   :: [VDoc]    <- runDB  sess   App.listVDocs
      decode (simpleBody vdocsRest) `shouldBe` Just vdocsDB

    it "if a vdoc is created, it shows in the output" $ \sess -> do
      bef <- runWai sess $ get listVDocsUri
      _   <- runDB  sess $ App.createVDoc sampleCreateVDoc
      aft <- runWai sess $ get listVDocsUri
      let check :: SResponse -> Int
          check resp = either (\msg -> error $ unwords [show msg, cs $ simpleBody resp]) length
                       $ eitherDecode @[VDoc] (simpleBody resp)
      check aft `shouldBe` (check bef + 1)

  describe "sGetVDoc" $ do
    it "retrieves a vdoc" $ \sess -> do
      vdoc <- runDB sess $ App.createVDoc sampleCreateVDoc
      resp <- runWai sess . get $ getVDocUri (vdoc ^. vdocID)
      respCode resp `shouldBe` 200

  describe "sCreateVDoc" $ do
    it "stores a vdoc in the db" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      be :: CompositeVDoc <- runDB      sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      fe `shouldBe` be

  describe "sAddNote" $ do
    it "stores note with full-document chunk range" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      fn :: Note          <- runWaiBody sess $
        postJSON
          (addNoteUri (fe ^. compositeVDocRepo . vdocHeadEdit))
          (CreateNote "[note]" True (CreateChunkRange Nothing Nothing))
      be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      be ^. compositeVDocNotes . to elems `shouldContain` [fn]

    it "stores note with non-trivial valid chunk range" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      fn :: Note          <- runWaiBody sess $
        let cp1, cp2 :: ChunkPoint
            cp1 = ChunkPoint (DataUID 1) 0
            cp2 = ChunkPoint (DataUID 1) 1
        in postJSON
          (addNoteUri (fe ^. compositeVDocRepo . vdocHeadEdit))
          (CreateNote "[note]" True (CreateChunkRange (Just cp1) (Just cp2)))
      be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      be ^. compositeVDocNotes . to elems `shouldContain` [fn]

    it "fails with error on non-trivial *invalid* chunk range" $ \sess -> do
      vdoc :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      Left resp :: Either String Note <- runWaiBody' sess $
        let cp1, cp2 :: ChunkPoint
            cp1 = ChunkPoint (DataUID 1) 0
            cp2 = ChunkPoint (DataUID 100) 100
        in postJSON
          (addNoteUri (vdoc ^. compositeVDocRepo . vdocHeadEdit))
          (CreateNote "[note]" True (CreateChunkRange (Just cp1) (Just cp2)))

      (resp `shouldContain`) `mapM_` ["AppVDocError", "VDocHTMLErrorChunkRangeErrors", "ChunkRangeBadEndNode"]

      vdoc' :: CompositeVDoc <- runDB sess $ getCompositeVDoc (vdoc ^. compositeVDoc . vdocID)
      vdoc' ^. compositeVDocNotes `shouldBe` mempty

  describe "sAddDiscussion" $ do
    it "stores discussion with no ranges" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      fn :: CompositeDiscussion <- runWaiBody sess $
        postJSON
          (addDiscussionUri (fe ^. compositeVDocRepo . vdocHeadEdit))
          (CreateDiscussion "[discussion initial statement]" True (CreateChunkRange Nothing Nothing))
      be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      be ^. compositeVDocDiscussions . to elems `shouldContain` [fn]

  describe "sAddStatement" $ do
    it "stores statement for given discussion" $ \_sess -> do
      pendingWith "this test case shouldn't be too hard to write, and should be working already."

  describe "sAddEdit" $ do
    let setup sess = do
          fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
          fp :: Edit          <- runWaiBody sess $
            postJSON
              (addEditUri (fe ^. compositeVDocRepo . vdocHeadEdit))
              (CreateEdit "new edit" (CreateChunkRange Nothing Nothing) (VDocVersion "[new vdoc version]"))
          pure (fe, fp)

    context "on edit without ranges" $ do
      it "stores an edit and returns its version" $ \sess -> do
        (_, fp) <- setup sess
        be' :: VDocVersion 'HTMLCanonical <- runDB sess $ do
              handles <- db $ DB.handlesForEdit (fp ^. editID)
              docRepo $ uncurry DocRepo.getVersion handles
        be' `shouldBe` VDocVersion "<span data-uid=\"1\">[new\nvdoc\nversion]</span>"

      it "stores an edit and returns it in the list of edits applicable to its base" $ \sess -> do
        pendingWith "applicableEdits is not implemented."
        (fe, fp) <- setup sess
        be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
        be ^. compositeVDocEdits . to elems`shouldContain` [fp]
