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
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad (void)
import           Control.Natural (run)
import           Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import           Data.Default (def)
import           Data.Proxy (Proxy(Proxy))
import           Data.String.Conversions (SBS, cs, (<>))
import           Network.HTTP.Types.Status (Status(statusCode))
import           Network.URI (URI, uriToString)
import           Network.Wai.Test (SResponse(..))
import           Servant.Utils.Links (safeLink)
import           Test.Hspec
                  ( Spec
                  , ActionWith
                  , around, describe, it
                  , shouldBe, shouldContain
                  , pendingWith
                  )
import           Test.Hspec.Wai (get, request)
import qualified Test.Hspec.Wai.Internal as Wai

import Refine.Backend.App as App
import Refine.Backend.AppSpec (withTempCurrentDirectory)
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

addPatchUri :: ID Patch -> SBS
addPatchUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddPatch)

addCommentUri :: ID Patch -> SBS
addCommentUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddComment)

addNoteUri :: ID Patch -> SBS
addNoteUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddNote)

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

  describe "sAddComment" $ do
    it "stores comment with no ranges" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      fc :: Comment       <- runWaiBody sess $
        postJSON
          (addCommentUri (fe ^. compositeVDocRepo . vdocHeadPatch))
          (CreateComment "[comment]" True (CreateChunkRange Nothing Nothing))
      be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      be ^. compositeVDocComments `shouldContain` [fc]

  describe "sAddNote" $ do
    it "stores comment with no ranges" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      fn :: Note          <- runWaiBody sess $
        postJSON
          (addNoteUri (fe ^. compositeVDocRepo . vdocHeadPatch))
          (CreateNote "[note]" Remark (CreateChunkRange Nothing Nothing))
      be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      be ^. compositeVDocNotes `shouldContain` [fn]

  describe "sAddPatch" $ do
    it "it stores a patch with no ranges" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON createVDocUri sampleCreateVDoc
      fp :: Patch         <- runWaiBody sess $
        postJSON
          (addPatchUri (fe ^. compositeVDocRepo . vdocHeadPatch))
          (CreatePatch "new patch" (CreateChunkRange Nothing Nothing) (VDocVersion "[new vdoc version]"))

      -- FIXME: Remove this check use the appropiate one after the pending.
      be' :: VDocVersion 'HTMLCanonical <- runDB sess $ do
              handles <- db $ DB.handlesForPatch (fp ^. patchID)
              docRepo $ uncurry DocRepo.getVersion handles
      be' `shouldBe` VDocVersion "<span>[new\nvdoc\nversion]</span>"

      pendingWith "applicablePatches is not implemented."
      be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      be ^. compositeVDocPatches `shouldContain` [fp]
