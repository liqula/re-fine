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
import           Control.Lens ((^.), to)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad (void)
import           Control.Natural (run)
import           Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import           Data.String.Conversions (SBS, ST, cs, (<>))
import           Network.HTTP.Types.Status (Status(statusCode))
import           Network.Wai.Test (SResponse(..))
import           Test.Hspec (Spec, ActionWith, around, describe, it, shouldBe, pending)
import           Test.Hspec.Wai (get, request)
import qualified Test.Hspec.Wai.Internal as Wai
import           Web.HttpApiData (toUrlPiece)

import Refine.Backend.App as App
import Refine.Backend.AppSpec (withTempCurrentDirectory)
import Refine.Backend.Database (DB)
import Refine.Backend.Server
import Refine.Common.Rest
import Refine.Common.Types


-- * machine room

runWai :: Backend -> Wai.WaiSession a -> IO a
runWai sess = Wai.withApplication (backendServer sess)

-- | Call 'runWaiBody'' and crash if the expected type cannot be read.
runWaiBody :: FromJSON a => Backend -> Wai.WaiSession SResponse -> IO a
runWaiBody sess = crashOnLeft . runWaiBody' sess

-- | Run a rest call and parse the body.
runWaiBody' :: FromJSON a => Backend -> Wai.WaiSession SResponse -> IO (Either String a)
runWaiBody' sess m = do
  resp <- Wai.withApplication (backendServer sess) m
  pure $ case eitherDecode $ simpleBody resp of
    Left err -> Left $ show err <> cs (simpleBody resp)
    Right x  -> Right x

-- | Call 'runDB'' and crash on 'Left'.
runDB :: Backend -> App DB a -> IO a
runDB sess = crashOnLeft . runDB' sess

-- | Call an 'App' action.
runDB' :: Backend -> App DB a -> IO (Either AppError a)
runDB' sess = runExceptT . run (backendMonad sess)

crashOnLeft :: Show e => IO (Either e a) -> IO a
crashOnLeft action = either (throwIO . ErrorCall . show) pure =<< action

createTestSession :: ActionWith Backend -> IO ()
createTestSession action = withTempCurrentDirectory $ do
  void $ action =<< mkBackend (defaultBackendConfig {backendShouldLog = False})


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


-- * test cases

spec :: Spec
spec = around createTestSession $ do

  describe "sListVDocs" $ do
    it "returns a vdocs list with HTTP status 200" $ \sess -> do
      resp :: SResponse <- runWai sess $ get "/r/vdocs"
      respCode resp `shouldBe` 200

    it "yields the same vdocs list as the db" $ \sess -> do
      vdocsRest :: SResponse <- runWai sess $ get "/r/vdocs"
      vdocsDB   :: [ID VDoc] <- runDB  sess   listVDocs
      decode (simpleBody vdocsRest) `shouldBe` Just vdocsDB

    it "if a vdoc is created, it shows in the output" $ \sess -> do
      bef <- runWai sess $ get "/r/vdocs"
      _   <- runDB  sess $ App.createVDoc sampleCreateVDoc
      aft <- runWai sess $ get "/r/vdocs"
      let check :: SResponse -> Maybe Int
          check resp = length <$> (decode (simpleBody resp) :: Maybe [ID VDoc])
      check aft `shouldBe` (+1) <$> check bef

  describe "sGetVDoc" $ do
    it "retrieves a vdoc" $ \sess -> do
      vdoc <- runDB sess $ App.createVDoc sampleCreateVDoc
      let vid :: ST = vdoc ^. vdocID . to toUrlPiece
      resp <- runWai sess $ get ("/r/vdoc/" <> cs vid)
      respCode resp `shouldBe` 200

  describe "sCreateVDoc" $ do
    it "stores a vdoc in the db" $ \sess -> do
      fe :: CompositeVDoc <- runWaiBody sess $ postJSON "/r/vdoc" sampleCreateVDoc
      be :: CompositeVDoc <- runDB      sess $ getCompositeVDoc (fe ^. heavyVDoc . vdocID)
      fe `shouldBe` be

  describe "sAddComment" $ do
    it "..." $ \_sess -> do
      pending

  describe "sAddPatch" $ do
    it "..." $ \_sess -> do
      pending
