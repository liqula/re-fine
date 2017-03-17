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
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad (void)
import           Control.Natural (run)
import           Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import qualified Data.ByteString as SBS
import           Data.Default (def)
import           Data.Proxy (Proxy(Proxy))
import           Data.Typeable (Typeable, typeOf)
import           Data.Map (elems)
import           Data.String.Conversions (SBS, LBS, cs, (<>))
import           Network.HTTP.Types.Status (Status(statusCode))
import           Network.HTTP.Types (Method, Header, methodGet)
import           Network.URI (URI, uriToString)
import           Network.Wai (requestMethod, requestHeaders, defaultRequest)
import           Network.Wai.Test (SRequest(..), SResponse(..))
import qualified Network.Wai.Test as Wai
import           Servant.Utils.Links (safeLink)
import           Test.Hspec

import Refine.Backend.App as App
import Refine.Backend.Config
import Refine.Backend.Database.Class as DB
import Refine.Backend.Database (DB)
import Refine.Backend.DocRepo as DocRepo
import Refine.Backend.Server
import Refine.Backend.Test.Util (withTempCurrentDirectory)
import Refine.Backend.User
import Refine.Common.Rest
import Refine.Common.Types as Common

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- * machine room

runWai :: Backend db uh -> Wai.Session a -> IO a
runWai sess m = Wai.runSession m (backendServer sess)

-- | Call 'runWaiBodyE' and throw an error if the expected type cannot be read, discard the
-- response, keep only the body.
runWaiBody :: FromJSON a => Backend db uh -> Wai.Session SResponse -> IO a
runWaiBody sess = fmap fst . runWaiBodyRsp sess

-- | Call 'runWaiBodyE' and throw an error if the expected type cannot be read.
runWaiBodyRsp :: FromJSON a => Backend db uh -> Wai.Session SResponse -> IO (a, SResponse)
runWaiBodyRsp sess = errorOnLeft . runWaiBodyE sess

-- | Run a rest call and parse the body.
runWaiBodyE :: FromJSON a => Backend db uh -> Wai.Session SResponse -> IO (Either String (a, SResponse))
runWaiBodyE sess m = do
  resp <- runWai sess m
  pure $ case eitherDecode $ simpleBody resp of
    Left err -> Left $ unwords [show err, show (simpleHeaders resp), cs (simpleBody resp)]
    Right x  -> Right (x, resp)

-- | Call 'runDB'' and crash on 'Left'.
runDB :: Backend DB uh -> AppM DB uh a -> IO a
runDB sess = errorOnLeft . runDB' sess

-- | Call an 'App' action.
runDB' :: Backend DB uh -> AppM DB uh a -> IO (Either AppError a)
runDB' sess = runExceptT . run (backendRunApp sess)

errorOnLeft :: Show e => IO (Either e a) -> IO a
errorOnLeft action = either (throwIO . ErrorCall . show) pure =<< action


createDevModeTestSession :: ActionWith (Backend DB FreeUH) -> IO ()
createDevModeTestSession action = withTempCurrentDirectory $ do
  void $ action =<< mkDevModeBackend (def & cfgShouldLog .~ False) mockLogin

createTestSession :: ActionWith (Backend DB UH) -> IO ()
createTestSession action = withTempCurrentDirectory $ do
  void $ action =<< mkProdBackend (def & cfgShouldLog .~ False)


-- * test helpers

-- (see also: https://hackage.haskell.org/package/wai-extra/docs/Network-Wai-Test.html)

sampleCreateVDoc :: CreateVDoc
sampleCreateVDoc = CreateVDoc
  (Title "[title]")
  (Abstract "[abstract]")
  (vdocVersionFromST "[versioned content]")

respCode :: SResponse -> Int
respCode = statusCode . simpleStatus

get :: SBS -> Wai.Session SResponse
get path = request methodGet path [] ""

post :: (ToJSON a) => SBS -> a -> Wai.Session SResponse
post path json = request "POST" path [("Content-Type", "application/json")] (encode json)

postJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> a -> Wai.Session b
postJSON path json = do
  resp <- request "POST" path [("Content-Type", "application/json")] (encode json)
  liftIO $ case eitherDecode $ simpleBody resp of
    Left err -> throwIO . ErrorCall $ unlines [cs path, show (typeOf json), show resp, show err]
    Right x  -> pure x

-- | This is from hspec-wai, but we modified it to work on 'Wai.Session' directly.  'WaiSession'
-- does not keep the cookies in the 'Session' between requests.
request :: Method -> SBS -> [Header] -> LBS -> Wai.Session SResponse
request method path headers body = Wai.srequest $ SRequest req body
  where
    req = Wai.setPath defaultRequest {requestMethod = method, requestHeaders = headers} path


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

createUserUri :: SBS
createUserUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SCreateUser)

loginUri :: SBS
loginUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SLogin)

logoutUri :: SBS
logoutUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SLogout)


-- * test cases

spec :: Spec
spec = do -- FUTUREWORK: mark this as 'parallel' (needs some work)
  specMockedLogin
  specUserHandling

specMockedLogin :: Spec
specMockedLogin = around createDevModeTestSession $ do
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
      fe :: CompositeVDoc <- runWaiBody sess $ post createVDocUri sampleCreateVDoc
      be :: CompositeVDoc <- runDB      sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
      fe `shouldBe` be

  describe "sAddNote" $ do
    it "stores note with full-document chunk range" $ \sess -> do
      runWai sess $ do
        un :: Username <- postJSON loginUri $ Login "username" "password"
        liftIO $ un `shouldBe` "username"
        fe :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc
        fn :: Note          <- postJSON
            (addNoteUri (fe ^. compositeVDocRepo . vdocHeadEdit))
            (CreateNote "[note]" True (ChunkRange Nothing Nothing))
        liftIO $ do
          be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
          be ^. compositeVDocNotes . to elems `shouldContain` [fn]

    it "stores note with non-trivial valid chunk range" $ \sess -> do
      runWai sess $ do
        un :: Username <- postJSON loginUri $ Login "username" "password"
        liftIO $ un `shouldBe` "username"
        fe :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc
        let cp1 = ChunkPoint (DataUID 1) 0
            cp2 = ChunkPoint (DataUID 1) 1
        fn :: Note <- postJSON
          (addNoteUri (fe ^. compositeVDocRepo . vdocHeadEdit))
          (CreateNote "[note]" True (ChunkRange (Just cp1) (Just cp2)))

        liftIO $ do
          be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
          be ^. compositeVDocNotes . to elems `shouldContain` [fn]

    it "fails with error on non-trivial *invalid* chunk range" $ \sess -> do
      vdoc :: CompositeVDoc <- runWaiBody sess $ post createVDocUri sampleCreateVDoc
      resp :: SResponse <- runWai sess $
        let cp1, cp2 :: ChunkPoint
            cp1 = ChunkPoint (DataUID 1) 0
            cp2 = ChunkPoint (DataUID 100) 100
        in post
          (addNoteUri (vdoc ^. compositeVDocRepo . vdocHeadEdit))
          (CreateNote "[note]" True (ChunkRange (Just cp1) (Just cp2)))

      respCode resp `shouldBe` 409
      cs (simpleBody resp) `shouldContain` ("ChunkRangeBadDataUID" :: String)

      vdoc' :: CompositeVDoc <- runDB sess $ getCompositeVDoc (vdoc ^. compositeVDoc . vdocID)
      vdoc' ^. compositeVDocNotes `shouldBe` mempty

  describe "sAddDiscussion" $ do
    it "stores discussion with no ranges" $ \sess -> do
      runWai sess $ do
        un :: Username <- postJSON loginUri $ Login "username" "password"
        liftIO $ un `shouldBe` "username"
        fe :: CompositeVDoc <- postJSON createVDocUri sampleCreateVDoc
        fn :: CompositeDiscussion <-
          postJSON
            (addDiscussionUri (fe ^. compositeVDocRepo . vdocHeadEdit))
            (CreateDiscussion "[discussion initial statement]" True (ChunkRange Nothing Nothing))

        liftIO $ do
          be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
          be ^. compositeVDocDiscussions . to elems `shouldContain` [fn]

  describe "sAddStatement" $ do
    it "stores statement for given discussion" $ \_sess -> do
      pendingWith "this test case shouldn't be too hard to write, and should be working already."

  describe "sAddEdit" $ do
    let setup sess = do
          fe :: CompositeVDoc <- runWaiBody sess $ post createVDocUri sampleCreateVDoc
          fp :: Edit          <- runWaiBody sess $
            post
              (addEditUri (fe ^. compositeVDocRepo . vdocHeadEdit))
              (CreateEdit
                "new edit"
                (ChunkRange Nothing Nothing)
                (vdocVersionFromST "[new vdoc version]")
                Grammar
                "no motivation")
          pure (fe, fp)

    context "on edit without ranges" $ do
      it "stores an edit and returns its version" $ \sess -> do
        (_, fp) <- setup sess
        be' :: VDocVersion 'HTMLCanonical <- runDB sess $ do
              handles <- db $ DB.handlesForEdit (fp ^. editID)
              docRepo $ uncurry DocRepo.getVersion handles
        be' `shouldBe` vdocVersionFromST "<span data-uid=\"1\">[new\nvdoc\nversion]</span>"

      it "stores an edit and returns it in the list of edits applicable to its base" $ \sess -> do
        pendingWith "applicableEdits is not implemented."
        (fe, fp) <- setup sess
        be :: CompositeVDoc <- runDB sess $ getCompositeVDoc (fe ^. compositeVDoc . vdocID)
        be ^. compositeVDocEdits . to elems`shouldContain` [fp]

specUserHandling :: Spec
specUserHandling = around createTestSession $ do
  describe "User handling" $ do
    let doCreate = post createUserUri (CreateUser userName "mail@email.com" userPass)
        doLogin = post loginUri
        doLogout = post logoutUri ()

        checkCookie resp = simpleHeaders resp `shouldSatisfy`
            any (\(k, v) -> k == "Set-Cookie" && refineCookieName `SBS.isPrefixOf` v)

        userName = "user"
        userPass = "password"

    describe "create" $ do
      it "works" $ \sess -> do
        runWaiBody sess doCreate `shouldReturn` User (ID 1)

      it "is secure" $ \_ -> do
        pendingWith "needs design & implementation: what makes a create requests legit?"

    describe "login" $ do
      context "with valid credentials" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogin (Login userName userPass)
          respCode resp `shouldBe` 200
          checkCookie resp

      context "with invalid credentials" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogin (Login userName "")
          respCode resp `shouldBe` 404
          checkCookie resp

    describe "logout" $ do
      context "logged in" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogin (Login userName userPass) >> doLogout
          respCode resp `shouldBe` 200
          checkCookie resp

      context "logged out" $ do
        it "works (and returns the cookie)" $ \sess -> do
          resp <- runWai sess $ doCreate >> doLogout
          respCode resp `shouldBe` 200
          checkCookie resp
