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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.Test.AppServer where

import Refine.Backend.Prelude hiding (Header)

import           Control.Concurrent.MVar
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import           Network.HTTP.Types (Method, Header, methodGet, methodPut, methodDelete)
import           Network.HTTP.Types.Status (Status(statusCode))
import           Network.URI (URI, uriToString)
import           Network.Wai (requestMethod, requestHeaders, defaultRequest)
import           Network.Wai.Test (SRequest(..), SResponse(..))
import qualified Network.Wai.Test as Wai
import qualified Network.Wai.Test.Internal as Wai
import qualified Network.Wai.Session as SCS
import           Web.Cookie (setCookieValue)

import           Refine.Backend.App hiding (getEdit)
import           Refine.Backend.Config
import           Refine.Backend.Database (DB)
import           Refine.Backend.Server
import           Refine.Backend.Test.Util (withTempCurrentDirectory)
import           Refine.Common.Rest
import           Refine.Common.Types as Common

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- | This type carries a 'Backend' (which contains of an 'AppM' and an 'Application'), plus a wai
-- test client.  (It is only used in this module, so it does not need to go to
-- "Refine.Backend.Test.AppRunner", where an 'AppM' tester is provided.)
--
-- FUTUREWORK: actually, i think that "Refine.Backend.Test.AppRunner" and the code here could
-- benefit from some refactoring to reduce code duplication.
data TestBackend = TestBackend
  { _testBackend            :: Backend DB
  , _testBackendState       :: MVar Wai.ClientState
  }

makeLenses ''TestBackend

mkTestBackend :: Config -> IO (TestBackend, IO ())
mkTestBackend cfg = do
  (be, dstr) <- mkProdBackend cfg
  sess <- TestBackend be <$> newMVar Wai.initState
  pure (sess, dstr)

-- | get a session cookie from the server (response does not matter otherwise).
testBackendFetchCookie :: TestBackend -> IO ()
testBackendFetchCookie sess = void . runWai sess . wget $ "/r/"

-- | Create session via 'mkProdBackend'.  Note that there is no network listener and 'WarpSettings'
-- are meaningless; the session only creates an 'AppM' runner and an 'Application'.
createTestSession :: (TestBackend -> IO ()) -> IO ()
createTestSession action = withTempCurrentDirectory $ do
  let cfg = def
        & cfgLogger     .~ LogCfg (LogCfgFile testLogfilePath) LogDebug
        & cfgDBKind     .~ DBOnDisk "test.db"
        & cfgSmtp       .~ Nothing
        & cfgAllAreGods .~ True
  (sess, destroy) <- mkTestBackend cfg
  () <- action sess
  destroy

createTestSessionWith :: (TestBackend -> IO ()) -> (TestBackend -> IO ()) -> IO ()
createTestSessionWith initAction action = createTestSession $ \sess -> do
  () <- initAction sess
  () <- action sess
  pure ()

runWai :: TestBackend -> Wai.Session a -> IO a
runWai sess m = do
  st <- takeMVar (sess ^. testBackendState)
  (a, st') <- Wai.runSessionWith st m (backendServer (sess ^. testBackend))
  putMVar (sess ^. testBackendState) st'
  pure a

-- | Call 'runWai' and throw an error if the expected type cannot be read, discard the
-- response, keep only the body.
runWaiJSON :: FromJSON a => TestBackend -> Wai.Session SResponse -> IO a
runWaiJSON sess m = do
  resp <- runWai sess m
  case eitherDecode $ simpleBody resp of
    Left err -> throwIO . ErrorCall $ unwords [show err, show (simpleHeaders resp), cs (simpleBody resp)]
    Right x  -> pure x

-- | Call 'runDB'' and crash on 'Left'.
runDB :: TestBackend -> AppM DB a -> IO a
runDB sess = errorOnLeft . runDB' sess

-- | Call an 'App' action.
--
-- FIXME: This does not share session state with the rest api
-- interface in @backendServer (sess ^. testBackend)@.  the functions
-- to establish that are *almost* done, but to finish them we need to
-- understand how servant-cookie-session uses vault to store data in
-- cookies, and either emulate that or (preferably) call the
-- resp. functions.  If this is resolved, '_testBackendCurrentUser'
-- won't be needed any more.
runDB' :: TestBackend -> AppM DB a -> IO (Either AppError a)
runDB' sess action = runExceptT . unwrapNT (backendRunApp (sess ^. testBackend)) $ do
    putAppState *> action <* getAppState
  where
    putAppState :: AppM DB ()
    putAppState = do
      co <- liftIO $ readMVar (sess ^. testBackendState)
      ms <- liftIO $ lookupCookie (sess ^. testBackend . to backendSessionStore) co
      put `mapM_` ms
      where
        lookupCookie :: SCS.SessionStore IO () (AppState, MVar ()) -> Wai.ClientState -> IO (Maybe AppState)
        lookupCookie store (Wai.ClientState (Map.toList -> cookies)) = case cookies of
          [(_, cookie)] -> (store . Just . setCookieValue $ cookie) >>= \((lkup, _), _) -> fst <$$> lkup ()
          bad -> error $ "runDB': bad session state: " <> show bad
          -- you need to call 'runWai' to initialize session before running runDB'.

    getAppState :: AppM DB ()
    getAppState = do
      co <- liftIO $ readMVar (sess ^. testBackendState)
      liftIO . updateCookie (sess ^. testBackend . to backendSessionStore) co =<< get
      where
        updateCookie :: SCS.SessionStore IO () (AppState, MVar ()) -> Wai.ClientState -> AppState -> IO ()
        updateCookie store (Wai.ClientState (Map.toList -> cookies)) appState = case cookies of
          [(_, cookie)] -> (store . Just . setCookieValue $ cookie) >>= \((lkup, upd), _) -> do
            lkup () >>= \case
              Just (_, lock) -> do
                () <- takeMVar lock
                upd () (appState, lock)
                putMVar lock ()
              Nothing ->
                error "runDB': this should not happen, but it does!"
          bad -> error $ "runDB': bad session state: " <> show bad

errorOnLeft :: Show e => IO (Either e a) -> IO a
errorOnLeft action = either (throwIO . ErrorCall . show') pure =<< action
  where
    show' x = "errorOnLeft: " <> show x

testLogfilePath :: FilePath
testLogfilePath = "logfile"

readTestLogfile :: IO String
readTestLogfile = readFile testLogfilePath


-- * test helpers

-- (see also: https://hackage.haskell.org/package/wai-extra/docs/Network-Wai-Test.html)

respCode :: SResponse -> Int
respCode = statusCode . simpleStatus

wget :: SBS -> Wai.Session SResponse
wget path = request methodGet path [] ""

wput :: SBS -> Wai.Session SResponse
wput path = request methodPut path [] ""

wdel :: SBS -> Wai.Session SResponse
wdel path = request methodDelete path [] ""

post :: (ToJSON a) => SBS -> a -> Wai.Session SResponse
post path js = request "POST" path [("Content-Type", "application/json")] (encode js)

putJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> a -> Wai.Session b
putJSON = rqJSON "PUT"

postJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> a -> Wai.Session b
postJSON = rqJSON "POST"

rqJSON :: forall a b . (Typeable a, ToJSON a, FromJSON b) => SBS -> SBS -> a -> Wai.Session b
rqJSON method path js = do
  resp <- request method path [("Content-Type", "application/json")] (encode js)
  liftIO $ case eitherDecode $ simpleBody resp of
    Left err -> throwIO . ErrorCall $ unlines [cs path, show (typeOf js), show resp, show err]
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

getVDocUri :: ID VDoc -> SBS
getVDocUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetVDocSimple)

createVDocUri :: SBS
createVDocUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SCreateVDoc)

updateVDocUri :: ID VDoc -> SBS
updateVDocUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SUpdateVDoc)

addEditUri :: ID Edit -> SBS
addEditUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddEdit)

updateEditUri :: ID Edit -> SBS
updateEditUri = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SUpdateEdit)

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

changeRoleUri :: SBS
changeRoleUri = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SChangeRole)

putVoteUri :: ID Edit -> Vote -> SBS
putVoteUri eid v = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SPutSimpleVoteOnEdit) eid v

deleteVoteUri :: ID Edit ->  SBS
deleteVoteUri eid = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SDeleteSimpleVoteOnEdit) eid

getVotesUri :: ID Edit -> SBS
getVotesUri eid = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetSimpleVotesOnEdit) eid

getAddGroup :: SBS
getAddGroup = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SAddGroup)

getGetGroup :: ID Group -> SBS
getGetGroup = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetGroup)

getUpdateGroup :: ID Group -> SBS
getUpdateGroup = uriStr . safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SUpdateGroup)

getGetGroups :: SBS
getGetGroups = uriStr $ safeLink (Proxy :: Proxy RefineAPI) (Proxy :: Proxy SGetGroups)


-- * sample data

sampleCreateVDoc :: CreateVDoc
sampleCreateVDoc = CreateVDoc
  (Title "[title]")
  (Abstract "[abstract]")
  sampleCreateVDocE0
  defaultGroupID

sampleCreateVDocE0 :: RawContent
sampleCreateVDocE0 = mkRawContent $ mkBlock "[versioned content]" :| []

sampleCreateVDocE1 :: RawContent
sampleCreateVDocE1 = mkRawContent $ mkBlock "[versioned content, edit1]" :| []

sampleCreateVDocE2 :: RawContent
sampleCreateVDocE2 = mkRawContent $ mkBlock "[versioned, edit2, content]" :| []

mkCVDoc :: TestBackend -> CreateVDoc -> IO VDoc
mkCVDoc sess vdoc = runWai sess $ postJSON createVDocUri vdoc

mkEdit :: TestBackend -> IO (ID Edit)
mkEdit = fmap (^. vdocHeadEdit) . (`mkCVDoc` sampleCreateVDoc)


testUsername :: Username
testUsername = "testUsername"

testUserEmail :: Email
testUserEmail = "testUsername@email.com"

testPassword :: Password
testPassword = "testPassword"

addUserAndLogin :: TestBackend -> Username -> Email -> Password -> IO (ID User)
addUserAndLogin sess username useremail userpass = runDB sess $ do
  _ <- unsafeBeAGod *> createUser (CreateUser username useremail userpass) <* beAMortal
  (^. userID) <$> login (Login username userpass)

addTestUserAndLogin :: TestBackend -> IO ()
addTestUserAndLogin sess = void $ addUserAndLogin sess testUsername testUserEmail testPassword

mkTestUserAndEditAndLogin :: TestBackend -> IO (ID Edit)
mkTestUserAndEditAndLogin sess = addTestUserAndLogin sess >> mkEdit sess

-- | we're just hoping this is the ID of the default group that is created in 'mkProdBackend'.  if
-- this fails, we need to be smarter about constructing the test cases here.
defaultGroupID :: ID Group
defaultGroupID = ID 1
