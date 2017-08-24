{-# LANGUAGE CPP #-}
#include "language.hs"

module Main where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Either
import           Data.String.Conversions
import           Network.Wreq hiding (get, post, put, delete)
import qualified Network.Wreq as Wreq
import           System.Environment
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec
import           Text.Show.Pretty (ppShow)

import Refine.Common.Test.Samples
import Refine.Common.Types
import Refine.Common.VDoc.Draft
import Refine.Prelude


verbose :: Bool
verbose = True

rootUrl :: String
rootUrl = "http://localhost:3000/"

testCreateUser :: CreateUser
testCreateUser = CreateUser "a" "a@example.com" "a"

main :: IO ()
main = do
  -- edit this to your liking; use stuff from building blocks section below, or write your own.
  prepareSession
  runMakeVDoc

  -- FIXME: this won't work any more until we use websockets (the client side) instead of wreq.


-- * building blocks

-- http://www.serpentine.com/wreq/tutorial.html
-- http://hackage.haskell.org/package/wreq-0.5.0.1/docs/Network-Wreq.html

-- | create test user if it does not exist, login, and store the cookie for use in later requests.
-- this function is idempotent.
prepareSession :: IO ()
prepareSession = do
  void (post "r/user/create" testCreateUser :: IO (Response User)) `catch` \(_ :: SomeException) -> pure ()
  r :: Response User <- post "r/user/login" (case testCreateUser of CreateUser n _ p -> Login n p)
  modifyMVar cjarRef (\_ -> pure (r ^.. responseHeader "set-cookie", ()))

-- | this is probably too simple for when we have groups and processes, but for now it works.
runMakeVDoc :: IO ()
runMakeVDoc = do
  r :: Response CompositeVDoc
    <- post "r/vdoc" $ CreateVDoc sampleTitle sampleAbstract sampleRawContent1 defaultGroupID

  let eid = r ^. responseBody . compositeVDocThisEdit . editMetaID . miID . unID
      vdoc = r ^. responseBody . compositeVDocThisEdit . editVDocVersion
      rnge = minimumRange vdoc

  d :: Response Discussion
    <- post ("r/discussion/" <> show eid) (CreateDiscussion "this is my initial statement!" (Just rnge))

  print d  -- FIXME: create a small but non-trivial statement tree under d instead of printing it.

  pure ()


-- * helpers

-- | A global 'MVar' storing the cookie as received on login.  (there is probably a better way to do
-- this, something about the manager from http-client that can be passed set in the wreq interface.
-- but this way is easier!)
--
-- NOTE: I also tried using the `responseCookie` and `responseCookieJar` lenses provided by wreq,
-- but they came up empty, so we're just manipulating the HTTP headers by hand.
cjarRef :: MVar [SBS]
cjarRef = unsafePerformIO $ newMVar mempty
{-# NOINLINE cjarRef #-}

post :: (Show resp, FromJSON resp, ToJSON req) => String -> req -> IO (Response resp)
post url req = do
  cjar <- readMVar cjarRef
  when verbose $ print ("cookie: " :: String, cjar)
  let defaults' = defaults & header "cookie" .~ cjar
  resp <- asJSON =<< Wreq.postWith defaults' (rootUrl <> url) (toJSON req)
  dumpRsp resp
  pure resp

dumpRsp :: Show a => Response a -> IO ()
dumpRsp r = when verbose $ do
  putStrLn . ppShow $ r ^. responseStatus
  putStrLn . ppShow $ r ^. responseHeaders
  putStrLn . ppShow $ r ^. responseBody
  pure ()

_chrashUnless :: (HasCallStack, Show msg) => msg -> Bool -> IO ()
_chrashUnless _   True  = pure ()
_chrashUnless msg False = error $ "crash: " <> show msg
