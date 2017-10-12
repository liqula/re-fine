{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.StoreSpec where
#include "import_frontend.hs"

import Data.Aeson (encode)
import React.Flux (transform)
import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.Hspec
import Test.QuickCheck as QC
import Text.Read (readMaybe)

import Refine.Common.WebSocket
import Refine.Frontend.Access
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Store ()
import Refine.Frontend.Store.Types
import Refine.Frontend.Test.Arbitrary ()


relaxedRoundtripSpecs :: forall proxy a.
                         (HasCallStack, FromJSON a, ToJSON a, Arbitrary a, Show a, Typeable a)
                      => proxy a -> Spec
relaxedRoundtripSpecs _ =
  it (msg :: String) . QC.property $ \(v :: a) ->
                        Right (encode v) `shouldBe` (fmap encode . eitherDecode @a . encode) v
  where
    msg :: String = "instance FromJSON, ToJSON for " <> show (typeOf (undefined :: a)) <> " (ignoring NoJSONRep wholes)"


spec :: Spec
spec = do
  describe "Store" $ do
    describe "ToJSON, FromJSON" $ do
      roundtripSpecs (Proxy @AllVerticalSpanBounds)
      roundtripSpecs (Proxy @ScreenState)

      {- FIXME: the following don't work because of 'NoJSONRep' equality.  we would need to write a generic
         function that traverses a pair of values of an arbitrary type and runs equality on all
         nodes except for 'NoJSONRep' nodes (those are left unforced).  also, genServerCache needs
         to be set to "deep" (not "shallow"), and generate map values (not only keys).  this is very
         slow and probably has to be tuned before we can use it.

      describe "@SLOW" $ roundtripSpecs (Proxy @ServerCache)
      roundtripSpecs (Proxy @(PageState DocumentState))
      roundtripSpecs (Proxy @DevState)

      describe "@SLOW" $ relaxedRoundtripSpecs (Proxy @GlobalState)
      roundtripSpecs (Proxy @AccessState)

      -}

    describe "transform" $ do
      context "gsScreenState" $ do
        it "adds the header height to the state" $ do
          newState <- transform (ScreenAction (AddHeaderHeight 64)) emptyGlobalState
          _ssHeaderHeight (_gsScreenState newState) `shouldBe` (64 :: Int)


  describe "issue #242" $ do
    it "ffi throw an exception when marshalling Ints fails" $ do
      js_reproduce_issue_242            `shouldReturn` 123

    it "ffi throw an exception when marshalling Ints fails (+1)" $ do
      ((+1) <$> js_reproduce_issue_242) `shouldReturn` 124

    it "ffi throw an exception when marshalling Ints fails (show)" $ do
      (show <$> js_reproduce_issue_242) `shouldReturn` "123"

    it "ffi throw an exception when marshalling Ints fails (Aeson.encode)" $ do
      (encode <$> js_reproduce_issue_242) `shouldReturn` "123"

    it "readMaybe" $ do
      readMaybe "123.123" `shouldBe` (Nothing :: Maybe Int)

    it "read" $ do
      print (read "123.123" :: Int) `shouldThrow` anyException

#ifdef __GHCJS__

foreign import javascript safe
  "123.456"
  js_reproduce_issue_242 :: IO Int

#else

{-# ANN js_reproduce_issue_242 ("HLint: ignore Use camelCase" :: String) #-}
js_reproduce_issue_242 :: IO Int
js_reproduce_issue_242 = error "javascript FFI not available in GHC"

#endif
