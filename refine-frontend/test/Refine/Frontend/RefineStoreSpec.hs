{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}

module Refine.Frontend.RefineStoreSpec where

import qualified Data.Map.Strict as M
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances ()

import           React.Flux (transform)

import Refine.Common.VDoc
import Refine.Frontend.RefineStore

instance Arbitrary (AUID VDoc) where
    arbitrary = AUID <$> arbitrary

instance Arbitrary VDocListItem where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        pure VDocListItem
                    { _vdliVDoc = a
                    , _vdliVDocTitle = b
                    }


emptyState :: RefineState
emptyState = RefineState Nothing Nothing 0 (MarkPositions M.empty) Desktop

spec :: Spec
spec = do
  describe "RefineStore" $ do
    describe "transform" $ do
      it "integrates the loaded document list into the store" . property $
        \list -> monadicIO . run $ do
            result <- transform (LoadedDocumentList list) emptyState
            _vdocList result `shouldBe` Just list
