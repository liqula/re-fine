{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.StoreSpec where

import Control.Lens ((^.))
import qualified Data.Map.Strict as M
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances ()

import           React.Flux (transform)

import Refine.Common.Types
import Refine.Frontend.Types
import Refine.Frontend.Store ()


instance Arbitrary (ID VDoc) where
    arbitrary = ID <$> arbitrary


emptyRState :: RState
emptyRState = RState Nothing Nothing 0 (MarkPositions M.empty) Desktop (Nothing, Nothing)

spec :: Spec
spec = do
  describe "RefineStore" $ do
    describe "transform" $ do
      it "integrates the loaded document list into the store" . property $
        \list -> monadicIO . run $ do
            result <- transform (LoadedDocumentList list) emptyRState
            result ^. rsVDocList `shouldBe` Just list
