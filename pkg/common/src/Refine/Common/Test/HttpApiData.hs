{-# LANGUAGE GADTs                     #-}

module Refine.Common.Test.HttpApiData where

import Data.String.Conversions (cs)
import Data.Typeable (Typeable, typeOf)
import Test.Hspec
import Test.QuickCheck
import Web.HttpApiData

import Refine.Common.Test.Arbitrary ()


data HttpApiGen d where
  HttpApiGen :: (Show d, Typeable d, FromHttpApiData d, ToHttpApiData d) =>
    (d -> d -> Bool) -> Gen d -> (d -> [d]) -> HttpApiGen d

h :: (Eq d, Arbitrary d, Show d, Typeable d, FromHttpApiData d, ToHttpApiData d) => HttpApiGen d
h = HttpApiGen (==) arbitrary shrink

fromAndToHttpApiDataAreInverses :: HttpApiGen d -> Spec
fromAndToHttpApiDataAreInverses (HttpApiGen eq g s) =
    it (show $ typeOf g) . property . forAllShrink g s $ \httpApiData ->
        either (error . cs) (eq httpApiData) . parseUrlPiece $ toUrlPiece httpApiData