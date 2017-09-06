{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Test.HttpApiData where
#include "import_common.hs"

import Test.Hspec
import Test.QuickCheck

import Refine.Common.Test.Arbitrary ()


data HttpApiGen d where
  HttpApiGen :: (Show d, Typeable d, FromHttpApiData d, ToHttpApiData d) =>
    (d -> d -> Bool) -> Gen d -> (d -> [d]) -> HttpApiGen d

httpApiGen :: (Eq d, Arbitrary d, Show d, Typeable d, FromHttpApiData d, ToHttpApiData d) => HttpApiGen d
httpApiGen = HttpApiGen (==) arbitrary shrink

fromAndToHttpApiDataAreInverses :: HttpApiGen d -> Spec
fromAndToHttpApiDataAreInverses (HttpApiGen eq g s) =
    it (show $ typeOf g) . property . forAllShrink g s $ \httpApiData ->
        either (error . cs) (eq httpApiData) . parseUrlPiece $ toUrlPiece httpApiData
