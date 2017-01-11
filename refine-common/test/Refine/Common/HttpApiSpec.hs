{-# LANGUAGE GADTs                      #-}
module Refine.Common.HttpApiSpec where

import Refine.Common.Types.Arbitrary()
import Test.Hspec
import Test.QuickCheck

import Data.Typeable

import Data.String.Conversions (cs)
import Refine.Common.Types.Prelude
import Web.HttpApiData



data HttpApiGen d where
  HttpApiGen :: (Show d, Typeable d, FromHttpApiData d, ToHttpApiData d) =>
    (d -> d -> Bool) -> Gen d -> (d -> [d]) -> HttpApiGen d

h :: (Eq d, Arbitrary d, Show d, Typeable d, FromHttpApiData d, ToHttpApiData d) => HttpApiGen d
h = HttpApiGen (==) arbitrary shrink

fromAndToHttpApiDataAreInverses :: HttpApiGen d -> Spec
fromAndToHttpApiDataAreInverses (HttpApiGen eq g s) =
    it (show $ typeOf g) . property . forAllShrink g s $ \httpApiData ->
        either (error . cs) (eq httpApiData) . parseUrlPiece $ toUrlPiece httpApiData

spec :: Spec
spec = do
  describe "FromHttpApiData and ToHttpApiData are inverses" .
    mapM_ fromAndToHttpApiDataAreInverses $
      [ h :: HttpApiGen (ID ())
      ]
