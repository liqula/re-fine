{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.TypesSpec where
#include "import_common.hs"

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import "quickcheck-instances" Test.QuickCheck.Instances ()

import Refine.Common.Test.HttpApiData
import Refine.Common.Types


spec :: Spec
spec = parallel $ do
  describe "FromHttpApiData and ToHttpApiData are inverses" $ do
    fromAndToHttpApiDataAreInverses (httpApiGen :: HttpApiGen (ID ()))

  roundtripSpecs (Proxy @(ID Int))
  roundtripSpecs (Proxy @(ID (Maybe Int)))

  -- test {From,To}JSONKey instances
  roundtripSpecs (Proxy @(Map (ID Int) Int))
  roundtripSpecs (Proxy @(Map (ID (Maybe Int)) Int))

  roundtripSpecs (Proxy @L10)
