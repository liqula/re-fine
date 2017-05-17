{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Refine.Common.VDoc.OTSpec where

import Refine.Common.Prelude

import           Test.QuickCheck
import           Test.Hspec

import Refine.Common.OTSpec hiding (spec)
import Refine.Common.Test.Arbitrary
import Refine.Common.OT
import Refine.Common.VDoc.OT
import qualified Refine.Common.VDoc.Draft as Draft


-- do not insert more than 5 elems into an Entity set
instance HasEnoughInhabitants (Atom Entity) where numOfInhabitants _ = Just 5

instance GenEdit Draft.RawContent where
    genEdit d = map ERawContent <$> genEdit (rawContentToDoc d)

--------------------------------------------------------- tests

spec :: Spec
spec = parallel $ do

    -- if this take too long to run on a regular basis, just activate for debugging or deep-tests:
    -- runTest $ allTests @Draft.RawContent

    it "Doc <-> RawContent conversion" . property $ \d ->
      rawContentToDoc (docToRawContent d) `shouldBe` simplifyDoc d

    it "RawContent <-> Doc conversion" . property $ \d -> do
      let clear = sanitizeRawContent . Draft.resetBlockKeys
      (clear . docToRawContent . rawContentToDoc) d `shouldBe` clear d
