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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.DraftSpec where

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.QuickCheck

import Refine.Common.Test.Arbitrary ()
import Refine.Common.Test.Samples ()  -- (just importing it so we know it compiles.)
import Refine.Common.Types
import Refine.Common.VDoc.Draft


spec :: Spec
spec = do
  describe "RawContent" $ do
    roundtripSpecs (Proxy @RawContent)

  describe "rawContentToVDocVersion, rawContentFromVDocVersion" $ do
    it "rawContentFromVDocVersion . rawContentToVDocVersion == id" . property $ \(rawContent :: RawContent) -> do
      rawContentFromVDocVersion (rawContentToVDocVersion rawContent) `shouldBe` rawContent

  describe "getMarkSelectors" $ do
    let cid0 = ContribIDNote (ID 13)
        cid1 = ContribIDNote (ID 35)
        block0 = BlockKey "0"
        block1 = BlockKey "1"

    it "works (1)" $ do
      let marks      = []
          want       = []
      getMarkSelectors marks `shouldBe` want

    it "works (2)" $ do
      let marks      = [(cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block0 4))]
          want       = [(cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 1)]
      getMarkSelectors marks `shouldBe` want

    it "works (2.5)" $ do
      let marks      = [(cid0, SelectionState False (SelectionPoint block0 0) (SelectionPoint block0 4))]
          want       = [(cid0, MarkSelector MarkSelectorTop block0 0, MarkSelector MarkSelectorBottom block0 0)]
      getMarkSelectors marks `shouldBe` want

    it "works (3)" $ do
      let marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block0 4))
                       , (cid1, SelectionState True (SelectionPoint block0 3) (SelectionPoint block0 7))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block0 2)
                       , (cid1, MarkSelector MarkSelectorTop block0 2, MarkSelector MarkSelectorBottom block0 3)
                       ]
      getMarkSelectors marks `shouldBe` want

    it "works (4)" $ do
      let marks      = [ (cid0, SelectionState False (SelectionPoint block0 2) (SelectionPoint block1 3))
                       , (cid1, SelectionState True (SelectionPoint block1 1) (SelectionPoint block1 2))
                       ]
          want       = [ (cid0, MarkSelector MarkSelectorTop block0 1, MarkSelector MarkSelectorBottom block1 2)
                       , (cid1, MarkSelector MarkSelectorTop block1 1, MarkSelector MarkSelectorBottom block1 1)
                       ]
      getMarkSelectors marks `shouldBe` want
