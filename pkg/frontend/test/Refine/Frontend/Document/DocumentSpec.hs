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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Document.DocumentSpec
where

import Control.Lens ((^.))
import Test.Hspec
import Test.QuickCheck

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.Draft
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Document
import Refine.Frontend.Document.FFI
import Refine.Frontend.Document.Store
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Test.Console
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.Test.Samples


spec :: Spec
spec = do
  describe "Samples" $ do
    it "work" $ do
      (rawContentFromVDocVersion sampleText ^. rawContentBlocks) `shouldNotBe` []

  describe "convertToRaw, convertFromRaw" $ do
    it "are isomorphic" . property $ \(sanitizeRawContent -> rawContent) -> do
      pending
      consoleLogJSONM "**" (convertFromRaw rawContent)
      consoleLogJSONM "**" rawContent

      {-

      observations:

      - some things having ranges disappear, some are left intact.
      - both the removed ones and the intact ones had valid offset and length.
      - it affects both blockStyles and entityRanges.

      questions:

      - is there something wrong with ghcjs again?
      - is it about this: "Ignoring that the GHCJS boot package "aeson" has a different version, 1.1.1.0, than the resolver's wanted version, 1.0.2.1"
      - or with draft?

      -}

      (resetBlockKeys . convertToRaw . convertFromRaw) rawContent `shouldBe` resetBlockKeys rawContent

    it "regression.2" $ do
      pending
      let rawContent = RawContent {_rawContentBlocks = [Block {_blockText = "rF", _blockEntityRanges = [], _blockStyles = [((0,1),Italic),((1,1),Italic),((1,1),Italic),((0,1),Bold)], _blockType = NormalText, _blockDepth = 0, _blockKey = Nothing}], _rawContentEntityMap = mempty}
      (resetBlockKeys . convertToRaw . convertFromRaw) rawContent `shouldBe` rawContent

    it "regression.1" $ do
      pending
      let rawContent = RawContent {_rawContentBlocks = [Block {_blockText = "rF", _blockEntityRanges = [], _blockStyles = [((0,1),Italic)], _blockType = NormalText, _blockDepth = 0, _blockKey = Nothing}], _rawContentEntityMap = mempty}
      (resetBlockKeys . convertToRaw . convertFromRaw) rawContent `shouldBe` rawContent



  describe "Document" $ do
    let mkTestProps :: RawContent -> DocumentProps
        mkTestProps c = DocumentProps
          (DocumentStateEdit (editorStateFromVDocVersion $ rawContentToVDocVersion c) Grammar)
          emptyContributionState
          EditToolbarExtension

    it "renders with empty content" $ do
      pending
      wrapper <- shallow $ document_ (mkTestProps $ mkRawContent [])
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` 1

    it "renders with arbitrary content" . property $ \rawContent -> do
      pending
      wrapper <- shallow $ document_ (mkTestProps rawContent)
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` 1
