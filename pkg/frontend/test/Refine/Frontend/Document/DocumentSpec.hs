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

import Control.Lens ((^.), (&), (.~))
import Data.Aeson
import Data.String.Conversions
import GHCJS.Types
import Test.Hspec
import Test.QuickCheck

import Refine.Common.Test.Arbitrary
import Refine.Common.Test.Samples
import Refine.Common.Types
import Refine.Common.VDoc.Draft
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.Document
import Refine.Frontend.Document.FFI
import Refine.Frontend.Document.Store
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.ThirdPartyViews


spec :: Spec
spec = do
  describe "Samples" $ do
    it "work" $ do
      (rawContentFromVDocVersion sampleVDocVersion ^. rawContentBlocks) `shouldNotBe` []

  describe "convertToRaw, convertFromRaw" $ do
    it "are isomorphic" . property $ \(sanitizeRawContent -> rawContent) -> do
      -- because entity ranges are canonicalized, we make the test slightly weaker.  see regression
      -- tests below for details.
      let f = convertToRaw . convertFromRaw
          f' = f . f
      f' rawContent `shouldBe` f rawContent

    it "regression.4" $ do
      let rawContent = mkRawContent [mkBlock "rF.." & blockEntityRanges .~
              [ (EntityLink "http://www.example.com", (0,1))
              , (EntityLink "http://www.example.com", (1,1))
              , (EntityLink "http://www.example.com", (2,1))
              , (EntityLink "http://www.example.com", (3,1))
              ]]
          rawContent' = (resetBlockKeys . convertToRaw . convertFromRaw) rawContent
      head (rawContent' ^. rawContentBlocks) ^. blockEntityRanges `shouldBe` [(EntityKey {_unEntityKey = 0},(0,4))]

    it "regression.3" $ do
      let rawContent = mkRawContent [mkBlock "rF" & blockEntityRanges .~ [(EntityLink "http://www.example.com", (0,2))]]
          rawContent' = (resetBlockKeys . convertToRaw . convertFromRaw) rawContent
      rawContent' `shouldBe` rawContent

    it "regression.2" $ do
      let rawContent = mkRawContent [mkBlock "rF" & blockStyles .~ [((0,1),Italic),((1,1),Italic),((1,1),Italic),((0,1),Bold)]]
          rawContent' = (resetBlockKeys . convertToRaw . convertFromRaw) rawContent
      head (rawContent' ^. rawContentBlocks) ^. blockStyles `shouldBe` [((0,2),Italic),((0,1),Bold)]

    it "regression.1" $ do
      let rawContent = mkRawContent [mkBlock "rF" & blockStyles .~ [((0,1),Italic)]]
      decode (encode rawContent) `shouldBe` Just rawContent
      (resetBlockKeys . convertToRaw . convertFromRaw) rawContent `shouldBe` rawContent


  describe "Draft" $ do
    it "editor_ mounts" $ do
      let doc :: String = "1243/asdf_#$%^"
      wrapper <- mount $ editor_ (defaultEditorProps doc) mempty
      contents :: String <- cs <$> html wrapper
      contents `shouldContain` "class=\"public-DraftEditor-content\""
      contents `shouldContain` doc


  describe "Document" $ do
    let mkTestProps :: RawContent -> DocumentProps
        mkTestProps c = DocumentProps
          (DocumentStateEdit (editorStateFromVDocVersion $ rawContentToVDocVersion c) Grammar)
          emptyContributionState
          EditToolbarExtension

    it "renders with empty content" $ do
      wrapper <- shallow $ document_ (mkTestProps emptyRawContent)
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` 1

    it "renders with arbitrary content" . property $ \rawContent -> do
      wrapper <- shallow $ document_ (mkTestProps rawContent)
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` 1

    it "document_ mounts" $ do
      let rawContent = RawContent [Block "asdf_1234-#$!&" [] [] NormalText 0 (Just (BlockKey "0"))] mempty
      wrapper <- mount $ document_ (mkTestProps rawContent)
      contents :: String <- cs <$> html wrapper
      contents `shouldContain` "<article "

    it "marks overlapping contribution ranges correctly" $ do
      pending
      -- select df_12
      -- mark as bold
      -- select 1234
      -- mark as italic
      -- retrieve rawcontent and check inline styles against literal.
      -- see also: https://github.com/facebook/draft-js/issues/325#issuecomment-273915121


foreign import javascript unsafe
    "refine_test$testConvertFromToRaw($1)"
    js_testConvertFromToRaw :: JSString -> Bool
