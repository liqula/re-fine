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
import Refine.Frontend.Test.Console
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.ThirdPartyViews


spec :: Spec
spec = do
  describe "Samples" $ do
    it "work" $ do
      (rawContentFromVDocVersion sampleVDocVersion ^. rawContentBlocks) `shouldNotBe` []

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
      let rawContent = RawContent
            { _rawContentBlocks =
                [ Block { _blockText = "rF"
                        , _blockEntityRanges = []
                        , _blockStyles = [((0,1),Italic),((1,1),Italic),((1,1),Italic),((0,1),Bold)]
                        , _blockType = NormalText
                        , _blockDepth = 0
                        , _blockKey = Nothing
                        }
                ]
            , _rawContentEntityMap = mempty
            }
      pending
      (resetBlockKeys . convertToRaw . convertFromRaw) rawContent `shouldBe` rawContent

    it "regression.1" $ do
      let rawContent = RawContent
            { _rawContentBlocks =
                [ Block { _blockText = "rF"
                        , _blockEntityRanges = []
                        , _blockStyles = [((0,1),Italic)]
                        , _blockType = NormalText
                        , _blockDepth = 0
                        , _blockKey = Nothing
                        }
                ]
            , _rawContentEntityMap = mempty
            }
      decode (encode rawContent) `shouldBe` Just rawContent                              -- passes
      pending
      js_testConvertFromToRaw (cs $ encode rawContent) `shouldBe` True                   -- fails!
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
