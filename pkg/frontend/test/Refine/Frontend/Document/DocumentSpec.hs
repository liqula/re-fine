{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Frontend.Prelude hiding (property)

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
import Refine.Frontend.Store
import Refine.Frontend.Store.Types
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.ThirdPartyViews
import Refine.Frontend.Test.Store


spec :: Spec
spec = do
  describe "Samples" $ do
    it "work" $ do
      (rawContentFromVDocVersion sampleVDocVersion ^. rawContentBlocks) `shouldNotBe` []

  describe "convertToRaw, convertFromRaw" $ do
    it "are isomorphic" . property $ \(sanitizeRawContent -> rawContent) -> do
      let f = convertToRaw . convertFromRaw
      sanitizeRawContent (f rawContent) `shouldBe` sanitizeRawContent rawContent

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


    describe "componentDidMount" $ do
      let test :: HasCallStack => Expectation
          test = do
            reactFluxWorkAroundThreadDelay 0.5
            resetState (emptyGlobalState & gsDevState .~ Just (DevState []))
            -- FIXME: without the call to 'reactFluxWorkAroundThreadDelay' above, this fails.  why?!
            -- FIXME: resetState here is taking more than 0.1 seconds to stabilize.  why?!
            -- (both these are left-overs from when this was still a test on the mark_ component.  try again!)

            storeShouldEventuallyBe (^?! gsDevState . _Just . devStateTrace) []

            -- let cvdoc = ...
            -- executeAction $ OpenDocument cvdoc
            -- _ <- mount $ document_ (... cvdoc)
            -- if that works, try skipping the delay above.  or reducing it.

            pending
            storeShouldEventuallyContain (^?! gsDevState . _Just . devStateTrace)
              [ContributionAction (SetMarkPositions [(ContribIDNote (ID 77), MarkPosition 0 0)])]

      it "dispatches SetMarkPositions only once" test


    describe "mouse-over" $ do
      context "no mouse-over" $ do
        it "does not highlight anything" $ do
          wrapper <- shallow $ document_ emptyDocumentProps
          is wrapper (StringSelector ".o-mark--hover") `shouldReturn` False

      context "mouse-over on mark" $ do
        it "highlights the correct mark" $ do
          pending

        it "highlights one associated bubble" $ do
          pending

        it "highlights two associated bubbles" $ do
          pending

        it "does not highlight the other marks" $ do
          pending

        it "does not highlight other bubbles" $ do
          pending

      context "mouse-over on bubble" $ do
        it "highlights the bubble" $ do
          pending

        it "highlights associated mark" $ do
          pending

        it "highlights associated bubble" $ do
          pending

        it "does not highlight the other marks" $ do
          pending

        it "does not highlight other bubbles" $ do
          pending

      it "inserts the id of the current mark into the state on mouseEnter and removes it again on mouseLeave" $ do
        pending

        {- the old test on rfMark_ looked like this:
        pendingWith "fails very sporadically"

        wrapper <- mount $ rfMark_ theProps mempty
        _ <- simulate wrapper MouseEnter
        storeShouldEventuallyBe (^. gsContributionState . csHighlightedMarkAndBubble) $ Just (cnid 77)
        _ <- simulate wrapper MouseLeave
        storeShouldEventuallyBe (^. gsContributionState . csHighlightedMarkAndBubble) Nothing
        -}


-- * helpers

foreign import javascript unsafe
    "refine_test$testConvertFromToRaw($1)"
    js_testConvertFromToRaw :: JSString -> Bool
