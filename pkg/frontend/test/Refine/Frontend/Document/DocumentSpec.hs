{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
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
      rawContentFromVDocVersion sampleVDocVersion `shouldNotBe` emptyRawContent


  describe "convertToRaw, convertFromRaw" $ do
    it "are isomorphic" . property $ \(sanitizeRawContent -> rawContent) -> do
      let f = convertToRaw . convertFromRaw
      sanitizeRawContent (f rawContent) `shouldBe` sanitizeRawContent rawContent

    let sanitizeRCHack = (rawContentBlocks %~ initBlockKeys) . convertToRaw . convertFromRaw

    it "regression.4" $ do
      let rawContent = mkRawContent . (:| []) $ mkBlock "rF.." & blockEntityRanges .~
              [ (EntityLink "http://www.example.com", EntityRange 0 1)
              , (EntityLink "http://www.example.com", EntityRange 1 1)
              , (EntityLink "http://www.example.com", EntityRange 2 1)
              , (EntityLink "http://www.example.com", EntityRange 3 1)
              ]
          rawContent' = sanitizeRCHack rawContent
      NEL.head (rawContent' ^. rawContentBlocks) ^. blockEntityRanges `shouldBe` [(EntityKey {_unEntityKey = 0}, EntityRange 0 4)]

    it "regression.3" $ do
      let rawContent = mkRawContent . (:| []) $  mkBlock "rF" & blockEntityRanges .~ [(EntityLink "http://www.example.com", EntityRange 0 2)]
          rawContent' = sanitizeRCHack rawContent
      rawContent' `shouldBe` rawContent

    it "regression.2" $ do
      let rawContent = mkRawContent . (:| []) $ mkBlock "rF" & blockStyles .~ [(EntityRange 0 1, Italic), (EntityRange 1 1,Italic), (EntityRange 1 1, Italic), (EntityRange 0 1, Bold)]
          rawContent' = sanitizeRCHack rawContent
      NEL.head (rawContent' ^. rawContentBlocks) ^. blockStyles `shouldBe` [(EntityRange 0 1, Bold), (EntityRange 0 2, Italic)]

    it "regression.1" $ do
      let rawContent = mkRawContent . (:| []) $ mkBlock "rF" & blockStyles .~ [(EntityRange 0 1, Italic)]
          rawContent' = sanitizeRCHack rawContent
      decode (encode rawContent) `shouldBe` Just rawContent
      rawContent' `shouldBe` rawContent


  describe "selectors" $ do
    describe "getEntitySelectors" $ do
      it "works" . property $ \(RawContentWithSelections _rc _sels) -> do
        pending

    describe "getMarkSelectors" $ do
      it "### works (between RawContent and DOM)" . property $ \rc -> do
        pending
        let msels :: [(ContributionID, MarkSelector, MarkSelector)]
            msels = getMarkSelectors rc

            RawContentSeparateStyles _ stys = separateStyles rc
            contribs = [ (cid, r)
                       | (Right (Mark cid), rs) <- Map.toList stys
                       , r <- unRanges rs]

        -- render the document component and retrieve the contents of the marks via the browser
        -- selection api (not draft/react, since 'MarkSelector' is referencing DOM nodes).

        _ <- mount $ document_ DocumentProps
          { _dpDocumentState     = mkDocumentStateView rc
          , _dpContributionState = emptyContributionState
          , _dpToolbarStatus     = ToolbarExtensionClosed
          }

        length msels `shouldBe` length contribs
        forM_ (zip (sort contribs) (sort msels)) $ \((cid, sel), (cid', beginpoint, endpoint)) -> do
          cid `shouldBe` cid'
          have <- js_getRawContentBetweenElems (cs $ renderMarkSelector beginpoint) (cs $ renderMarkSelector endpoint)
          cs have `shouldBe` selectionText BlockBoundaryIsNewline rc (styleRangeToSelectionState rc sel)


  describe "Draft" $ do
    it "editor_ mounts" $ do
      let doc :: String = "1243/asdf_#$%^"
      wrapper <- mount $ editor_ (defaultEditorProps doc) mempty
      contents :: String <- cs <$> html wrapper
      contents `shouldContain` "<div"
      contents `shouldContain` "public-DraftEditor-content"
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
      let rawContent = RawContent (Block "asdf_1234-#$!&" [] [] NormalText 0 (BlockKey "0") :| []) mempty
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

#ifdef __GHCJS__

foreign import javascript safe
    "refine_test$testConvertFromToRaw($1)"
    js_testConvertFromToRaw :: JSString -> Bool

foreign import javascript safe
    "refine$getRawContentBetweenElems($1, $2)"
    js_getRawContentBetweenElems :: JSString {- begin MarkSelector -} -> JSString {- end MarkSelector -} -> IO JSString

#else

{-# ANN js_testConvertFromToRaw ("HLint: ignore Use camelCase" :: String) #-}
js_testConvertFromToRaw :: JSString -> Bool
js_testConvertFromToRaw = error "javascript FFI not available in GHC"

{-# ANN js_getRawContentBetweenElems ("HLint: ignore Use camelCase" :: String) #-}
js_getRawContentBetweenElems :: JSString {- begin MarkSelector -} -> JSString {- end MarkSelector -} -> IO JSString
js_getRawContentBetweenElems = error "javascript FFI not available in GHC"

#endif
