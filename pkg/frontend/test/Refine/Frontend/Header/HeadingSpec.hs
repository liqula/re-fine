{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Header.HeadingSpec where
#include "import_frontend.hs"

import           Test.Hspec

import           Refine.Common.Test.Samples (sampleRawContent1, sampleMetaID)
import           Refine.Common.Types
import           Refine.Frontend.Access
import           Refine.Frontend.Header.EditToolbar
import           Refine.Frontend.Header.Heading
import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Component
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme as EZ
import           Refine.Frontend.Test.Store


spec :: Spec
spec = do
  describe "The mainHeader_ component" $ do
    it "sets the header height to a nonzero value" $ do
      pendingWith "#201, #221"  -- (i actually think this may fail because we fail to handle actions in Enzyme.ReactWrapper.mount.)

      let newVDoc :: CompositeVDoc
          newVDoc = CompositeVDoc
            (VDoc sampleMetaID (Title "the-title") (Abstract "the-abstract") (ID 1) (ID 1) mempty Nothing)
            (Edit (MetaID 1 un) un un un (sampleMetaID ^. miID) sampleRawContent1 un mempty mempty)
            mempty
            mempty
            where
              un = undefined

          gs :: GlobalState
          gs = emptyGlobalState
             & gsPageState .~ PageStateVDoc (emptyProcessState 1)
             & gsServerCache %~ serverCacheUpdate (LoadVDoc (newVDoc ^. compositeVDoc . vdocID))

          as :: AccessState
          as = undefined

      resetState gs
      _wrapper <- mount (mainHeader_ $ mkMainHeaderProps emptyAccessState (wipeDocumentState as gs))
      storeShouldEventuallySatisfy ((^. gsScreenState . ssHeaderHeight) :: GlobalState -> Int) (> 0)
