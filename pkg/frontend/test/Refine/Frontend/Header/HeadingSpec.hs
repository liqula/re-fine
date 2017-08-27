{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Header.HeadingSpec where
#include "import_frontend.hs"

import           Test.Hspec

import           Refine.Common.Test.Samples (sampleRawContent1, sampleMetaID)
import           Refine.Common.Types
import           Refine.Frontend.Header.Heading
import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Access
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme as EZ
import           Refine.Frontend.Test.Store

spec :: Spec
spec = do
  describe "The topMenuBar_ component" $ do
    context "not sticky" $ do
      it "renders its elements" $ do
        wrapper <- mount (topMenuBar_ (TopMenuBarProps UserLoggedOut))
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu")) `shouldReturn` 1
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__menu-button")) `shouldReturn` 1
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__icon-bar")) `shouldReturn` 3
        label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
        EZ.lengthOf label `shouldReturn` 1
        text label `shouldReturn` "MENU"

    context "sticky" $ do
      let vdoc = VDoc sampleMetaID (Title "the-title") (Abstract "the-abstract") undefined undefined mempty

      it "the MENU item in the toolbar does not render if it is not sticky" $ do
        wrapper <- shallow (toolbar_ vdoc)
        label <- find wrapper (StringSelector ".c-toolbar-menu-label-visible")
        EZ.lengthOf label `shouldReturn` 0
        label' <- find wrapper (StringSelector ".c-toolbar-menu-label-hidden")
        EZ.lengthOf label' `shouldReturn` 1

      it "does render the MENU label in toolbar" $ do
        pending
        wrapper <- mount (toolbar_ vdoc)
        -- scroll down  -- FIXME
        label <- find wrapper (StringSelector ".c-toolbar-menu-label-visible")
        EZ.lengthOf label `shouldReturn` 1
        label' <- find wrapper (StringSelector ".c-toolbar-menu-label-hidden")
        EZ.lengthOf label' `shouldReturn` 0

  describe "The mainHeader_ component" $ do
    it "sets the header height to a nonzero value" $ do
      pendingWith "#201, #221"  -- (i actually think this may fail because we fail to handle actions in Enzyme.ReactWrapper.mount.)

      let _newVDoc :: CompositeVDoc
          _newVDoc = CompositeVDoc
            (VDoc sampleMetaID (Title "the-title") (Abstract "the-abstract") (ID 1) (ID 1) mempty)
            (Edit (MetaID 1 un) un un un (sampleMetaID ^. miID) sampleRawContent1 un mempty mempty)
            mempty
            mempty
            where
              un = undefined

          gs :: GlobalState
          gs = emptyGlobalState
             & gsEditID .~ Just 1
-- FIXME: remove this line if this test pass --    & gsServerCache %~ serverCacheUpdate (LoadVDoc (AfterAjax newVDoc))

      resetState gs
      _wrapper <- mount (mainHeader_ $ mkMainHeaderProps emptyAccessState gs)
      storeShouldEventuallySatisfy ((^. gsScreenState . ssHeaderHeight) :: GlobalState -> Int) (> 0)
