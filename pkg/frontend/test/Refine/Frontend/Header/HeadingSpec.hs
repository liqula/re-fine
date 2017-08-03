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


module Refine.Frontend.Header.HeadingSpec where

import Refine.Frontend.Prelude

import           Control.Lens ((^.), (&), (.~))
import           Test.Hspec

import           Refine.Common.Test.Samples (sampleRawContent1, sampleMetaID)
import           Refine.Common.Types
import           Refine.Frontend.Header.Heading
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme as EZ
import           Refine.Frontend.Test.Store
import           Refine.Frontend.ThirdPartyViews (stickyContainer_)

spec :: Spec
spec = do
  describe "The topMenuBar_ component" $ do
    context "not sticky" $ do
      it "renders its elements" $ do
        wrapper <- mount (topMenuBar_ (TopMenuBarProps False UserLoggedOut))
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu")) `shouldReturn` 1
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__menu-button")) `shouldReturn` 1
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__icon-bar")) `shouldReturn` 3
        label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
        EZ.lengthOf label `shouldReturn` 1
        text label `shouldReturn` "MENU"

      it "does not render with sticky css class" $ do
        wrapper <- shallow (topMenuBar_ (TopMenuBarProps False UserLoggedOut))
        label <- find wrapper (StringSelector ".c-mainmenu--toolbar-combined")  -- (it's called combined, though, not sticky)
        EZ.lengthOf label `shouldReturn` 0

    context "sticky" $ do
      it "does not render the label" $ do
        wrapper <- shallow (topMenuBar_ (TopMenuBarProps True UserLoggedOut))
        label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
        EZ.lengthOf label `shouldReturn` 0

      it "renders with sticky css class" $ do
        wrapper <- shallow (topMenuBar_ (TopMenuBarProps True UserLoggedOut))
        label <- find wrapper (StringSelector ".c-mainmenu--toolbar-combined")  -- (it's called combined, though, not sticky)
        EZ.lengthOf label `shouldReturn` 1

  describe "The mainHeader_ component" $ do
    it "sets the header height to a nonzero value" $ do
      pendingWith "#201, #221"  -- (i actually think this may fail because we fail to handle actions in Enzyme.ReactWrapper.mount.)

      let newVDoc :: CompositeVDoc
          newVDoc = CompositeVDoc
            (VDoc sampleMetaID (Title "the-title") (Abstract "the-abstract") (ID 1) (ID 1))
            (Edit (MetaID 1 un) un un un (sampleMetaID ^. miID) sampleRawContent1 un mempty mempty mempty)
            mempty mempty mempty
            where
              un = undefined

          gs :: GlobalState
          gs = emptyGlobalState & gsVDoc .~ Just newVDoc

      resetState gs
      _wrapper <- mount (stickyContainer_ [] . mainHeader_ $ mkMainHeaderProps gs)
      storeShouldEventuallySatisfy ((^. gsScreenState . ssHeaderHeight) :: GlobalState -> Int) (> 0)
