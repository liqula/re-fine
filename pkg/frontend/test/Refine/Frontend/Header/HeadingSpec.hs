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

import           Control.Concurrent.MVar
import           Control.Lens ((^.))
import qualified Data.Map.Strict as M
import qualified Data.Tree as DT
import           React.Flux
import           Test.Hspec
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Frontend.Header.Heading
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.ThirdPartyViews (stickyContainer_)


spec :: Spec
spec = do
  describe "The topMenuBar_ component" $ do
    context "not sticky" $ do
      it "renders its elements" $ do
        wrapper <- shallow (topMenuBar_ (TopMenuBarProps False UserLoggedOut))
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu")) `shouldReturn` (1 :: Int)
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__menu-button")) `shouldReturn` (1 :: Int)
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__icon-bar")) `shouldReturn` (3 :: Int)
        label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
        lengthOf label `shouldReturn` (1 :: Int)
        text label `shouldReturn` "MENU"

      it "does not render with sticky css class" $ do
        wrapper <- shallow (topMenuBar_ (TopMenuBarProps False UserLoggedOut))
        label <- find wrapper (StringSelector ".c-mainmenu--toolbar-combined")  -- (it's called combined, though, not sticky)
        lengthOf label `shouldReturn` (0 :: Int)

    context "sticky" $ do
      it "does not render the label" $ do
        wrapper <- shallow (topMenuBar_ (TopMenuBarProps True UserLoggedOut))
        label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
        lengthOf label `shouldReturn` (0 :: Int)

      it "renders with sticky css class" $ do
        wrapper <- shallow (topMenuBar_ (TopMenuBarProps True UserLoggedOut))
        label <- find wrapper (StringSelector ".c-mainmenu--toolbar-combined")  -- (it's called combined, though, not sticky)
        lengthOf label `shouldReturn` (1 :: Int)

  describe "The mainHeader_ component" $ do
    it "sets the header height to a nonzero value" $ do
      pendingWith "#201, #221"

      let newVDoc = CompositeVDoc (VDoc (ID 1) (Title "the-title") (Abstract "the-abstract") (ID 1))
                                  (VDocRepo (ID 1) (ID 1))
                                  (ID 1)
                                  (VDocVersion [DT.Node (HTMLP.TagOpen "div" [HTMLP.Attr "data-offset" "0", HTMLP.Attr "data-uid" "77"]) []])
                                  M.empty M.empty M.empty
      _wrapper <- mount (stickyContainer_ [] . mainHeader_ $ emptyGlobalState { _gsVDoc = Just newVDoc })

      lock <- newEmptyMVar
      reactFluxWorkAroundForkIO $ do
        globalState0 <- readStoreData @GlobalState
        (globalState0 ^. gsScreenState . ssHeaderHeight) `shouldSatisfy` (> 0)
        putMVar lock ()
      () <- takeMVar lock
      pure ()
