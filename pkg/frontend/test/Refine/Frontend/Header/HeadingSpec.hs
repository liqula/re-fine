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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Header.HeadingSpec where

import Test.Hspec

import Refine.Frontend.Test.Enzyme.ShallowWrapperAPI
import Refine.Frontend.Header.Heading


spec :: Spec
spec = do
  describe "The menuButton_ component" $ do
    context "not sticky" $ do
      it "renders its elements" $ do
        wrapper <- shallow (menuButton_ (MenuButtonProps False))
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu")) `shouldReturn` (1 :: Int)
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__menu-button")) `shouldReturn` (1 :: Int)
        lengthOfIO (find wrapper (StringSelector ".c-mainmenu__icon-bar")) `shouldReturn` (3 :: Int)
        label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
        lengthOf label `shouldReturn` (1 :: Int)
        text label `shouldReturn` "MENU"

      it "does not render with sticky css class" $ do
        wrapper <- shallow (menuButton_ (MenuButtonProps False))
        label <- find wrapper (StringSelector ".c-mainmenu--toolbar-combined")  -- (it's called combined, though, not sticky)
        lengthOf label `shouldReturn` (0 :: Int)

    context "sticky" $ do
      it "does not render the label" $ do
        wrapper <- shallow (menuButton_ (MenuButtonProps True))
        label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
        lengthOf label `shouldReturn` (0 :: Int)

      it "renders with sticky css class" $ do
        wrapper <- shallow (menuButton_ (MenuButtonProps True))
        label <- find wrapper (StringSelector ".c-mainmenu--toolbar-combined")  -- (it's called combined, though, not sticky)
        lengthOf label `shouldReturn` (1 :: Int)


-- TODO how to test headerSizeCapture_? We want to mock js_getBoundingClientRect...
-- TODO also: we need mounting for this -> we need jsdom for this -> we need ghc-dom...
