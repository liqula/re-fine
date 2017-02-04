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


module Refine.Frontend.HeadingSpec where

import Test.Hspec

import Refine.Common.Types
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.Heading


spec :: Spec
spec = do
  describe "The menuButton_ component" $ do
    it "renders its elements" $ do
      wrapper <- shallow menuButton_
      lengthOfIO (find wrapper (StringSelector ".c-mainmenu")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector ".c-mainmenu__menu-button")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector ".c-mainmenu__icon-bar")) `shouldReturn` (3 :: Int)
      label <- find wrapper (StringSelector ".c-mainmenu__menu-button-label")
      lengthOf label `shouldReturn` (1 :: Int)
      text label `shouldReturn` "MENU"


  describe "The documentHeader_ component" $ do
    it "renders its elements" $ do
      wrapper <- shallow $ documentHeader_ (DocumentHeaderProps (Title "title") (Abstract "abstract"))
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-header")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector "DocumentAbstract")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector "DocumentTitle")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector "Phases")) `shouldReturn` (1 :: Int)


  describe "The documentTitle_ component" $ do
    it "renders the title" $ do
      wrapper <- shallow $ documentTitle_ (Title "The Awesome Document Title")
      text wrapper `shouldReturn` "The Awesome Document Title"


  describe "The documentAbstract_ component" $ do
    it "renders the abstract" $ do
      wrapper <- shallow $ documentAbstract_ (Abstract "The informative document abstract.")
      text wrapper `shouldReturn` "The informative document abstract."


  describe "The phases_ component" $ do
    it "renders its phases" $ do
      wrapper <- shallow phases_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-header__phase")) `shouldReturn` (3 :: Int)

    it "has only one active phase" $ do
      wrapper <- shallow phases_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-header__phase--active")) `shouldReturn` (1 :: Int)


  describe "The editToolbar_ component" $ do
    it "renders an element with the toolbar class" $ do
      wrapper <- shallow editToolbar_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar")) `shouldReturn` (1 :: Int)

    it "contains two separators" $ do
      wrapper <- shallow editToolbar_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar__separator")) `shouldReturn` (2 :: Int)

    it "contains 5 normal icon buttons" $ do
      wrapper <- shallow editToolbar_
      lengthOfIO (find wrapper (StringSelector "IconButton")) `shouldReturn` (5 :: Int)

    it "contains 1 aligned icon button" $ do
      wrapper <- shallow editToolbar_
      lengthOfIO (find wrapper (StringSelector "IconButtonWithAlignment")) `shouldReturn` (1 :: Int)

  describe "The editToolbarExtension_ component" $ do
    it "renders an element with the toolbar extension class" $ do
      wrapper <- shallow editToolbarExtension_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (1 :: Int)

    it "contains a pointer element" $ do
      wrapper <- shallow editToolbarExtension_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension__pointer")) `shouldReturn` (1 :: Int)

    it "contains an annotation section with 2 normal icon buttons" $ do
      wrapper <- shallow editToolbarExtension_
      annotation <- find wrapper (StringSelector ".c-vdoc-toolbar-extension__annotation")
      lengthOf annotation `shouldReturn` (1 :: Int)
      lengthOfIO (find annotation (StringSelector "IconButton")) `shouldReturn` (2 :: Int)

    it "contains a modification section with 1 normal icon button" $ do
      wrapper <- shallow editToolbarExtension_
      modification <- find wrapper (StringSelector ".c-vdoc-toolbar-extension__modification")
      lengthOf modification `shouldReturn` (1 :: Int)
      lengthOfIO (find modification (StringSelector "IconButton")) `shouldReturn` (1 :: Int)


-- TODO how to test headerSizeCapture_? We want to mock js_getBoundingClientRect...
-- TODO also: we need mounting for this -> we need jsdom for this -> we need ghc-dom...
