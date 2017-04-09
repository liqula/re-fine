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

module Refine.Frontend.Header.ToolbarSpec where

import           Control.Lens((^.))
import           Test.Hspec
import           React.Flux (readStoreData)

import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types

spec :: Spec
spec = do
  describe "The toolbar_ component" $ do
    it "renders an element with the toolbar class" $ do
      wrapper <- shallow toolbar_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar")) `shouldReturn` (1 :: Int)

    it "contains two separators" $ do
      wrapper <- shallow toolbar_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar__separator")) `shouldReturn` (2 :: Int)

    it "contains 6 icon buttons" $ do
      wrapper <- shallow toolbar_
      lengthOfIO (find wrapper (StringSelector "IconButton")) `shouldReturn` (6 :: Int)

    it "toggles the visibility of the edit toolbar extension when the 'new comment' button is clicked" $ do
      dispatchAndExec $ ResetState emptyGlobalState
      wrapper <- mount toolbar_
      button <- find wrapper (StringSelector ".c-vdoc-toolbar__btn-add-annotation")

      pending

      _ <- simulate button Click
      globalState1 <- readStoreData @GlobalState
      globalState1 ^. gsHeaderState . hsToolbarExtensionStatus `shouldBe` CommentToolbarExtensionWithoutRange

      _ <- simulate button Click
      globalState2 <- readStoreData @GlobalState
      globalState2 ^. gsHeaderState . hsToolbarExtensionStatus `shouldBe` ToolbarExtensionClosed


  describe "The commentToolbarExtension_ component" $ do
    it "renders an element with the toolbar extension class" $ do
      wrapper <- shallow . commentToolbarExtension_ $ CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (1 :: Int)

    it "contains a pointer element" $ do
      wrapper <- shallow . commentToolbarExtension_ $ CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension__pointer")) `shouldReturn` (1 :: Int)

    it "contains an annotation section with 2 normal icon buttons" $ do
      wrapper <- shallow . commentToolbarExtension_ $ CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange
      annotation <- find wrapper (StringSelector ".c-vdoc-toolbar-extension__annotation")
      lengthOf annotation `shouldReturn` (1 :: Int)
      lengthOfIO (find annotation (StringSelector "IconButton")) `shouldReturn` (2 :: Int)

  describe "The editToolbarExtension_ component" $ do
    it "renders an element with the toolbar extension class" $ do
      pendingWith "we need to enable the editToolbarExtension first"
      wrapper <- shallow . editToolbarExtension_ $ EditToolbarExtensionProps EditToolbarExtension
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (1 :: Int)

    it "contains a pointer element" $ do
      pendingWith "we need to enable the editToolbarExtension first"
      wrapper <- shallow . editToolbarExtension_ $ EditToolbarExtensionProps EditToolbarExtension
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension__pointer")) `shouldReturn` (1 :: Int)

    it "contains a modification section with 1 normal icon button" $ do
      pendingWith "we need to enable the editToolbarExtension first"
      wrapper <- shallow . editToolbarExtension_ $ EditToolbarExtensionProps EditToolbarExtension
      modification <- find wrapper (StringSelector ".c-vdoc-toolbar-extension__modification")
      lengthOf modification `shouldReturn` (1 :: Int)
      lengthOfIO (find modification (StringSelector "IconButton")) `shouldReturn` (1 :: Int)
