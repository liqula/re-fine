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

module Refine.Frontend.Header.ToolbarSpec where

import Refine.Frontend.Prelude

import           Test.Hspec

import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Test.Enzyme as EZ
import           Refine.Frontend.Test.Store
import           Refine.Frontend.Store.Types


spec :: Spec
spec = do
  describe "The toolbar_ component" $ do
    it "renders an element with the toolbar class" $ do
      wrapper <- shallow toolbar_
      EZ.lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar")) `shouldReturn` 1

    it "contains two separators" $ do
      wrapper <- shallow toolbar_
      EZ.lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar__separator")) `shouldReturn` 2

    it "contains 6 icon buttons" $ do
      wrapper <- shallow toolbar_
      EZ.lengthOfIO (find wrapper (StringSelector "IconButton")) `shouldReturn` 6

    it "toggles the visibility of the edit toolbar extension when the 'new comment' button is clicked" $ do
      pendingWith "sometimes fails, sometimes passes; related to #201 or #221?"

      wrapper <- mount toolbar_
      button <- find wrapper (StringSelector ".c-vdoc-toolbar__btn-add-annotation")

      _ <- simulate button Click
      storeShouldEventuallyBe (^. gsHeaderState . hsToolbarExtensionStatus) CommentToolbarExtensionWithoutRange

      _ <- simulate button Click
      storeShouldEventuallyBe (^. gsHeaderState . hsToolbarExtensionStatus) ToolbarExtensionClosed


  describe "The commentToolbarExtension_ component" $ do
    it "renders an element with the toolbar extension class" $ do
      wrapper <- shallow . commentToolbarExtension_ $ CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange
      EZ.lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` 1

    it "contains a pointer element" $ do
      wrapper <- shallow . commentToolbarExtension_ $ CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange
      EZ.lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension__pointer")) `shouldReturn` 1

    it "contains an annotation section with 2 normal icon buttons" $ do
      wrapper <- shallow . commentToolbarExtension_ $ CommentToolbarExtensionProps CommentToolbarExtensionWithoutRange
      annotation <- find wrapper (StringSelector ".c-vdoc-toolbar-extension__annotation")
      EZ.lengthOf annotation `shouldReturn` 1
      EZ.lengthOfIO (find annotation (StringSelector "IconButton")) `shouldReturn` 2
