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

import           Refine.Common.Types
import           Refine.Frontend.Header.Heading
import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme as EZ
import           Refine.Frontend.Test.Store


spec :: Spec
spec = do
  describe "The toolbar_ component" $ do
    it "toggles the visibility of the edit toolbar extension when the 'new comment' button is clicked" $ do
      pendingWith "sometimes fails, sometimes passes; related to #201 or #221?"

      let mockVDoc :: VDoc
          mockVDoc = VDoc (MetaID (ID 0) undefined) (Title "title") (Abstract "abstract") undefined undefined

      wrapper <- mount $ toolbarWrapper_ (toolbar_ mockVDoc)
      button <- find wrapper (StringSelector ".c-vdoc-toolbar__btn-add-annotation")

      _ <- simulate button Click
      storeShouldEventuallyBe ((^. gsHeaderState . hsToolbarExtensionStatus) :: GlobalState -> ToolbarExtensionStatus) CommentToolbarExtensionWithoutRange

      _ <- simulate button Click
      storeShouldEventuallyBe ((^. gsHeaderState . hsToolbarExtensionStatus) :: GlobalState -> ToolbarExtensionStatus) ToolbarExtensionClosed


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
