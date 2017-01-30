{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
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
      getIOWrapperAttr (find wrapper ".c-mainmenu") "length" `shouldReturn` (1 :: Int)
      getIOWrapperAttr (find wrapper ".c-mainmenu__menu-button") "length" `shouldReturn` (1 :: Int)
      getIOWrapperAttr (find wrapper ".c-mainmenu__icon-bar") "length" `shouldReturn` (3 :: Int)
      label <- find wrapper ".c-mainmenu__menu-button-label"
      getWrapperAttr label "length" `shouldReturn` (1 :: Int)
      text label `shouldReturn` "MENU"


  describe "The documentHeader_ component" $ do
    it "renders its elements" $ do
      wrapper <- shallow $ documentHeader_ (DocumentHeaderProps (Title "title") (Abstract "abstract"))
      getIOWrapperAttr (find wrapper ".c-vdoc-header") "length" `shouldReturn` (1 :: Int)
      getIOWrapperAttr (find wrapper "DocumentAbstract") "length" `shouldReturn` (1 :: Int)
      getIOWrapperAttr (find wrapper "DocumentTitle") "length" `shouldReturn` (1 :: Int)
      getIOWrapperAttr (find wrapper "Phases") "length" `shouldReturn` (1 :: Int)


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
      getIOWrapperAttr (find wrapper ".c-vdoc-header__phase") "length" `shouldReturn` (3 :: Int)

    it "has only one active phase" $ do
      wrapper <- shallow $ phases_
      getIOWrapperAttr (find wrapper ".c-vdoc-header__phase--active") "length" `shouldReturn` (1 :: Int)


  describe "The editToolbar_ component" $ do
    it "renders an element with the toolbar class" $ do
      wrapper <- shallow editToolbar_
      getIOWrapperAttr (find wrapper ".c-vdoc-toolbar") "length" `shouldReturn` (1 :: Int)

    it "contains two separators" $ do
      wrapper <- shallow editToolbar_
      getIOWrapperAttr (find wrapper ".c-vdoc-toolbar__separator") "length" `shouldReturn` (2 :: Int)

    it "contains 5 normal icon buttons" $ do
      wrapper <- shallow editToolbar_
      getIOWrapperAttr (find wrapper "IconButton") "length" `shouldReturn` (5 :: Int)

    it "contains 1 aligned icon button" $ do
      wrapper <- shallow editToolbar_
      getIOWrapperAttr (find wrapper "IconButtonWithAlignment") "length" `shouldReturn` (1 :: Int)

  describe "The editToolbarExtension_ component" $ do
    it "renders an element with the toolbar extension class" $ do
      wrapper <- shallow editToolbarExtension_
      getIOWrapperAttr (find wrapper ".c-vdoc-toolbar-extension") "length" `shouldReturn` (1 :: Int)

    it "contains a pointer element" $ do
      wrapper <- shallow editToolbarExtension_
      getIOWrapperAttr (find wrapper ".c-vdoc-toolbar-extension__pointer") "length" `shouldReturn` (1 :: Int)

    it "contains an annotation section with 2 normal icon buttons" $ do
      wrapper <- shallow editToolbarExtension_
      annotation <- find wrapper ".c-vdoc-toolbar-extension__annotation"
      getWrapperAttr annotation "length" `shouldReturn` (1 :: Int)
      getIOWrapperAttr (find annotation "IconButton") "length" `shouldReturn` (2 :: Int)

    it "contains a modification section with 1 normal icon button" $ do
      wrapper <- shallow editToolbarExtension_
      modification <- find wrapper ".c-vdoc-toolbar-extension__modification"
      getWrapperAttr modification "length" `shouldReturn` (1 :: Int)
      getIOWrapperAttr (find modification "IconButton") "length" `shouldReturn` (1 :: Int)


