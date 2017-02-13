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


module Refine.Frontend.Header.ToolbarSpec where

import Test.Hspec

import Refine.Frontend.Test.Enzyme
import Refine.Frontend.Header.Toolbar


spec :: Spec
spec = do
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
