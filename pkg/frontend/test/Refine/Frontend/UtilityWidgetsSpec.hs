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

module Refine.Frontend.UtilityWidgetsSpec where

import Test.Hspec

import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.UtilityWidgets


spec :: Spec
spec = do
  describe "The icon_ component" $ do
    it "annotates the block together with the icon module" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      lengthIO (find wrapper (StringSelector ".bla__icon")) `shouldReturn` (1 :: Int)

    -- TODO I hope this is only temporary
    it "annotates the block together with the category-icon module" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      lengthIO (find wrapper (StringSelector ".bla__category-icon")) `shouldReturn` (1 :: Int)

    it "annotates the highlight class if True is passed" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      lengthIO (find wrapper (StringSelector ".o-icon-highlight")) `shouldReturn` (1 :: Int)

    it "does not annotate the highlight class if False is passed" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" False ("Image", "striped") XXL)
      lengthIO (find wrapper (StringSelector ".o-icon-highlight")) `shouldReturn` (0 :: Int)

    it "annotates the icon image class that is passed" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      lengthIO (find wrapper (StringSelector ".Image_striped")) `shouldReturn` (1 :: Int)

    it "annotates the RO icon image when the mouse has entered the icon and the normal one when it left again" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      wrapper2 <- simulate wrapper MouseEnter
      lengthIO (find wrapper2 (StringSelector ".Image_striped")) `shouldReturn` (0 :: Int)
      lengthIO (find wrapper2 (StringSelector ".Image_RO")) `shouldReturn` (1 :: Int)
      wrapper3 <- simulate wrapper2 MouseLeave
      lengthIO (find wrapper3 (StringSelector ".Image_striped")) `shouldReturn` (1 :: Int)
      lengthIO (find wrapper3 (StringSelector ".Image_RO")) `shouldReturn` (0 :: Int)

    it "annotates the iconsize class with the correct size (XXL)" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      lengthIO (find wrapper (StringSelector ".iconsize-xxl")) `shouldReturn` (1 :: Int)
    it "annotates the iconsize class with the correct size (M)" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") M)
      lengthIO (find wrapper (StringSelector ".iconsize-m")) `shouldReturn` (1 :: Int)

    it "has 8 spans" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") M)
      lengthIO (find wrapper (StringSelector "span")) `shouldReturn` (8 :: Int)

  describe "iconButtonWithAlignmentCore_ component" $ do
    let iconProps1 = IconProps  "the-block-name" True ("Image", "striped") M
    let element = "the-element-name"
    let module1 = "the-module-name"
    let ctype = "the-content-type"
    let label1 = "the-label"

    it "has the data content type passed to it" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps iconProps1 element module1 ctype label1 False (\_ -> []))
          False Nothing)
      lengthIO (find wrapper (PropertySelector [Prop "data-content-type" ("the-content-type" :: String)])) `shouldReturn` (1 :: Int)

    it "renders the block name with the button element" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps iconProps1 element module1 ctype label1 False (\_ -> []))
          False Nothing)
      consoleLogShallowWrapper "..." wrapper
      lengthIO (find wrapper (StringSelector ".the-block-name__button")) `shouldReturn` (1 :: Int)

    it "renders the block__element class as passed to it" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps iconProps1 element module1 ctype label1 False (\_ -> []))
          False Nothing)
      lengthIO (find wrapper (StringSelector ".the-block-name__the-element-name")) `shouldReturn` (1 :: Int)

    it "renders the block__element--module class as passed to it" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps iconProps1 element module1 ctype label1 False (\_ -> []))
          False Nothing)
      lengthIO (find wrapper (StringSelector ".the-block-name__the-element-name--the-module-name")) `shouldReturn` (1 :: Int)

    it "renders the alignment class when it should be right-aligned" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps iconProps1 element module1 ctype label1 False (\_ -> []))
          True Nothing)
      lengthIO (find wrapper (StringSelector ".the-block-name--align-right")) `shouldReturn` (1 :: Int)

    it "does not render the alignment class when it should not be right-aligned" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps iconProps1 element module1 ctype label1 False (\_ -> []))
          False Nothing)
      lengthIO (find wrapper (StringSelector ".the-block-name--align-right")) `shouldReturn` (0 :: Int)


  describe "toClasses" $ do
    it "turns one class name into a string containing that class name" $ do
      toClasses ["single-class-name"] `shouldBe` "single-class-name"
    it "intersperses multiple class names with blanks" $ do
      toClasses ["class-name-1", "class-name-2"] `shouldBe` "class-name-1 class-name-2"
    it "ignores empty strings" $ do
      toClasses ["", "class-name-1", "", "class-name-2", ""] `shouldBe` "class-name-1 class-name-2"
