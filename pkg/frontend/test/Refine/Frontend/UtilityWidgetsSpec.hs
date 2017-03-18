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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.UtilityWidgetsSpec where

import Test.Hspec

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Style
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.UtilityWidgets


spec :: Spec
spec = do

  let iconProps = IconProps "the-block-name" True ("Image", "striped")
  let element = "the-element-name"
  let module1 = "the-module-name"
  let ctype = "the-content-type"
  let label1 = "the-label"

  describe "The icon_ component" $ do
    it "annotates the block together with the icon module" $ do
      wrapper <- shallow . icon_ $ iconProps XXL
      lengthOfIO (find wrapper (StringSelector ".the-block-name__icon")) `shouldReturn` (1 :: Int)

    it "annotates the highlight class if True is passed" $ do
      wrapper <- shallow . icon_ $ iconProps XXL
      lengthOfIO (find wrapper (StringSelector ".o-icon-highlight")) `shouldReturn` (1 :: Int)

    it "does not annotate the highlight class if False is passed" $ do
      wrapper <- shallow $ icon_ (IconProps "the-block-name" False ("Image", "striped") XXL)
      lengthOfIO (find wrapper (StringSelector ".o-icon-highlight")) `shouldReturn` (0 :: Int)

    it "annotates the icon image class that is passed" $ do
      wrapper <- shallow . icon_ $ iconProps XXL
      lengthOfIO (find wrapper (StringSelector ".Image_striped")) `shouldReturn` (1 :: Int)

    it "annotates the RO icon image when the mouse has entered the icon and the normal one when it left again" $ do
      wrapper <- shallow . icon_ $ iconProps XXL
      simulate wrapper MouseEnter
      lengthOfIO (find wrapper (StringSelector ".Image_striped")) `shouldReturn` (0 :: Int)
      lengthOfIO (find wrapper (StringSelector ".Image_RO")) `shouldReturn` (1 :: Int)
      simulate wrapper MouseLeave
      lengthOfIO (find wrapper (StringSelector ".Image_striped")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector ".Image_RO")) `shouldReturn` (0 :: Int)

    it "annotates the iconsize class with the correct size (XXL)" $ do
      wrapper <- shallow . icon_ $ iconProps XXL
      lengthOfIO (find wrapper (StringSelector ".iconsize-xxl")) `shouldReturn` (1 :: Int)
    it "annotates the iconsize class with the correct size (M)" $ do
      wrapper <- shallow . icon_ $ iconProps M
      lengthOfIO (find wrapper (StringSelector ".iconsize-m")) `shouldReturn` (1 :: Int)

  describe "iconButtonWithAlignmentCore_ component" $ do
    it "has the data content type passed to it" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      is wrapper (PropertySelector [Prop "data-content-type" ("the-content-type" :: String)]) `shouldReturn` True

    it "renders the block name with the button element" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      is wrapper (StringSelector ".the-block-name__button") `shouldReturn` True

    it "renders the block__element class as passed to it" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      is wrapper (StringSelector ".the-block-name__the-element-name") `shouldReturn` True

    it "renders the block__element--module class as passed to it" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      is wrapper (StringSelector ".the-block-name__the-element-name--the-module-name") `shouldReturn` True

    it "renders the alignment class when it should be right-aligned" $ do
      let rightAligned1 = True
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          rightAligned1 Nothing)
      is wrapper (StringSelector ".the-block-name--align-right") `shouldReturn` True

    it "does not render the alignment class when it should not be right-aligned" $ do
      let rightAligned1 = False
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          rightAligned1 Nothing)
      is wrapper (StringSelector ".the-block-name--align-right") `shouldReturn` False

    it "renders the position when it receives a position value" $ do
      let position1 = Just 101
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False position1)
      is wrapper (PropertySelector [Prop "style" [Style "top" (101 :: Int)]]) `shouldReturn` True

    it "does not render the position when it receives no position value" $ do
      let position1 = Nothing
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False position1)
      is wrapper (PropertySelector [Prop "style" ([] :: [Style])]) `shouldReturn` True

    it "renders an Icon" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      lengthOfIO (find wrapper (StringSelector "Icon")) `shouldReturn` (1 :: Int)

    it "has a span that has the block name with the button-label module" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      span1 <- find wrapper (StringSelector "span")
      is span1 (StringSelector ".the-block-name__button-label") `shouldReturn` True

    it "shows a pointer mouse cursor when it is not disabled" $ do
      let disabled1 = False
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 disabled1 (\_ -> []) [])
          False Nothing)
      is wrapper (PropertySelector [Prop "style" [Style "cursor" ("pointer" :: String)]]) `shouldReturn` True

    it "shows the span's text in grey when it is disabled" $ do
      let disabled1 = True
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 disabled1 (\_ -> []) [])
          False Nothing)
      span1 <- find wrapper (StringSelector "span")
      is span1 (PropertySelector [Prop "style" [Style "color" Color.disabledText]]) `shouldReturn` True

    it "displays the label as passed to it" $ do
      wrapper <- shallow $ iconButtonWithAlignmentCore_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      span1 <- find wrapper (StringSelector "span")
      text span1 `shouldReturn` "the-label"

    context "enabled" $ do
      it "reacts to a click event" $ do
        pending

      it "reacts to a tap event" $ do
        pending

    context "disabled" $ do
      it "does not react to a click event" $ do
        pending

      it "does not react to a tap event" $ do
        pending

  describe "iconButtonWithAlignment_ component" $ do
    it "wraps hammer around the inner component" $ do
      pendingWith "We have temporarily removed hammer from the implementation"
      wrapper <- shallow $ iconButtonWithAlignment_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      is wrapper (StringSelector "Hammer") `shouldReturn` True

    it "renders the icon button core component" $ do
      wrapper <- shallow $ iconButtonWithAlignment_
        (IconButtonWithAlignmentProps
          (IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) [])
          False Nothing)
      lengthOfIO (find wrapper (StringSelector "IconButtonWithAlignmentCore")) `shouldReturn` (1 :: Int)


  describe "positionedIconButton_ component" $ do

    let theProps = IconButtonProps (iconProps M) element module1 ctype label1 False (\_ -> []) []

    it "always renders the position that is passed to it" $ do
      coreWrapper <- shallow (positionedIconButton_ (theProps, 377)) >>= (`find` StringSelector "IconButtonWithAlignment")
                  >>= shallowChild >>= (`find` StringSelector "IconButtonWithAlignmentCore") >>= shallowChild
      is coreWrapper (PropertySelector [Prop "style" [Style "top" (377 :: Int)]]) `shouldReturn` True

    it "never renders the right-alignment flag" $ do
      coreWrapper <- shallow (positionedIconButton_ (theProps, 377)) >>= (`find` StringSelector "IconButtonWithAlignment")
                  >>= shallowChild >>= (`find` StringSelector "IconButtonWithAlignmentCore") >>= shallowChild
      lengthOfIO (find coreWrapper (StringSelector ".the-block-name--align-right")) `shouldReturn` (0 :: Int)


  describe "toClasses" $ do
    it "turns one class name into a string containing that class name" $ do
      toClasses ["single-class-name"] `shouldBe` "single-class-name"
    it "intersperses multiple class names with blanks" $ do
      toClasses ["class-name-1", "class-name-2"] `shouldBe` "class-name-1 class-name-2"
    it "ignores empty strings" $ do
      toClasses ["", "class-name-1", "", "class-name-2", ""] `shouldBe` "class-name-1 class-name-2"
