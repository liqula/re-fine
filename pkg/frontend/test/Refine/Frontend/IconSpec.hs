{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.IconSpec where

import Refine.Frontend.Prelude hiding (S)

import Test.Hspec
import Language.Css.Syntax hiding (S)

import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Icon
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Util


iconButtonTestProps :: IconButtonProps
iconButtonTestProps =
    IconButtonProps "key" (iconProps Medium) element module1 label1 False Nothing False [] [] []
  where
    element :: JSString
    element = "the-element-name"

    module1 :: JSString
    module1 = "the-module-name"

    label1 :: JSString
    label1 = "the-label"

iconProps :: IconSize -> IconProps
iconProps = IconProps "the-block-name" True ("Image", "striped")


spec :: Spec
spec = do
  describe "The icon_ component" $ do
    it "renders ok" $ do  -- FUTUREWORK: make this a generic test that is run on all properties implicitly.
      wrapper <- mount $ icon_ IconProps
          { _iconPropsBlockName = "blockname"
          , _iconPropsHighlight = True
          , _iconPropsDesc      = ("desc1", "desc2")
          , _iconPropsSize      = Medium
          }
      contents :: String <- cs <$> html wrapper
      contents `shouldContain` "<div "

    it "annotates the block together with the icon module" $ do
      wrapper <- shallow . icon_ $ iconProps XXLarge
      lengthOfIO (find wrapper (StringSelector ".the-block-name__icon")) `shouldReturn` (1 :: Int)

    it "annotates the highlight class if True is passed" $ do
      wrapper <- shallow . icon_ $ iconProps XXLarge
      lengthOfIO (find wrapper (StringSelector ".o-icon-highlight")) `shouldReturn` (1 :: Int)

    it "does not annotate the highlight class if False is passed" $ do
      wrapper <- shallow $ icon_ (IconProps "the-block-name" False ("Image", "striped") XXLarge)
      lengthOfIO (find wrapper (StringSelector ".o-icon-highlight")) `shouldReturn` (0 :: Int)

    it "annotates the icon image class that is passed" $ do
      wrapper <- shallow . icon_ $ iconProps XXLarge
      lengthOfIO (find wrapper (StringSelector ".Image_striped")) `shouldReturn` (1 :: Int)

    it "annotates the RO icon image when the mouse has entered the icon and the normal one when it left again" $ do
      wrapper <- mount . icon_ $ iconProps XXLarge
      simulate wrapper MouseEnter
      lengthOfIO (find wrapper (StringSelector ".Image_striped")) `shouldReturn` (0 :: Int)
      lengthOfIO (find wrapper (StringSelector ".Image_RO")) `shouldReturn` (1 :: Int)
      simulate wrapper MouseLeave
      lengthOfIO (find wrapper (StringSelector ".Image_striped")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector ".Image_RO")) `shouldReturn` (0 :: Int)

  describe "iconButton_ component" $ do
    it "renders the block name with the button element" $ do
      wrapper <- shallow $ iconButton_
        iconButtonTestProps
      is wrapper (StringSelector ".the-block-name__button") `shouldReturn` True

    it "renders the block__element class as passed to it" $ do
      wrapper <- shallow $ iconButton_
        iconButtonTestProps
      is wrapper (StringSelector ".the-block-name__the-element-name") `shouldReturn` True

    it "renders the block__element--module class as passed to it" $ do
      wrapper <- shallow $ iconButton_
        iconButtonTestProps
      is wrapper (StringSelector ".the-block-name__the-element-name--the-module-name") `shouldReturn` True

    it "renders the alignment class when it should be right-aligned" $ do
      wrapper <- shallow $ iconButton_ (iconButtonTestProps & iconButtonPropsAlignRight .~ True)
      is wrapper (StringSelector ".the-block-name--align-right") `shouldReturn` True

    it "does not render the alignment class when it should not be right-aligned" $ do
      wrapper <- shallow $ iconButton_ iconButtonTestProps
      is wrapper (StringSelector ".the-block-name--align-right") `shouldReturn` False

    it "renders the position when it receives a position value" $ do
      wrapper <- shallow $ iconButton_ (iconButtonTestProps & iconButtonPropsPosition .~ Just 101)
      is wrapper (StyleSelector [decl "top" (Px 101)]) `shouldReturn` True

    it "does not render the position when it receives no position value" $ do
      wrapper <- shallow $ iconButton_ iconButtonTestProps
      is wrapper (StyleSelector []) `shouldReturn` True

    it "renders an Icon" $ do
      wrapper <- shallow $ iconButton_ iconButtonTestProps
      lengthOfIO (find wrapper (StringSelector "Icon")) `shouldReturn` (1 :: Int)

    it "has a span that has the block name with the button-label module" $ do
      wrapper <- shallow $ iconButton_ iconButtonTestProps
      span1 <- find wrapper (StringSelector "span")
      is span1 (StringSelector ".the-block-name__button-label") `shouldReturn` True

    it "shows the span's text in grey when it is disabled" $ do
      wrapper <- shallow $ iconButton_ (iconButtonTestProps & iconButtonPropsDisabled .~ True)
      span1 <- find wrapper (StringSelector "span")
      is span1 (StyleSelector [decl "color" Color.DisabledTextColor]) `shouldReturn` True

    it "shows a pointer mouse cursor when it is not disabled" $ do
      wrapper <- shallow $ iconButton_ iconButtonTestProps
      is wrapper (StyleSelector [decl "cursor" (Ident "pointer")]) `shouldReturn` True

    it "displays the label as passed to it" $ do
      wrapper <- shallow $ iconButton_ iconButtonTestProps
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

    it "wraps hammer around the inner component" $ do
      pendingWith "We have temporarily removed hammer from the implementation"
      wrapper <- shallow $ iconButton_ iconButtonTestProps
      is wrapper (StringSelector "Hammer") `shouldReturn` True

    it "renders" $ do
      pending
      wrapper <- shallow $ iconButton_ iconButtonTestProps
      lengthOfIO (find wrapper (StringSelector "IconButton")) `shouldReturn` 1


    let theProps = iconButtonTestProps & iconButtonPropsPosition .~ Just 377

    it "always renders the position that is passed to it" $ do
      wrapper <- shallow (iconButton_ theProps)
      is wrapper (StyleSelector [decl "top" (Px 377)]) `shouldReturn` True

    it "never renders the right-alignment flag" $ do
      wrapper <- shallow (iconButton_ theProps)
      lengthOfIO (find wrapper (StringSelector ".the-block-name--align-right")) `shouldReturn` 0
