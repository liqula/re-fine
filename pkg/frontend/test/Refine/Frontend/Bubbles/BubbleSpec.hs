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

module Refine.Frontend.Bubbles.BubbleSpec where

import           Test.Hspec

import qualified Refine.Frontend.Screen.Types as SC
import           Refine.Frontend.Style
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Bubbles.Bubble

--import Refine.Frontend.Test.Console


spec :: Spec
spec = do

-- TODO add tests for Tablet and Mobile, where appropriate

  let chunkId = 99
      contentType = "the-content-type"
      iconSide = "the-icon-side"
      iconStyle = ("the-icon-name", "the-icon-style")
      markPosition = Just (140, 180)
      callback _ = []
      screenState = SC.ScreenState 95 SC.Desktop
      bubbleProps = BubbleProps chunkId contentType iconSide iconStyle markPosition callback screenState

  describe "The bubble_ component" $ do
    it "does not render anything if there is no mark position in the props" $ do
      wrapper <- shallow $ bubble_ (BubbleProps chunkId contentType iconSide iconStyle Nothing (\_ -> []) screenState) mempty
      -- TODO actually this should already hold - improve react-flux here?
      -- lengthOf wrapper `shouldReturn` (0 :: Int)
      -- TODO and this should return ""
      html wrapper `shouldReturn` "<div></div>"

    it "renders the data-chunk-id that was passed to it" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (PropertySelector [Prop "data-chunk-id" ("99" :: String)]) `shouldReturn` True

    it "renders the data-content-type that was passed to it" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (PropertySelector [Prop "data-content-type" ("the-content-type" :: String)]) `shouldReturn` True

    it "renders the o-snippet class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StringSelector ".o-snippet") `shouldReturn` True

    it "renders the o-snippet-- + contenttype class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StringSelector ".o-snippet--the-content-type") `shouldReturn` True

    it "renders the top style with the correct value" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (PropertySelector [Prop "style" [Style "top" (145 :: Int)]]) `shouldReturn` True

    it "has a child with the icon-bg class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      lengthOfIO (find wrapper (StringSelector ".o-snippet__icon-bg")) `shouldReturn` (1 :: Int)

    it "has a child with the icon-bg + side class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      lengthOfIO (find wrapper (StringSelector ".o-snippet__icon-bg--the-icon-side")) `shouldReturn` (1 :: Int)

    it "has a child with the content class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      lengthOfIO (find wrapper (StringSelector ".o-snippet__content")) `shouldReturn` (1 :: Int)

