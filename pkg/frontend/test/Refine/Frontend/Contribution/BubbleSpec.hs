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

module Refine.Frontend.Contribution.BubbleSpec where

import           Control.Lens((^.), (&), (.~))
import           Data.Int (Int64)
import           Test.Hspec
import           React.Flux (registerInitialStore, readStoreData)

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Types
import qualified Refine.Frontend.Screen.Types as SC
import           Refine.Frontend.Style
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Types


cnid :: Int64 -> ContributionID
cnid = ContribIDNote . ID


spec :: Spec
spec = do
  let contributionId = cnid 99
      iconSide = "the-icon-side"
      iconStyle = ("the-icon-name", "the-icon-style")
      markPosition = Just (MarkPosition (SC.OffsetFromDocumentTop (140 + 180)) (SC.OffsetFromDocumentTop (160 + 180)))
      highlight = Nothing
      callback _ = []
      screenState = SC.ScreenState 95 0 SC.Desktop
      bubbleProps = BubbleProps contributionId iconSide iconStyle markPosition highlight callback screenState

  describe "The bubble_ component" $ do
    it "renders the data-contribution-id that was passed to it" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (PropertySelector [Prop "data-contribution-id" ("n99" :: String)]) `shouldReturn` True

    it "renders the o-snippet class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StringSelector ".o-snippet") `shouldReturn` True

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

    it "does not render the hover class when there is no highlighted bubble" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` False

    it "does not render the hover class when the highlighted bubble does not match the current one" $ do
      wrapper <- shallow $ bubble_ (bubbleProps & bubblePropsHighlightedBubble .~ Just (cnid 101)) mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` False

    it "renders the hover class when the highlighted bubble matches the current one" $ do
      wrapper <- shallow $ bubble_ (bubbleProps & bubblePropsHighlightedBubble .~ Just (cnid 99)) mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` True

    it "inserts the id of the current bubble into the state on mouseEnter and removes it again on mouseLeave" $ do
      registerInitialStore emptyGlobalState
      wrapper <- mount $ bubble_ bubbleProps mempty
      globalState0 <- readStoreData @GlobalState
      globalState0 ^. gsContributionState . csHighlightedMarkAndBubble `shouldBe` Nothing

      _ <- simulate wrapper MouseEnter
      globalState1 <- readStoreData @GlobalState
      globalState1 ^. gsContributionState . csHighlightedMarkAndBubble `shouldBe` Just (cnid 99)

      _ <- simulate wrapper MouseLeave
      globalState2 <- readStoreData @GlobalState
      globalState2 ^. gsContributionState . csHighlightedMarkAndBubble `shouldBe` Nothing

  describe "tablet and mobile" $ do
    it "works" $ do
      pending
