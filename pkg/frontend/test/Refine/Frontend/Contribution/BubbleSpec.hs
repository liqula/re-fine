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

module Refine.Frontend.Contribution.BubbleSpec where

import           Control.Lens((^.), (&), (%~))
import           Data.Int (Int64)
import           Test.Hspec
import           React.Flux (getStoreData)

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Types
import qualified Refine.Frontend.Screen.Types as SC
import           Refine.Frontend.Store (refineStore)
import           Refine.Frontend.Style
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Types

--import Refine.Frontend.Test.Console


cnid :: Int64 -> ContributionID
cnid = ContribIDNote . ID


spec :: Spec
spec = do

-- TODO add tests for Tablet and Mobile, where appropriate

  let contributionId = cnid 99
      contentType = "the-content-type"
      iconSide = "the-icon-side"
      iconStyle = ("the-icon-name", "the-icon-style")
      markPosition = Just (MarkPosition (SC.OffsetFromDocumentTop (140 + 180)) (SC.OffsetFromDocumentTop (160 + 180)))
      highlight = Nothing
      callback _ = []
      screenState = SC.ScreenState 95 SC.Desktop
      bubbleProps = BubbleProps contributionId contentType iconSide iconStyle markPosition highlight callback screenState

  describe "The bubble_ component" $ do
    it "does not render anything if there is no mark position in the props" $ do
      wrapper <- shallow $ bubble_ (BubbleProps contributionId contentType iconSide iconStyle Nothing highlight callback screenState) mempty
      -- TODO actually this should already hold - improve react-flux here?
      -- lengthOf wrapper `shouldReturn` (0 :: Int)
      -- TODO and this should return ""
      html wrapper `shouldReturn` "<div></div>"

    it "renders the data-contribution-id that was passed to it" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (PropertySelector [Prop "data-contribution-id" ("n99" :: String)]) `shouldReturn` True

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

    it "does not render the hover class when there is no highlighted bubble" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` False

    it "does not render the hover class when the highlighted bubble does not match the current one" $ do
      wrapper <- shallow $ bubble_ (BubbleProps contributionId contentType iconSide iconStyle markPosition (Just (cnid 101)) callback screenState) mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` False

    it "renders the hover class when the highlighted bubble matches the current one" $ do
      wrapper <- shallow $ bubble_ (BubbleProps contributionId contentType iconSide iconStyle markPosition (Just (cnid 99)) callback screenState) mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` True

    it "inserts the id of the current bubble into the state on mouseEnter and removes it again on mouseLeave" $ do
      wrapper <- mount $ bubble_ bubbleProps mempty
      -- init the state:
      globalState0 <- getStoreData refineStore
      let _ = globalState0 & gsContributionState . csHighlightedMarkAndBubble %~ \_ -> Nothing
      -- simulate events:
      _ <- simulate wrapper MouseEnter
      globalState1 <- getStoreData refineStore
      globalState1 ^. gsContributionState . csHighlightedMarkAndBubble `shouldBe` Just (cnid 99)
      _ <- simulate wrapper MouseLeave
      globalState2 <- getStoreData refineStore
      globalState2 ^. gsContributionState . csHighlightedMarkAndBubble `shouldBe` Nothing
