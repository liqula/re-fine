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


module Refine.Frontend.Contribution.MarkSpec where

import           Control.Lens((^.), (&), (%~))
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           React.Flux (getStoreData)
import           Test.Hspec
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Mark
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Store (refineStore)
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Types


cnid :: Int64 -> ContributionID
cnid = ContribIDNote . ID


spec :: Spec
spec = do
  describe "The rfMark_ component" $ do
    let theAttribs = [HTMLP.Attr "data-contribution-id" "n77"]
    let theProps = MarkProps theAttribs Nothing Nothing

    it "does not render anything when there is no data-contribution-id" $ do
      wrapper <- shallow $ rfMark_ (MarkProps [] Nothing Nothing) mempty
      html wrapper `shouldReturn` "<div></div>" -- TODO should be empty (react doesn't need this work-around any more since (perhaps) 15.0)

    it "renders a HTML mark at top level" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector "mark") `shouldReturn` True

    it "has the data-contribution-id annotation that was passed to it" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (PropertySelector [Prop "data-contribution-id" ("n77" :: String)]) `shouldReturn` True

    it "has all other annotations that were passed to it" $ do
      let moreAttrs = [HTMLP.Attr "a" "1", HTMLP.Attr "b" "2", HTMLP.Attr "c" "3"] <> theAttribs
      wrapper <- shallow $ rfMark_ (MarkProps moreAttrs Nothing Nothing) mempty
      is wrapper (PropertySelector [Prop "a" ("1" :: String), Prop "b" ("2" :: String), Prop "c" ("3" :: String)]) `shouldReturn` True

    it "has a mark class" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector ".o-mark") `shouldReturn` True

    it "has a mark class with the content type that was passed to it" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector ".o-mark--note") `shouldReturn` True

    it "does not render the hover class when there is no selected mark" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector ".o-mark--hover") `shouldReturn` False

    it "does not render the hover class when the selected mark does not match the current one" $ do
      wrapper <- shallow $ rfMark_ (MarkProps theAttribs (Just (cnid 88)) Nothing) mempty
      is wrapper (StringSelector ".o-mark--hover") `shouldReturn` False

    it "renders the hover class when the selected mark matches the current one" $ do
      wrapper <- shallow $ rfMark_ (MarkProps theAttribs (Just (cnid 77)) Nothing) mempty
      is wrapper (StringSelector ".o-mark--hover") `shouldReturn` True

    it "inserts the id of the current mark into the state on mouseEnter and removes it again on mouseLeave" $ do
      wrapper <- mount $ rfMark_ theProps mempty
      -- init the state:
      globalState0 <- getStoreData refineStore
      let _ = globalState0 & gsContributionState . csHighlightedMarkAndBubble %~ \_ -> Nothing
      -- simulate events:
      _ <- simulate wrapper MouseEnter
      globalState1 <- getStoreData refineStore
      globalState1 ^. gsContributionState . csHighlightedMarkAndBubble `shouldBe` Just (cnid 77)
      _ <- simulate wrapper MouseLeave
      globalState2 <- getStoreData refineStore
      globalState2 ^. gsContributionState . csHighlightedMarkAndBubble `shouldBe` Nothing

-- TODO tests for componentDidMount code
