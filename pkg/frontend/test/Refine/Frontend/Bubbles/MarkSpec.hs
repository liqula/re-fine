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


module Refine.Frontend.Bubbles.MarkSpec where

import           Control.Lens((^.), (&), (%~))
import           Data.Monoid ((<>))
import           React.Flux (getStoreData)
import           Test.Hspec
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Frontend.Bubbles.Mark
import           Refine.Frontend.Bubbles.Types
import           Refine.Frontend.Store (refineStore)
import           Refine.Frontend.Test.Enzyme
import qualified Refine.Frontend.Test.Enzyme.ReactWrapperAPI as RW
import           Refine.Frontend.Types


spec :: Spec
spec = do
  describe "The rfMark_ component" $ do
    let theAttribs = [HTMLP.Attr "data-chunk-id" "77", HTMLP.Attr "data-chunk-kind" "the-content-type"]
    let theProps = MarkProps theAttribs Nothing

    it "does not render anything when there is no data-chunk-id" $ do
      wrapper <- shallow $ rfMark_ (MarkProps [HTMLP.Attr "data-chunk-kind" "the-content-type"] Nothing) mempty
      html wrapper `shouldReturn` "<div></div>" -- TODO should be empty

    it "does not render anything when there is no data-chunk-kind" $ do
      wrapper <- shallow $ rfMark_ (MarkProps [HTMLP.Attr "data-chunk-id" "77"] Nothing) mempty
      html wrapper `shouldReturn` "<div></div>" -- TODO should be empty

    it "renders a HTML mark at top level" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector "mark") `shouldReturn` True

    it "has the data-chunk-id annotation that was passed to it " $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (PropertySelector [Prop "data-chunk-id" ("77" :: String)]) `shouldReturn` True

    it "has all other annotations that were passed to it " $ do
      let moreAttrs = [HTMLP.Attr "a" "1", HTMLP.Attr "b" "2", HTMLP.Attr "c" "3"] <> theAttribs
      wrapper <- shallow $ rfMark_ (MarkProps moreAttrs Nothing) mempty
      is wrapper (PropertySelector [Prop "a" ("1" :: String), Prop "b" ("2" :: String), Prop "c" ("3" :: String)]) `shouldReturn` True

    it "has a mark class" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector ".o-mark") `shouldReturn` True

    it "has a mark class with the content type that was passed to it" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector ".o-mark--the-content-type") `shouldReturn` True

    it "does not render the hover class when there is no selected mark" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector ".o-mark--hover") `shouldReturn` False

    it "does not render the hover class when the selected mark does not match the current one" $ do
      wrapper <- shallow $ rfMark_ (MarkProps theAttribs (Just (ID 88))) mempty
      is wrapper (StringSelector ".o-mark--hover") `shouldReturn` False

    it "renders the hover class when the selected mark matches the current one" $ do
      wrapper <- shallow $ rfMark_ (MarkProps theAttribs (Just (ID 77))) mempty
      is wrapper (StringSelector ".o-mark--hover") `shouldReturn` True

    it "inserts the id of the current mark into the state on mouseEnter and removes it again on mouseLeave" $ do
      wrapper <- RW.mount $ rfMark_ theProps mempty
      -- init the state:
      globalState0 <- getStoreData refineStore
      let _ = globalState0 & gsBubblesState . bsHighlightedMarkAndBubble %~ \_ -> Nothing
      -- simulate events:
      _ <- RW.simulate wrapper RW.MouseEnter
      globalState1 <- getStoreData refineStore
      globalState1 ^. gsBubblesState ^. bsHighlightedMarkAndBubble `shouldBe` Just (ID 77)
      _ <- RW.simulate wrapper RW.MouseLeave
      globalState2 <- getStoreData refineStore
      globalState2 ^. gsBubblesState ^. bsHighlightedMarkAndBubble `shouldBe` Nothing

-- TODO tests for componentDidMount code
