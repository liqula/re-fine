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

import Test.Hspec

import           Refine.Common.Types
import           Refine.Frontend.Bubbles.Mark
import           Refine.Frontend.Test.Enzyme


spec :: Spec
spec = do
  describe "The rfMark_ component" $ do
    let theAttribs = Just (MarkAttributes (ID 77) "the-content-type")
    let theProps = MarkProps theAttribs Nothing

    it "does not render anything when there are no mark props" $ do
      wrapper <- shallow $ rfMark_ (MarkProps Nothing Nothing) mempty
      html wrapper `shouldReturn` "<div></div>" -- TODO should be empty

    it "renders a HTML mark at top level" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (StringSelector "mark") `shouldReturn` True

    it "has the data-chunk-id annotation that was passed to it" $ do
      wrapper <- shallow $ rfMark_ theProps mempty
      is wrapper (PropertySelector [Prop "data-chunk-id" ("77" :: String)]) `shouldReturn` True

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

-- TODO tests for componentDidMount code
