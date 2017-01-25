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

import Refine.Frontend.Test.Enzyme
import Refine.Frontend.UtilityWidgets


spec :: Spec
spec =
  describe "The icon_ component" $ do
    it "annotates the block together with the icon module" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      wrapper1 <- find wrapper ".bla__icon"
      getWrapperAttr wrapper1 "length" `shouldReturn` (1 :: Int)

    -- TODO I hope this is only temporary
    it "annotates the block together with the category-icon module" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      wrapper1 <- find wrapper ".bla__category-icon"
      getWrapperAttr wrapper1 "length" `shouldReturn` (1 :: Int)

    it "annotates the highlight class if True is passed" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      wrapper2 <- find wrapper ".o-icon-highlight"
      getWrapperAttr wrapper2 "length" `shouldReturn` (1 :: Int)

    it "annotates the icon image class that passed" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      wrapper2 <- find wrapper ".Image_striped"
      getWrapperAttr wrapper2 "length" `shouldReturn` (1 :: Int)

    it "annotates the iconsize class with the correct size" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Image", "striped") XXL)
      wrapper3 <- find wrapper ".iconsize-xxl"
      getWrapperAttr wrapper3 "length" `shouldReturn` (1 :: Int)

