{-# LANGUAGE NoImplicitPrelude          #-}
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


module Refine.Frontend.Header.DocumentHeaderSpec where

import Refine.Frontend.Prelude

import Test.Hspec

import Refine.Common.Types
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.Header.DocumentHeader


spec :: Spec
spec = do
  describe "The documentHeader_ component" $ do
    it "renders its elements" $ do
      wrapper <- shallow $ documentHeader_ (DocumentHeaderProps (Title "title") (Abstract "abstract"))
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-header")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector "DocumentAbstract")) `shouldReturn` (1 :: Int)
      lengthOfIO (find wrapper (StringSelector "DocumentTitle")) `shouldReturn` (1 :: Int)

  describe "The documentTitle_ component" $ do
    it "renders the title" $ do
      wrapper <- shallow $ documentTitle_ (Title "The Awesome Document Title")
      text wrapper `shouldReturn` "The Awesome Document Title"


  describe "The documentAbstract_ component" $ do
    it "renders the abstract" $ do
      wrapper <- shallow $ documentAbstract_ (Abstract "The informative document abstract.")
      text wrapper `shouldReturn` "The informative document abstract."
