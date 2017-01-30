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


module Refine.Frontend.MarkSpec where

import Test.Hspec

import           Prelude hiding (length)
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Mark


spec :: Spec
spec = do
  describe "The rfMark_ component" $ do
    it "renders a HTML mark at top level" $ do
      wrapper <- shallow $ rfMark_ (MarkProps 77 "the-content-type") mempty
      is wrapper (StringSelector "mark") `shouldReturn` True

    it "has the data-chunk-id annotation that was passed to it" $ do
      wrapper <- shallow $ rfMark_ (MarkProps 77 "the-content-type") mempty
      is wrapper (PropertySelector [Prop "data-chunk-id" ("77" :: String)]) `shouldReturn` True

    it "has a mark class" $ do
      wrapper <- shallow $ rfMark_ (MarkProps 77 "the-content-type") mempty
      is wrapper (StringSelector ".o-mark") `shouldReturn` True

    it "has a mark class with the content type that was passed to it" $ do
      wrapper <- shallow $ rfMark_ (MarkProps 77 "the-content-type") mempty
      is wrapper (StringSelector ".o-mark--the-content-type") `shouldReturn` True

-- TODO tests for componentDidMount code
