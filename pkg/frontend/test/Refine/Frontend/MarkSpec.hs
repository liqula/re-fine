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

import Prelude hiding (length)
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.Mark


spec :: Spec
spec = do
  describe "The rfMark_ component" $ do
    it "renders a mark at top level" $ do
      wrapper <- shallow $ rfMark_ (MarkProps "the-chunk-id" "the-content-type") mempty
      is wrapper (StringSelector "mark") `shouldReturn` True

    it "contains the data-chunk-id annotation that was passed to it" $ do
      wrapper <- shallow $ rfMark_ (MarkProps "the-chunk-id" "the-content-type") mempty
      is wrapper (StringSelector "mark") `shouldReturn` True

