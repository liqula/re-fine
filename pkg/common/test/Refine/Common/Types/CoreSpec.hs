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
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.CoreSpec where

import Refine.Common.Prelude

import           Test.Hspec
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Utils

import Refine.Common.Types.Core
import Refine.Common.Test.Arbitrary

spec :: Spec
spec = parallel $ do
  testBatch $ monoid (mempty :: Ranges Int)

  describe "intersectionRanges" $ do
    it "associative" $ isAssociative (intersectionRanges :: Ranges Int -> Ranges Int -> Ranges Int)
    it "commutative" $ isCommutable (intersectionRanges :: Ranges Int -> Ranges Int -> Ranges Int)
