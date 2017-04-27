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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.ChunkSpec where

import           Control.Monad
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Refine.Common.Test.Arbitrary
import           Refine.Common.Types.Chunk


spec :: Spec
spec = do
  describe "SelectionState vs. ChunkRange" $ do
    it "are isomorphic" . property $
      \(RawContentWithSelections c ss) -> forM_ ss $ \s -> do
        chunkRangeToSelectionState c (selectionStateToChunkRange c s) `shouldBe` s
