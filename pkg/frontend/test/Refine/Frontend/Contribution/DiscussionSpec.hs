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

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports -fno-warn-unused-binds #-}  -- TODO

module Refine.Frontend.Contribution.DiscussionSpec where

import Refine.Frontend.Prelude

import           Test.Hspec

import           React.Flux.Missing
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Test.Enzyme

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


testDiscussionProps :: DiscussionProps
testDiscussionProps = undefined  -- TODO

spec :: Spec
spec = do
  describe "The discussion_ component" $ do
    describe "render" $ do
      it "show blocks overlapping with range" $ do
        -- _wrapper <- mount $ discussion_ testDiscussionProps
        pending

      it "do not show blocks not overlapping with range" $ do
        pending

      it "mark range" $ do
        pending

      it "show texts of all statements in discussion" $ do
        pending

      it "indent statement according to its depth in the tree" $ do
        pending

    describe "events -> actions" $ do
      it "upvote discussion" $ do
        pending

      it "downvote discussion" $ do
        pending

      it "back" $ do
        pending

      it "respond to statement" $ do
        pending

      it "upvote statement" $ do
        pending

      it "downvote statement" $ do
        pending
