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

module Refine.Frontend.Contribution.DialogSpec where

import           Test.Hspec

import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Test.Enzyme

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec = do
  describe "The commentInput_ component" $ do
    it "renders dark note and discussion icons when no comment has been selected" $ do
      wrapper <- mount $ commentInput_ (AddCommentProps True Nothing Nothing 10)
      lengthOfIO (find wrapper (StringSelector ".icon-Note_dark"))       `shouldReturn` 1
      lengthOfIO (find wrapper (StringSelector ".icon-Note_RO"))         `shouldReturn` 0

      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_dark")) `shouldReturn` 1
      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_RO"))   `shouldReturn` 0

    it "renders highlighted note and dark discussion icons when Note has been selected" $ do
      wrapper <- mount $ commentInput_ (AddCommentProps True Nothing (Just CommentKindNote) 10)
      lengthOfIO (find wrapper (StringSelector ".icon-Note_dark"))       `shouldReturn` 0
      lengthOfIO (find wrapper (StringSelector ".icon-Note_RO"))         `shouldReturn` 1

      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_dark")) `shouldReturn` 1
      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_RO"))   `shouldReturn` 0

    it "renders dark note and highlighted discussion icons when Discussion has been selected" $ do
      wrapper <- mount $ commentInput_ (AddCommentProps True Nothing (Just CommentKindDiscussion) 10)
      lengthOfIO (find wrapper (StringSelector ".icon-Note_dark"))       `shouldReturn` 1
      lengthOfIO (find wrapper (StringSelector ".icon-Note_RO"))         `shouldReturn` 0

      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_dark")) `shouldReturn` 0
      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_RO"))   `shouldReturn` 1
