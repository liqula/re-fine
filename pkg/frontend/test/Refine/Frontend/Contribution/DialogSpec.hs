{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Contribution.DialogSpec where

import Refine.Frontend.Prelude

import           Test.Hspec

import           React.Flux.Missing
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Test.Enzyme

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec = do
  describe "The commentInput_ component" $ do
    it "renders dark note and discussion icons (at this point, no comment type has been selected)" $ do
      r <- newLocalStateRefM $ CommentInputState (CommentInfo "" Nothing) False False
      wrapper <- mount $ commentInput_ r
      lengthOfIO (find wrapper (StringSelector ".icon-Note_dark"))       `shouldReturn` 1
      lengthOfIO (find wrapper (StringSelector ".icon-Note_RO"))         `shouldReturn` 0

      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_dark")) `shouldReturn` 1
      lengthOfIO (find wrapper (StringSelector ".icon-Discussion_RO"))   `shouldReturn` 0

    it "renders highlighted note and dark discussion icons when Note has been selected" $ do
      r <- newLocalStateRefM $ CommentInputState (CommentInfo "" Nothing) False False
      _wrapper <- mount $ commentInput_ r
      pendingWith "FIXME: find button and simulate click."

    it "renders dark note and highlighted discussion icons when Discussion has been selected" $ do
      r <- newLocalStateRefM $ CommentInputState (CommentInfo "" Nothing) False False
      _wrapper <- mount $ commentInput_ r
      pendingWith "FIXME: find button and simulate click."
