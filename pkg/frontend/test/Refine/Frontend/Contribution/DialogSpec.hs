{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Contribution.DialogSpec where
#include "import_frontend.hs"

import           Test.Hspec

import           React.Flux.Missing
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Test.Enzyme

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec = do
  describe "The commentInput_ component" $ do
    it "renders highlighted note and dark discussion icons when Note has been selected" $ do
      r <- newLocalStateRefM $ CommentInputState (CommentInfo "" Nothing) def def
      _wrapper <- mount $ commentInput_ r
      pendingWith "FIXME: find button and simulate click."

    it "renders dark note and highlighted discussion icons when Discussion has been selected" $ do
      r <- newLocalStateRefM $ CommentInputState (CommentInfo "" Nothing) def def
      _wrapper <- mount $ commentInput_ r
      pendingWith "FIXME: find button and simulate click."
