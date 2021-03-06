{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.ViewsSpec where
#include "import_frontend.hs"

import           Test.Hspec
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Views

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- we do not need this in prod, but it is important for testing the full app including the global state
refineApp_ :: ReactElementM eventHandler ()
refineApp_ = view_ refineApp "refineApp_"

clickNewComment :: ReactWrapper -> IO ()
clickNewComment wrapper = do
      button <- find wrapper (StringSelector ".c-vdoc-toolbar__btn-add-annotation")
      _ <- simulate button Click
      pure ()

clickArticleContent :: ReactWrapper -> IO ()
clickArticleContent wrapper = do
      mainText <- find wrapper (StringSelector ".c-article-content")
      _ <- simulate mainText Click
      pure ()

clickTextSpecificComment :: ReactWrapper -> IO ()
clickTextSpecificComment wrapper = do
      mainText <- find wrapper (StringSelector ".c-vdoc-toolbar-extension__btn-new-ann-text")
      _ <- simulate mainText Click
      pure ()


spec :: Spec
spec = do
  describe "The refineApp root component" $ do
    it "starts fine" $ do
      wrapper <- mount refineApp_
      contents :: String <- cs <$> html wrapper
      contents `shouldContain` "Username"

  describe "The mainScreen_ component" $ do
    it "initially the comment toolbar is visible" $ do
      pendingWith "#201"
      wrapper <- mount refineApp_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar")) `shouldReturn` (1 :: Int)

    it "initially the comment toolbar extension is not visible" $ do
      pendingWith "#201"
      wrapper <- mount refineApp_
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)

    it "opens the comment toolbar extension when the user clicks on the 'new comment' button" $ do
      pendingWith "#201"
      wrapper <- mount refineApp_
      clickNewComment wrapper
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (1 :: Int)

    it "closes the comment toolbar extension when the user clicks on the 'new comment' button twice" $ do
      pendingWith "#201"
      wrapper <- mount refineApp_
      clickNewComment wrapper
      clickNewComment wrapper
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)

    it "closes the comment toolbar extension when the user clicks somewhere else" $ do
      pendingWith "#201"
      wrapper <- mount refineApp_
      clickNewComment wrapper
      clickArticleContent wrapper
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)

    it "does not close the comment toolbar extension when the user clicks on the 'text-specific comment' button" $ do
      pendingWith "#201"
      wrapper <- mount refineApp_
      clickNewComment wrapper
      clickTextSpecificComment wrapper
      lengthOfIO (find wrapper (StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (1 :: Int)
