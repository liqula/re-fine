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

module Refine.Frontend.ViewsSpec where

import qualified Data.Map.Strict as M
import qualified Data.Tree as DT
import           React.Flux
import           Test.Hspec
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Types
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


clearState :: IO ()
clearState =
    let newVDoc = CompositeVDoc (VDoc (ID 1) (Title "the-title") (Abstract "the-abstract") (ID 1))
                                (VDocRepo (ID 1) (ID 1)) (ID 1)
                                (VDocVersion [DT.Node (HTMLP.TagOpen "div" [HTMLP.Attr "data-uid" "77", HTMLP.Attr "data-offset" "0"]) []])
                                M.empty M.empty M.empty
    in do
      -- FIXME: If we add ClearState to the list of Actions, we run into (timing?!) problems...
      dispatchManyM [OpenDocument newVDoc, HeaderAction CloseToolbarExtension]
      reactFluxWorkAroundThreadDelay


spec :: Spec
spec = do
  describe "The mainScreen_ component" . before clearState $ do

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
