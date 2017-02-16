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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.ViewsSpec where

import           Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Tree as DT
import           React.Flux
import           Test.Hspec
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store
import qualified Refine.Frontend.Test.Enzyme.ReactWrapper as RW
import           Refine.Frontend.Types
import           Refine.Frontend.Views

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


-- we do not need this in prod, but it is important for testing the full app including the global state
refineApp_ :: ReactElementM eventHandler ()
refineApp_ = view refineApp () mempty

dispatchAndMount :: [RefineAction] -> ReactElementM eventHandler () -> IO RW.ReactWrapper
dispatchAndMount actions comp_ = do
      forM_ (concatMap dispatch actions) executeAction
      RW.mount comp_

clickNewComment :: RW.ReactWrapper -> IO ()
clickNewComment wrapper = do
      button <- RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar__btn-add-annotation")
      _ <- RW.simulate button RW.Click
      pure ()

clickArticleContent :: RW.ReactWrapper -> IO ()
clickArticleContent wrapper = do
      mainText <- RW.find wrapper (RW.StringSelector ".c-article-content")
      _ <- RW.simulate mainText RW.Click
      pure ()


spec :: Spec
spec = do
  describe "The mainScreen_ component" $ do
    let newVDoc = CompositeVDoc (VDoc (ID 1) (Title "the-title") (Abstract "the-abstract") (ID 1))
                                (VDocRepo (ID 1) (ID 1))
                                (VDocVersion [DT.Node (HTMLP.TagOpen "div" [HTMLP.Attr "data-uid" "77", HTMLP.Attr "data-offset" "0"]) []])
                                M.empty M.empty M.empty

    it "initially the comment toolbar is visible" $ do
      wrapper <- dispatchAndMount [OpenDocument newVDoc, HeaderAction CloseCommentToolbarExtension] refineApp_
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar__btn-add-annotation")) `shouldReturn` (1 :: Int)

    it "initially the comment toolbar extension is not visible" $ do
      wrapper <- dispatchAndMount [OpenDocument newVDoc, HeaderAction CloseCommentToolbarExtension] refineApp_
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)

    it "opens the comment toolbar extension when the user clicks on the 'new comment' button" $ do
      wrapper <- dispatchAndMount [OpenDocument newVDoc, HeaderAction CloseCommentToolbarExtension] refineApp_
      clickNewComment wrapper
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (1 :: Int)

    it "closes the comment toolbar extension when the user clicks on the 'new comment' button twice" $ do
      wrapper <- dispatchAndMount [OpenDocument newVDoc, HeaderAction CloseCommentToolbarExtension] refineApp_
      clickNewComment wrapper
      clickNewComment wrapper
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)

    it "closes the comment toolbar extension when the user clicks somewhere else" $ do
      wrapper <- dispatchAndMount [OpenDocument newVDoc, HeaderAction CloseCommentToolbarExtension] refineApp_
      clickNewComment wrapper
      clickArticleContent wrapper
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)

    it "does not close the comment toolbar extension when the user clicks on the 'text-specific comment' button" $ do
      wrapper <- dispatchAndMount [OpenDocument newVDoc, HeaderAction CloseCommentToolbarExtension] refineApp_
      clickNewComment wrapper
      clickArticleContent wrapper
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension__btn-new-ann-text")) `shouldReturn` (0 :: Int)
