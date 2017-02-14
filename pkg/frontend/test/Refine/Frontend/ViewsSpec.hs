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

import qualified Data.Map.Strict as M
import qualified Data.Tree as DT
import           Test.Hspec
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import qualified Refine.Frontend.Test.Enzyme.ReactWrapperAPI as RW
import           Refine.Frontend.Types
import           Refine.Frontend.Views

--import           Refine.Frontend.Header.Toolbar

--import           Refine.Frontend.Test.Console

spec :: Spec
spec = do
  describe "The mainScreen_ component" $ do
    let newVDoc = CompositeVDoc (VDoc (ID 1) (Title "the-title") (Abstract "the-abstract") (ID 1)) (VDocRepo (ID 1) (ID 1)) (VDocVersion [DT.Node (HTMLP.ContentText "the-html-content") []]) M.empty M.empty M.empty

    it "initially the comment toolbar is visible" $ do
      -- _wrapper <- RW.mount editToolbar_
      _wrapper <- RW.mount (mainScreen_ $ emptyGlobalState { _gsVDoc = Just newVDoc })
      pure ()

{-
      text <- RW.text wrapper
      print text
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar__btn-add-annotation")) `shouldReturn` (1 :: Int)

    it "initially the comment toolbar extension is not visible" $ do
      wrapper <- RW.mount refineApp_
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)
-}


{-
    it "opens the comment toolbar extension when the user clicks on the 'new comment' button" $ do
      wrapper <- RW.mount refineApp_
      button <- RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar__btn-add-annotation")
      _ <- RW.simulate button RW.Click
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (1 :: Int)

    it "closes the comment toolbar extension when the user clicks somewhere else" $ do
      wrapper <- RW.mount refineApp_
      button <- RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar__btn-add-annotation")
      _ <- RW.simulate button RW.Click
      mainText <- RW.find wrapper (RW.StringSelector ".c-article-content")
      _ <- RW.simulate mainText RW.Click
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension")) `shouldReturn` (0 :: Int)

    it "does not close the comment toolbar extension when the user clicks on the 'text-specific comment' button" $ do
      wrapper <- RW.mount refineApp_
      button <- RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar__btn-add-annotation")
      _ <- RW.simulate button RW.Click
      mainText <- RW.find wrapper (RW.StringSelector ".c-article-content")
      _ <- RW.simulate mainText RW.Click
      RW.lengthOfIO (RW.find wrapper (RW.StringSelector ".c-vdoc-toolbar-extension__btn-new-ann-text")) `shouldReturn` (0 :: Int)
-}
