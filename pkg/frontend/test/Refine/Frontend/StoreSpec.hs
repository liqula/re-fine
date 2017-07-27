{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.StoreSpec where

import Refine.Frontend.Prelude

import Control.Lens ((^.))
import Data.Aeson (encode)
import React.Flux (transform)
import Test.Hspec
import Text.Read (readMaybe)

import Refine.Common.Types
import Refine.Frontend.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Store ()
import Refine.Frontend.Store.Types


spec :: Spec
spec = do
  describe "Store" $ do
    describe "transform" $ do
      context "gsVDocList" $ do
        it "integrates the loaded document list into the store" $ do
          let list = [ID 3]
          result <- transform (LoadDocumentList $ AfterAjax list) emptyGlobalState
          result ^. gsVDocList `shouldBe` Just list

      context "gsScreenState" $ do
        it "adds the header height to the state" $ do
          newState <- transform (ScreenAction (AddHeaderHeight 64)) emptyGlobalState
          _ssHeaderHeight (_gsScreenState newState) `shouldBe` (64 :: Int)


  describe "issue #242" $ do
    it "ffi throw an exception when marshalling Ints fails" $ do
      js_reproduce_issue_242            `shouldReturn` 123

    it "ffi throw an exception when marshalling Ints fails (+1)" $ do
      ((+1) <$> js_reproduce_issue_242) `shouldReturn` 124

    it "ffi throw an exception when marshalling Ints fails (show)" $ do
      (show <$> js_reproduce_issue_242) `shouldReturn` "123"

    it "ffi throw an exception when marshalling Ints fails (Aeson.encode)" $ do
      (encode <$> js_reproduce_issue_242) `shouldReturn` "123"

    it "readMaybe" $ do
      readMaybe "123.123" `shouldBe` (Nothing :: Maybe Int)

    it "read" $ do
      print (read "123.123" :: Int) `shouldThrow` anyException

#ifdef __GHCJS__

foreign import javascript safe
  "123.456"
  js_reproduce_issue_242 :: IO Int

#else

{-# ANN js_reproduce_issue_242 ("HLint: ignore Use camelCase" :: String) #-}
js_reproduce_issue_242 :: IO Int
js_reproduce_issue_242 = error "javascript FFI not available in GHC"

#endif
