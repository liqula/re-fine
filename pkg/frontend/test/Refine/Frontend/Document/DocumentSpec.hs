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

module Refine.Frontend.Document.DocumentSpec
where

import Test.Hspec
--import Test.QuickCheck

import Refine.Common.Test.Arbitrary ()
--import Refine.Common.Types
--import Refine.Common.VDoc.HTML
import Refine.Frontend.Document.Document
import Refine.Frontend.Test.Enzyme


spec :: Spec
spec = do
  describe "Document" $ do
    it "renders with empty content" $ do
      pending
      wrapper <- shallow $ editorWrapper_ (EditorWrapperProps Nothing)
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` (1 :: Int)

{- FIXME compile error due to changed property types - not clear to me how this should be set up with PBT
    it "renders with arbitrary content" . property $ \(insertMarks ([] :: [Contribution]) -> vers) -> do
      pending
      wrapper <- shallow $ editorWrapper_ (EditorWrapperProps vers)
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` (1 :: Int)
-}
