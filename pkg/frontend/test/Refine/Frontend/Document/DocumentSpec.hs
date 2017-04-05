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
import Test.QuickCheck

import Refine.Common.Test.Arbitrary ()
import Refine.Common.Types
import Refine.Frontend.Document.Document
import Refine.Frontend.Document.Store
import Refine.Frontend.Document.Types
import Refine.Frontend.Test.Enzyme


spec :: Spec
spec = do
  describe "Document" $ do
    let mkTestProps forest = EditorProps (createEditorState Grammar (VDocVersion forest))

    it "renders with empty content" $ do
      pending
      wrapper <- shallow $ editor_ (mkTestProps [])
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` (1 :: Int)

    it "renders with arbitrary content" . property $ \(VDocVersion forest :: VDocVersion 'HTMLCanonical) -> do
      pending
      wrapper <- shallow $ editor_ (mkTestProps forest)
      lengthOfIO (find wrapper (StringSelector ".editor_wrapper")) `shouldReturn` (1 :: Int)
