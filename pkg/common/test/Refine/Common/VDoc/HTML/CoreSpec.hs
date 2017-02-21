{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.HTML.CoreSpec where

import           Control.Exception (assert)
import           Control.Lens
import           Data.Functor.Compose
import           Data.String.Conversions ((<>))
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser

import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}


spec :: Spec
spec = parallel $ do
  describe "atNode" $ do
    it "view works" $ do
        let forest = [ Node (PreToken (TagOpen "p" [Attr "data-uid" "1"]))
                            [Node (PreToken (ContentText "ouph")) []]
                     , Node (PreToken (TagOpen "ol" [Attr "data-uid" "2"]))
                            [Node (PreToken (TagOpen "li" [Attr "data-uid" "3"]))
                                  ( Node (PreToken (ContentText "blue")) []
                                  : subf
                                  )
                            ]
                     ]
            subf = [ Node (PreToken (TagOpen "li" [Attr "data-uid" "4"]))
                          [Node {rootLabel = PreToken (ContentText "green"), subForest = []}]]
        forest ^. atPreToken (DataUID 4) `shouldBe` subf

    it "set works" $ do
        let forest  = [Node (TagOpen "a" [Attr "data-uid" "7"]) []]
            forest' = [Node (TagOpen "a" [Attr "data-uid" "7"]) [Node (TagOpen "x" []) []]]
            uid     = DataUID 7

            l :: Traversal' (Forest Token) (Forest Token)
            l = atToken uid

        view l (set l forest' forest) `shouldBe` forest'

    it "over works" $ do
        let forestOld     = [Node (ContentText "sdf") []] <> forestSubOld <> [Node (ContentText "123") []]
            forestSubOld  = [Node (TagOpen "a" [Attr "data-uid" "7"]) [Node (TagOpen "q" []) []]]
            forestSubNew  = [Node (TagOpen "c" [Attr "data-uid" "7"]) [Node (TagOpen "x" []) []]]

            l :: Traversal' (Forest Token) (Forest Token)
            l = atToken (DataUID 7)

        view l (over l (\forestSubOld' -> assert (forestSubOld' == forestSubOld) forestSubNew) forestOld)
          `shouldBe` forestSubNew


    describe "it's a lens if data-uid exists and is unique." $ do
      it "[lens-identity] You get back what you put in" . property $
        \(CanonicalVDocVersionPairWithDataUID (forest, uid) forest') -> do
          let l :: Traversal' (Forest Token) (Forest Token)
              l = atToken uid
              forest'' = case view l forest of
                [Node n _] -> [Node n forest']
                _ -> error "this should not be."
          view l (set l forest'' forest) `shouldBe` forest''

      it "[lens-identity'] Putting back what you got doesn't change anything" . property $
        \(CanonicalVDocVersionPairWithDataUID (forest, uid) _) -> do
          let l :: Traversal' (Forest Token) (Forest Token)
              l = atToken uid
          set l (view l forest) forest `shouldBe` forest

      it "[lens-idempotency] Setting twice is the same as setting once" . property $
        \(CanonicalVDocVersionPairWithDataUID (forest, uid) forest') -> do
          let l :: Traversal' (Forest Token) (Forest Token)
              l = atToken uid
              forest'' = case view l forest of
                [Node n _] -> [Node n forest']
                _ -> error "this should not be."
          set l forest'' (set l forest'' forest) `shouldBe` set l forest'' forest

    describe "it's also a traversal." $ do
      it "[traversal-identity] Traversing the identity doesn't change anything" . property $
        \(CanonicalVDocVersionPairWithDataUID (forest, uid) _) -> do
          let t :: Traversal' (Forest Token) (Forest Token)
              t = atToken uid
          t Identity forest `shouldBe` Identity forest

      it "[traversal-composition] Composition and traversal can be flipped" . property $
        \(CanonicalVDocVersionPairWithDataUID (forest, uid) _) -> do
          let t :: Traversal' (Forest Token) (Forest Token)
              t = atToken uid
              g = Identity
              f0 = const Nothing
              f1 = Just

              traverseThenCompose f = fmap (t f) . t g
              composeThenTraverse f = getCompose . t (Compose . fmap f . g)

          traverseThenCompose f0 forest `shouldBe` composeThenTraverse f0 forest
          traverseThenCompose f1 forest `shouldBe` composeThenTraverse f1 forest
