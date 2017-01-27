{-# LANGUAGE LambdaCase #-}

module Refine.Backend.Database.TreeSpec where

import           Data.List
import           Data.Tree
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Map as Map

import Refine.Backend.Database.Tree (buildTree)


spec :: Spec
spec = do
  describe "buildTree" . it "builds the same tree as the given reference" .
    property . forAll (genTree (arbitrary :: Gen Int)) $ \tree -> do
      let elems   = flatten tree
      let parents = parentCodings tree
      buildTree parents id elems `shouldBe` tree

-- * Helpers

-- | Generates a tree with unique nodes
genTree :: (Show a, Eq a) => Gen a -> Gen (Tree a)
genTree gen = (nub <$> listOf1 gen) >>= tree
  where
    tree []  = error "genTree: impossible"
    tree [a] = pure $ Node a []
    tree (a:as) = sized $ \case
      0 -> pure $ Node a []
      n -> do aas <- splitToNonEmptyLists as
              Node a <$> mapM (resize (n `div` 2) . tree) aas

splitToNonEmptyLists :: [a] -> Gen [[a]]
splitToNonEmptyLists [] = error "splitToNonEmptyLists: got empty list"
splitToNonEmptyLists as = do
  ns <- series (length as)
  pure $ split ns as
  where
    series :: Int -> Gen [Int]
    series 0 = pure []
    series n = choose (1, n) >>= \x -> (x:) <$> series (n - x)

    split []     [] = []
    split []     _  = error "splitToNonEmptyLists: split: impossible"
    split (n:ns) bs = uncurry (\h t -> h : split ns t)
                              (splitAt n bs)

parentCodings :: (Ord a) => Tree a -> (a -> Maybe a)
parentCodings tree a = Map.lookup a (childrenMap tree)
  where
    childrenMap (Node _ []) = Map.empty
    childrenMap (Node x cs) =
      Map.fromList ((rootLabel <$> cs) `zip` repeat x)
      `Map.union`
      Map.unions (childrenMap <$> cs)
