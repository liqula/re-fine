{-# LANGUAGE CPP #-}
#include "language_backend.hs"
module Refine.Backend.Database.Tree where
#include "import_backend.hs"

import Control.Arrow ((&&&), first)
import Data.Tree (Tree)


-- | Build a tree from a list of values.
-- ASSUMPTIONS:
--  * The values can be distingueshed from each other via the `key` function
--  * There is only one root element that has no parent.
buildTree :: (Ord k, Show k) => (a -> Maybe k) -> (a -> k) -> [a] -> Tree a
buildTree parent key xs = node root
  where
    node n = Tree.Node n (node <$> children n)

    children v = fromMaybe [] $ Map.lookup (key v) childrenMap

    root = case filter (isNothing . parent) xs of
      [x] -> x
      _ -> assert False $ error "buildTree: internal error."

    childrenMap =
      Map.unionsWith (++) -- O(m*log(n/m + 1))
      . map (uncurry Map.singleton . first fromJust) -- O(n)
      . filter (isJust . fst)    -- O(n)
      $ map (parent &&& pure) xs -- O(n)
