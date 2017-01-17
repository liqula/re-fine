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

module Refine.Common.VDoc.HTML.Canonicalize
  ( canonicalizeVDocVersion
  , decanonicalizeVDocVersion
  , canonicalizeWhitespace
  , setElemUIDs
  , wrapInTopLevelTags
  , trickledownUIInfo
  , canonicalizeAttrsForest, canonicalizeAttrsTree, canonicalizeAttrsStream, canonicalizeAttrs
  ) where

import           Control.Exception (assert)
import           Control.Lens ((%~), _1)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.State (State, runState, modify, get)
import           Data.Char (isSpace)
import           Data.Function (on)
import           Data.List as List (foldl', sort, nubBy)
import           Data.Set as Set hiding (foldl')
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..), parseTokens, canonicalizeTokens, renderTokens)
import           Text.HTML.Tree (tokensFromForest)

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core


-- | Does several things.
--
-- 1. Canonicalize whitespace and merge neighboring text tokens.  (See test suite for details.)
-- 2. Refresh @data-uid@ attributes in all tags.  Keep in mind that @data-uid@ are only meaningful
--    in the scope of their (immutable) @VDocVersion@.  Values may change for unchanging nodes
--    between versions.
-- 3. wrap all top-level text nodes into @<span>@ tags (so 'ChunkPoint' always has a @data-uid@
--    value to point to.
canonicalizeVDocVersion :: MonadError VDocHTMLError m
                        => VDocVersion 'HTMLRaw -> m (VDocVersion 'HTMLCanonical)
canonicalizeVDocVersion = fmap (VDocVersion . cs . renderTokens) . canonicalize . parseTokens . _unVDocVersion
  where
    canonicalize
      = wrapInTopLevelTags
      . setElemUIDs
      . fmap (onText canonicalizeWhitespace) . canonicalizeTokens

-- | De-canonicalizing is always safe, since every canonicalized 'VDocVersion' is also a raw one.
decanonicalizeVDocVersion :: VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLRaw
decanonicalizeVDocVersion (VDocVersion s) = VDocVersion s

onText :: (ST -> ST) -> Token -> Token
onText f (ContentText s) = ContentText $ f s
onText _ t = t

canonicalizeWhitespace :: ST -> ST
canonicalizeWhitespace t = leading t <> ST.intercalate "\n" (ST.words t) <> trailing t
  where
    leading s = if ST.all isSpace (ST.take 1 s) then "\n" else ""
    trailing = leading . ST.reverse


-- | Remove all existing @data-uid@ attributes and add new attributes with fresh unique values to
-- all tags.
setElemUIDs :: [Token] ->  [Token]
setElemUIDs = fill mempty . fmap clear
  where
    clear :: Token -> Token
    clear (TagOpen n xs) = TagOpen n (Prelude.filter stale xs)
    clear t              = t

    stale :: Attr -> Bool
    stale (Attr "data-uid" _) = False
    stale _                   = True

    fill :: Set DataUID -> [Token] -> [Token]
    fill already = reverse . snd . foldl' f (totalMaximum already, [])
      where
        f :: (DataUID, [Token]) -> Token -> (DataUID, [Token])
        f (next, acc) = \case
          (TagOpen n attrs) -> (nextDataUID next, TagOpen n (Attr "data-uid" (cs $ show next) : attrs) : acc)
          t                 -> (next,             t : acc)

        totalMaximum s = if Set.null s then DataUID 1 else nextDataUID (Set.findMax s)
        nextDataUID (DataUID n) = DataUID (n + 1)


wrapInTopLevelTags :: MonadError VDocHTMLError m => [Token] -> m [Token]
wrapInTopLevelTags ts = assert (ts == canonicalizeTokens ts)
                      $ tokensFromForest . fmap fill <$> tokensToForest' ts
  where
    defaultTag :: Token
    defaultTag = TagOpen "span" []

    fill :: Tree Token -> Tree Token
    fill n@(Node (ContentText _) _) = Node defaultTag [n]
    fill n                          = n


-- | Make sure all open tags have a @data-uid@ attribute (if missing: inherit from first suitable
-- ancestor) and a @data-offset@ attribute that contains the text offset relative to that ancestor.
-- (i.e., we traverse the left siblings and their descendents and compute the sum of the text
-- lenghts.)
--
-- This function should probably be called on (the forest contained in) @VDocVersion
-- 'HTMLWithMarks@, and probably only in the frontend.
--
-- TODO: it's probably better to write two functions for data-uid, data-offset and compose them.
-- also, there are probably much simpler algorithms to do this.  we probably don't need State.
trickledownUIInfo :: Forest Token -> Forest Token
trickledownUIInfo forest = assert (tokensFromForest forest == canonicalizeTokens (tokensFromForest forest))
                         . assert (Right (tokensFromForest forest) == wrapInTopLevelTags (tokensFromForest forest))
                         . fst . fst . (`runState` [])
                         $ doforest 0 forest
  where
    overwriteAttrT :: Attr -> Token -> Token
    overwriteAttrT attr (TagOpen n attrs) = canonicalizeAttrs $ TagOpen n (attr : attrs)
    overwriteAttrT _    t                 = t

    overwriteAttrN :: Attr -> Tree Token -> Tree Token
    overwriteAttrN attr (Node t xs) = Node (overwriteAttrT attr t) xs

    doforest :: Int -> [Tree Token] -> State [DataUID] ([Tree Token], Int)
    doforest acc [] = pure ([], acc)
    doforest acc (t : ts) = do
      (t', l) <- dotree t
      let t'' = overwriteAttrN (Attr "data-offset" (cs $ show acc)) t'
      (_1 %~ (t'' :)) <$> doforest (acc + l) ts

    dotree :: Tree Token -> State [DataUID] (Tree Token, Int)
    dotree t@(Node (ContentText s) []) = do
      pure (t, ST.length s)

    dotree (Node x@(TagOpen _ _) xs) = do
      let push :: [DataUID] -> [DataUID]
          push stack = new : stack
            where
              new = case (dataUidOfToken x, stack) of
                      (Just uid, _)       -> uid
                      (Nothing,  uid : _) -> uid
                      (Nothing,  [])      -> error "trickledownUIInfo: call wrapInTopLevelTags first!"

          pop :: [DataUID] -> [DataUID]
          pop (_ : stack) = stack
          pop [] = error "impossible."

      modify push

      (xs', l) <- doforest 0 xs
      (new : _) <- get

      modify pop

      let x' = overwriteAttrT (Attr "data-uid" (cs $ show new)) x
          n' = Node x' xs'
      pure (n', l)

    dotree n = pure (n, 0)


-- | Call 'canonicalizeAttrs' on a token 'Forest'.
canonicalizeAttrsForest :: Forest Token -> Forest Token
canonicalizeAttrsForest = fmap canonicalizeAttrsTree

-- | Call 'canonicalizeAttrs' on a token 'Tree'.
canonicalizeAttrsTree :: Tree Token -> Tree Token
canonicalizeAttrsTree (Node x xs) = Node (canonicalizeAttrs x) (canonicalizeAttrsForest xs)

-- | Call 'canonicalizeAttrs' on a token list.
canonicalizeAttrsStream :: [Token] -> [Token]
canonicalizeAttrsStream = fmap canonicalizeAttrs

-- | If one attribute key occurs several times, use the first occurrance and drop the rest.  Sort
-- the resulting list.
canonicalizeAttrs :: Token -> Token
canonicalizeAttrs (TagOpen n xs) = TagOpen n (sort $ nubBy ((==) `on` (\(Attr k _) -> k)) xs)
canonicalizeAttrs t = t
