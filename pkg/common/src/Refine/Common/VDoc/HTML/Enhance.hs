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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.HTML.Enhance
  ( addUIInfoToVDocVersion

  -- mainly for testing:
  , addUIInfoToForest
  , addDataUidsToForest
  , addDataUidsToTree
  , addOffsetsToForest
  , addOffsetsToForest_
  , addOffsetsToTree
  ) where

import           Data.String.Conversions (ST)
import qualified Data.Text as ST
import           Data.Tree
import           Text.HTML.Parser

import           Refine.Common.Types
import           Refine.Common.VDoc.HTML.Canonicalize (canonicalizeAttrs)


-- | Make sure all open tags have a @data-uid@ attribute (if missing: inherit from first suitable
-- ancestor) and a @data-offset@ attribute that contains the text offset relative to that ancestor.
-- (i.e., we traverse the left siblings and their descendants and compute the sum of the text
-- lenghts.)
--
-- It is an error to call this with a forest that has non-tag top-level elements.  Always pass data
-- through `canonicalizeVDocVersion` (or more directly through 'wrapInTopLevelTags') to make sure
-- this does not happen.
--
-- Input must satisfy this property: all non-mark tags have data-uid attributes.  Output satisfies
-- this property: all tags (mark and non-mark) have data-uid attributes.
--
-- This function should probably be called on (the forest contained in) @VDocVersion
-- 'HTMLWithMarks@, and probably only in the frontend.
addUIInfoToVDocVersion :: VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLWithMarks
addUIInfoToVDocVersion (VDocVersion forest) = VDocVersion (addUIInfoToForest forest)

addUIInfoToForest :: Forest Token -> Forest Token
addUIInfoToForest = addDataUidsToForest . addOffsetsToForest

addDataUidsToForest :: Forest Token -> Forest Token
addDataUidsToForest = fmap (addDataUidsToTree Nothing)

addDataUidsToTree :: Maybe ST -> Tree Token -> Tree Token
addDataUidsToTree muid (Node t children) = Node t' (addDataUidsToTree muid' <$> children)
  where
    (t', muid') = case t of
      TagOpen tagname@"mark" attrs -> case muid of
        Just i  -> (TagOpen tagname (canonicalizeAttrs $ Attr "data-uid" i : attrs), muid)
        Nothing -> error "addDataUidsToTree: mark tag without surrounding tag that carries data-uid."
      TagOpen _ attrs -> case findAttrUidIn attrs of
        Just i  -> (t, Just i)
        Nothing -> error "addDataUidsToTree: non-mark tag without data-uid."
      _ -> (t, muid)

findAttrUidIn :: [Attr] -> Maybe ST
findAttrUidIn [] = Nothing
findAttrUidIn (Attr "data-uid" value : _) = Just value
findAttrUidIn (_ : as) = findAttrUidIn as


addOffsetsToForest :: Forest Token -> Forest Token
addOffsetsToForest = fst . addOffsetsToForest_ 0

addOffsetsToForest_ :: Int -> Forest Token -> (Forest Token, Int)
addOffsetsToForest_ offset [] = ([], offset)
addOffsetsToForest_ offset (n@(Node (ContentText t) []) : trees) =
  let (newForest, newOffset) = addOffsetsToForest_ (offset + ST.length t) trees
  in (n : newForest, newOffset)
addOffsetsToForest_ _ (Node (ContentChar _) _ : _) =
  error "addOffsetsToForest_: non-canonical tree."
addOffsetsToForest_ offset (n : trees) =
  let (firstTree, firstOffset) = addOffsetsToTree offset n
      (newForest, newOffset) = addOffsetsToForest_ firstOffset trees
  in (firstTree : newForest, newOffset)


addOffsetsToTree :: Int -> Tree Token -> (Tree Token, Int)
addOffsetsToTree offset (Node (TagOpen tagname attrs) children) =
  let newAttrs = canonicalizeAttrs $ Attr "data-offset" (ST.pack (show zeroOrOffset)) : attrs
      zeroOrOffset = if tagname == "mark" then offset else 0
      (newChildren, newOffset) = addOffsetsToForest_ zeroOrOffset children
  in (Node (TagOpen tagname newAttrs) newChildren, newOffset)

addOffsetsToTree offset (Node t children) =
  let (newChildren, newOffset) = addOffsetsToForest_ offset children
  in (Node t newChildren, newOffset)
