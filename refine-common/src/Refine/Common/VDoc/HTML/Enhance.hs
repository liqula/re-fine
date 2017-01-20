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
  ( addUIInfoToForest
  -- mainly for testing:
  , addDataUidsToForest
  , addDataUidsToTree
  , addOffsetsToForest
  , addOffsetsToForest_
  , addOffsetsToTree
  ) where

import           Data.Maybe (fromMaybe, isNothing)
import           Data.String (fromString)
import           Data.String.Conversions (ST)
import qualified Data.Text as ST
import           Data.Tree
import           Text.HTML.Parser


-- | Make sure all open tags have a @data-uid@ attribute (if missing: inherit from first suitable
-- ancestor) and a @data-offset@ attribute that contains the text offset relative to that ancestor.
-- (i.e., we traverse the left siblings and their descendants and compute the sum of the text
-- lenghts.)
--
-- This function should probably be called on (the forest contained in) @VDocVersion
-- 'HTMLWithMarks@, and probably only in the frontend.
addUIInfoToForest :: Forest Token -> Forest Token
addUIInfoToForest = addDataUidsToForest . addOffsetsToForest

addDataUidsToForest :: Forest Token -> Forest Token
addDataUidsToForest = addDataUidsToForest_ ""

addDataUidsToForest_ :: ST -> Forest Token -> Forest Token
addDataUidsToForest_ uid = map (addDataUidsToTree uid)

addDataUidsToTree :: ST -> Tree Token -> Tree Token
addDataUidsToTree uid (Node (TagOpen tagname attrs) children) =
  let attrUid = findAttrUidIn attrs
      newUid = fromMaybe uid attrUid
      newAttrs = if isNothing attrUid then Attr (fromString "data-uid") uid:attrs else attrs
  in Node (TagOpen tagname newAttrs) (addDataUidsToForest_ newUid children)
  where
    findAttrUidIn :: [Attr] -> Maybe ST
    findAttrUidIn [] = Nothing
    findAttrUidIn (Attr "data-uid" value:_) = Just value
    findAttrUidIn (_:as) = findAttrUidIn as
addDataUidsToTree uid (Node t children) = Node t (addDataUidsToForest_ uid children)


addOffsetsToForest :: Forest Token -> Forest Token
addOffsetsToForest = fst . addOffsetsToForest_ 0

addOffsetsToForest_ :: Int -> Forest Token -> (Forest Token, Int)
addOffsetsToForest_ offset [] = ([], offset)
addOffsetsToForest_ offset (n@(Node (ContentText t) []):trees) =
  let (newForest, newOffset) = addOffsetsToForest_ (offset + ST.length t) trees
  in (n : newForest, newOffset)
addOffsetsToForest_ offset (n@(Node (ContentChar _) []):trees) =
  let (newForest, newOffset) = addOffsetsToForest_ (offset + 1) trees
  in (n : newForest, newOffset)
addOffsetsToForest_ offset (n:trees) =
  let (firstTree, firstOffset) = addOffsetsToTree offset n
      (newForest, newOffset) = addOffsetsToForest_ firstOffset trees
  in (firstTree : newForest, newOffset)


addOffsetsToTree :: Int -> Tree Token -> (Tree Token, Int)
addOffsetsToTree offset (Node (TagOpen "mark" attrs) children) =
  let newAttrs = (Attr "data-offset" (ST.pack (show offset)):attrs)
      (newForest, newOffset) = addOffsetsToForest_ offset children
  in (Node (TagOpen "mark" newAttrs) newForest, newOffset)
addOffsetsToTree _ (Node (TagOpen tagname attrs) children) =
  let (newForest, newOffset) = addOffsetsToForest_ 0 children
  in (Node (TagOpen tagname (Attr "data-offset" "0":attrs)) newForest, newOffset)
addOffsetsToTree offset (Node t children) =
  let (newForest, newOffset) = addOffsetsToForest_ offset children
  in (Node t newForest, newOffset)
