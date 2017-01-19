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

module Refine.Common.VDoc.HTML.Enhance where

import           Data.Maybe (fromMaybe, isNothing)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Tree
import           Text.HTML.Parser


addDataUidsToForest :: Forest Token -> Forest Token
addDataUidsToForest = addDataUidsToForest_ ""

addDataUidsToForest_ :: T.Text -> Forest Token -> Forest Token
addDataUidsToForest_ uid forest = map (addDataUidsToTree uid) forest

addDataUidsToTree :: T.Text -> Tree Token -> Tree Token
addDataUidsToTree uid (Node (TagOpen tagname attrs) children) =
  let attrUid = findAttrUidIn attrs
      newUid = fromMaybe uid attrUid
      newAttrs = if isNothing attrUid then ((Attr (fromString "data-uid") uid):attrs) else attrs
  in Node (TagOpen tagname newAttrs) (addDataUidsToForest_ newUid children)
  where
    findAttrUidIn :: [Attr] -> Maybe T.Text
    findAttrUidIn [] = Nothing
    findAttrUidIn ((Attr "data-uid" value):_) = Just value
    findAttrUidIn (_:as) = findAttrUidIn as
addDataUidsToTree uid (Node t children) = (Node t (addDataUidsToForest_ uid children))


addOffsetsToForest :: Forest Token -> Forest Token
addOffsetsToForest = fst . addOffsetsToForest_ 0

addOffsetsToForest_ :: Int -> Forest Token -> (Forest Token, Int)
addOffsetsToForest_ offset [] = ([], offset)
addOffsetsToForest_ offset (n@(Node (ContentText t) []):trees) =
  let (newForest, newOffset) = addOffsetsToForest_ (offset + T.length t) trees
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
  let newAttrs = ((Attr "data-offset" (T.pack (show offset))):attrs)
      (newForest, newOffset) = addOffsetsToForest_ offset children
  in (Node (TagOpen "mark" newAttrs) newForest, newOffset)
addOffsetsToTree _ (Node (TagOpen tagname attrs) children) =
  let (newForest, newOffset) = addOffsetsToForest_ 0 children
  in (Node (TagOpen tagname ((Attr "data-offset" "0"):attrs)) newForest, newOffset)
addOffsetsToTree offset (Node t children) =
  let (newForest, newOffset) = (addOffsetsToForest_ offset children)
  in (Node t newForest, newOffset)

