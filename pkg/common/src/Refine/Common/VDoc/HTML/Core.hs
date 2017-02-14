{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
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

module Refine.Common.VDoc.HTML.Core
  ( -- * errors
    ChunkRangeError(..)
  , _ChunkRangeBadDataUID
  , _ChunkRangeOffsetOutOfBounds
  , _ChunkRangeEmpty

    -- * pretokens
  , PreToken(..), DataChunkID, OwnerKind, runPreToken, dropPreTokens
  , PreToken', PreToken''(..), runPreToken''

    -- * misc
  , atNode, atToken, atPreToken
  , dataUidOfToken, dataUidOfPreToken

  , tokenTextLength
  , treeTextLength
  , forestTextLength
  , preTokenTextLength
  , preTreeTextLength
  , preForestTextLength

  , preTokensFromForest
  ) where

import           Control.Lens (Traversal')
import           Data.Char (toLower)
import           Data.List (foldl')
import           Data.Maybe (listToMaybe)
import           Data.String.Conversions (ST, cs, (<>))
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           GHC.Generics (Generic)
import           Text.HTML.Parser (Token(..), Attr(..))
import           Text.HTML.Tree (tokensFromForest)
import           Text.Read (readMaybe)

import Refine.Common.Types
import Refine.Prelude.TH (makeRefineType)


-- * errors

data ChunkRangeError =
    ChunkRangeBadDataUID ChunkPoint (Forest Token)
  | ChunkRangeOffsetOutOfBounds ChunkPoint (Forest Token)
  | ChunkRangeEmpty (Maybe ChunkPoint) (Maybe ChunkPoint) (Forest Token)
  deriving (Eq, Show, Generic)


-- * pretokens

-- | this type is introduced because we keep open and close marks separately in the tree at some
-- point.  in order to keep track of which close mark belongs to which open mark, we cannot rely on
-- their order (which still needs to be de-overlapped), so need close marks of the form
-- @PreMarkClose "data-contribution-id-value"@.
data PreToken = PreToken Token | PreMarkOpen DataChunkID OwnerKind | PreMarkClose DataChunkID
  deriving (Eq, Show, Generic)

-- | 'ID' of 'Edit' or 'Comment' that owns a 'ChunkRange'.  This is the @data-chunk-id@ attribute value
-- of the @mark@ tag corresponding to the 'ChunkRange' in the dom.
type DataChunkID = ST  -- FIXME: @newtype DataChunkID (forall a . Eq a => ID a)@
                       -- FIXME: nope, this should be @ID a@, if possible.

-- | This is the @data-chunk-owner@ attribute value of the @mark@ tag corresponding to the
-- 'ChunkRange' in the dom.  It can be either @edit@, @note@, @question@, @discussion@, or something
-- like that.
type OwnerKind = ST    -- FIXME: @type OwnerKind = TypeRep  -- e.g. @ID Edit@@

-- | Needed to make some of the helper functions total.
type PreToken' = (DataChunkID, OwnerKind)

-- | Needed to make some of the helper functions total.
data PreToken'' = PreMarkOpen'' DataChunkID OwnerKind | PreMarkClose'' DataChunkID
  deriving (Show)

runPreToken'' :: PreToken'' -> PreToken
runPreToken'' (PreMarkOpen'' n o) = PreMarkOpen n o
runPreToken'' (PreMarkClose'' n)  = PreMarkClose n


runPreToken :: PreToken -> Token
runPreToken (PreToken t)      = t
runPreToken (PreMarkOpen l k) = TagOpen "mark" [Attr "data-contribution-id" l, Attr "data-contribution-kind" $ ST.map toLower k]
runPreToken (PreMarkClose _)  = TagClose "mark"

dropPreTokens :: [PreToken] -> [Token]
dropPreTokens = fmap runPreToken . filter (\case (PreToken _) -> True; _ -> False)


-- | This is a bit of a hack.  We want to use tokensFromForest, so we 'fmap' 'runPreToken' on the
-- input forest, then render that, and recover the pretokens from the tokens.
--
-- The encoding uses 'Doctype' to encode marks, which works because all doctype elements have been
-- removed from the input by 'canonicalizeVDocVersion'.
preTokensFromForest :: Forest PreToken -> [PreToken]
preTokensFromForest = fmap unstashPreToken . tokensFromForest . fmap (fmap stashPreToken)

stashPreToken :: PreToken -> Token
stashPreToken (PreMarkOpen l k) = Doctype $ ST.intercalate "/" [l, k]
stashPreToken (PreMarkClose l)  = Doctype l
stashPreToken (PreToken t)      = t

unstashPreToken :: Token -> PreToken
unstashPreToken (Doctype s) = case ST.splitOn "/" s of
  [l, k] -> PreMarkOpen l k
  [l]    -> PreMarkClose l
  bad    -> error $ "unstashPreToken: " <> show bad
unstashPreToken t           = PreToken t


-- * misc

-- | 'Traversal' optics to find all sub-trees with a given `data-uid` html attribute.  Sub-trees are
-- wrapped in singleton lists.  Takes time linear in size of document for find the sub-tree.
atNode :: (a -> Bool) -> Traversal' (Forest a) (Forest a)
atNode prop focus = dive
  where
    dive [] = pure []
    dive (Node n chs : ts)
      = if prop n
          then (<>) <$> focus [Node n chs]    <*> dive ts
          else (:)  <$> (Node n <$> dive chs) <*> dive ts

atToken :: DataUID -> Traversal' (Forest Token) (Forest Token)
atToken node = atNode (\p -> dataUidOfToken p == Just node)

atPreToken :: DataUID -> Traversal' (Forest PreToken) (Forest PreToken)
atPreToken node = atNode (\p -> dataUidOfPreToken p == Just node)

dataUidOfToken :: Token -> Maybe DataUID
dataUidOfToken (TagOpen _ attrs) =
  readMaybe . cs =<< listToMaybe (mconcat $ (\(Attr k v) -> [v | k == "data-uid"]) <$> attrs)
dataUidOfToken _ = Nothing

dataUidOfPreToken :: PreToken -> Maybe DataUID
dataUidOfPreToken (PreToken t)      = dataUidOfToken t
dataUidOfPreToken (PreMarkOpen _ _) = Nothing
dataUidOfPreToken (PreMarkClose _)  = Nothing


tokenTextLength :: Token -> Int
tokenTextLength = \case
  (ContentText t) -> ST.length t
  (ContentChar _) -> 1
  _               -> 0

treeTextLength :: Tree Token -> Int
treeTextLength = foldl' (+) 0 . fmap tokenTextLength

forestTextLength :: Forest Token -> Int
forestTextLength = sum . fmap treeTextLength

preTokenTextLength :: PreToken -> Int
preTokenTextLength = tokenTextLength . runPreToken

preTreeTextLength :: Tree PreToken -> Int
preTreeTextLength = treeTextLength . fmap runPreToken

preForestTextLength :: Forest PreToken -> Int
preForestTextLength = forestTextLength . fmap (fmap runPreToken)


-- * lots of instances

makeRefineType ''ChunkRangeError
makeRefineType ''PreToken
