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
  , PreToken(..), ContributionKind, runPreToken, dropPreTokens
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
import           Data.List (foldl')
import           Data.Maybe (listToMaybe)
import           Data.String.Conversions (ST, cs, (<>))
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Data.Void
import           GHC.Generics (Generic)
import           Text.HTML.Parser (Token(..), Attr(..))
import           Text.HTML.Tree (tokensFromForest)
import           Text.Read (readMaybe)
import           Web.HttpApiData (toUrlPiece, parseUrlPiece, FromHttpApiData(..))

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
--
-- @(ID Void)@ is taken from 'ChunkRange' and refers to the contribution that causes this mark token
-- pair.
data PreToken = PreToken Token | PreMarkOpen (ID Void) ContributionKind | PreMarkClose (ID Void)
  deriving (Eq, Show, Generic)

-- | Needed to make some of the helper functions total.
type PreToken' = (ID Void, ContributionKind)

-- | Needed to make some of the helper functions total.
data PreToken'' = PreMarkOpen'' (ID Void) ContributionKind | PreMarkClose'' (ID Void)
  deriving (Show)

runPreToken'' :: PreToken'' -> PreToken
runPreToken'' (PreMarkOpen'' n o) = PreMarkOpen n o
runPreToken'' (PreMarkClose'' n)  = PreMarkClose n


runPreToken :: PreToken -> Token
runPreToken (PreToken t)      = t
runPreToken (PreMarkOpen l k) = TagOpen "mark"
  [ Attr "data-contribution-id"   $ toUrlPiece l
  , Attr "data-contribution-kind" $ toUrlPiece k
  ]
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
stashPreToken (PreMarkOpen l k) = Doctype $ ST.intercalate "/" [toUrlPiece l, toUrlPiece k]
stashPreToken (PreMarkClose l)  = Doctype $ toUrlPiece l
stashPreToken (PreToken t)      = t

unstashPreToken :: Token -> PreToken
unstashPreToken (Doctype s) = case ST.splitOn "/" s of
  [l, k] -> PreMarkOpen (confidentread l) (confidentread k)
  [l]    -> PreMarkClose (confidentread l)
  bad    -> error $ "unstashPreToken: " <> show bad
  where
    confidentread :: FromHttpApiData a => ST -> a
    confidentread = (\(Right v) -> v) . parseUrlPiece
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
dataUidOfToken = \case
  (TagOpen _ attrs)      -> go attrs
  (TagSelfClose _ attrs) -> go attrs
  _                      -> Nothing
  where
    go attrs = readMaybe . cs =<< listToMaybe (mconcat $ (\(Attr k v) -> [v | k == "data-uid"]) <$> attrs)

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
