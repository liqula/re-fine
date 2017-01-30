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

module Refine.Common.VDoc.HTML.Core
  ( -- * errors
    VDocHTMLError(..), ChunkRangeError(..)

    -- * pretokens
  , PreToken(..), runPreToken, dropPreTokens

    -- * misc
  , atNode
  , dataUidOfToken, dataUidOfPreToken
  , tokensToForest'

  , tokenTextLength
  , forestTextLength
  , preTokenTextLength
  , preForestTextLength

  , preTokensToForest
  , preTokensFromForest
  ) where

import           Control.Lens (Traversal')
import           Control.Monad.Error.Class (MonadError, throwError)
import           Data.Char (toLower)
import           Data.Maybe (listToMaybe)
import           Data.String.Conversions (ST, cs, (<>))
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..))
import           Text.HTML.Tree (ParseTokenForestError, tokensToForest, tokensFromForest)
import           Text.Read (readMaybe)

import Refine.Common.Types


-- * errors

data VDocHTMLError =
    VDocHTMLErrorBadTree ParseTokenForestError
  | VDocHTMLErrorChunkRangeErrors [ChunkRangeError]
  | VDocHTMLErrorBadChunkPoint (Forest PreToken) ChunkPoint  -- (should be an impossible error.)
  | VDocHTMLErrorNotEnoughCharsToSplit Int (Forest PreToken)
  | VDocHTMLErrorSplitPointsToSubtree Int (Forest PreToken)
  | VDocHTMLErrorInternal String
  deriving (Eq, Show)

data ChunkRangeError =
    ChunkRangeEmpty [Token] (Maybe ChunkPoint) (Maybe ChunkPoint)
  | ChunkRangeBadEndNode [Token] (Maybe ChunkPoint) (Maybe ChunkPoint)
  | ChunkRangeOffsetTooLarge [Token] ChunkPoint
  | ChunkRangeNotATree [Token]
  deriving (Eq, Show)


-- * pretokens

-- | this type is introduced because we keep open and close marks separately in the tree at some
-- point.  in order to keep track of which close mark belongs to which open mark, we cannot rely on
-- their order (which still needs to be de-overlapped), so need close marks of the form
-- @PreMarkClose "data-chunk-id-value"@.
data PreToken = PreToken Token | PreMarkOpen ST ST | PreMarkClose ST
  deriving (Eq, Show)

runPreToken :: PreToken -> Token
runPreToken (PreToken t)      = t
runPreToken (PreMarkOpen l k) = TagOpen "mark" [Attr "data-chunk-id" l, Attr "data-chunk-kind" $ ST.map toLower k]
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

-- | Inverse of 'preTokensFromForest' (equally hacky).
preTokensToForest :: MonadError VDocHTMLError m => [PreToken] -> m (Forest PreToken)
preTokensToForest = fmap (fmap (fmap unstashPreToken)) . tokensToForest' . fmap stashPreToken

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

-- | 'Traversal' optics to find all sub-trees with a given `data-uid` html attribute.  Takes time
-- linear in size of document for find the sub-tree.
--
-- NOTE: I think if a sub-tree of a matching sub-tree matches again, that will go unnoticed.  In
-- 'atDataUID' (where always only one node must satisfy the predicate) this isn't an issue.
atNode :: (a -> Bool) -> Traversal' (Forest a) (Forest a)
atNode prop focus = dive
  where
    dive [] = pure []
    dive (Node n chs : ts)
      = if prop n
          then (:) <$> (Node n <$> focus chs) <*> dive ts
          else (:) <$> (Node n <$> dive  chs) <*> dive ts

dataUidOfToken :: Token -> Maybe DataUID
dataUidOfToken (TagOpen _ attrs) =
  readMaybe . cs =<< listToMaybe (mconcat $ (\(Attr k v) -> [v | k == "data-uid"]) <$> attrs)
dataUidOfToken _ = Nothing

dataUidOfPreToken :: PreToken -> Maybe DataUID
dataUidOfPreToken (PreToken t)      = dataUidOfToken t
dataUidOfPreToken (PreMarkOpen _ _) = Nothing
dataUidOfPreToken (PreMarkClose _)  = Nothing


-- | Call 'tokensToForest' and convert the error type.
tokensToForest' :: MonadError VDocHTMLError m => [Token] -> m (Forest Token)
tokensToForest' ts = case tokensToForest ts of
  Right f  -> pure f
  Left msg -> throwError $ VDocHTMLErrorBadTree msg


tokenTextLength :: Token -> Int
tokenTextLength = \case
  (ContentText t) -> ST.length t
  (ContentChar _) -> 1
  _               -> 0

forestTextLength :: Forest Token -> Int
forestTextLength = sum . fmap tokenTextLength . tokensFromForest

preTokenTextLength :: PreToken -> Int
preTokenTextLength = tokenTextLength . runPreToken

preForestTextLength :: Forest PreToken -> Int
preForestTextLength = sum . fmap preTokenTextLength . preTokensFromForest
