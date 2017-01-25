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
    VDocHTMLError(..)
  , PreToken(..)

    -- * misc
  , atNode
  , dataUidOfToken
  , tokensToForest'
  ) where

import           Control.Lens (Traversal')
import           Control.Monad.Error.Class (MonadError, throwError)
import           Data.Maybe (listToMaybe)
import           Data.String.Conversions (ST, cs)
import           Data.Tree (Forest, Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..))
import           Text.HTML.Tree (ParseTokenForestError, tokensToForest)
import           Text.Read (readMaybe)

import Refine.Common.Types


-- * errors

data VDocHTMLError =
    VDocHTMLErrorBadTree ParseTokenForestError
  | VDocHTMLErrorBadChunkPoint (Forest PreToken) ChunkPoint
  | VDocHTMLErrorNotEnouchCharsToSplit Int [PreToken]
  | VDocHTMLErrorInternal String
  deriving (Eq, Show)

-- | this type is introduced because we keep open and close marks separately in the tree at some
-- point.  in order to keep track of which close mark belongs to which open mark, we cannot rely on
-- their order (which still needs to be de-overlapped), so need close marks of the form
-- @PreMarkClose "data-chunk-id-value"@.
data PreToken = PreToken Token | PreMarkOpen ST | PreMarkClose ST
  deriving (Eq, Show)


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

-- | Call 'tokensToForest' and convert the error type.
tokensToForest' :: MonadError VDocHTMLError m => [Token] -> m (Forest Token)
tokensToForest' ts = case tokensToForest ts of
  Right f  -> pure f
  Left msg -> throwError $ VDocHTMLErrorBadTree msg
