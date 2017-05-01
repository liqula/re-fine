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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Types where

import Data.Char (toLower)
import Data.JSString (JSString)
import Data.String.Conversions
import GHC.Generics (Generic)

import Refine.Common.Types
import Refine.Frontend.CS ()
import Refine.Prelude.TH (makeRefineType)


class CssClass a where
  showCssClass :: a -> JSString

-- | TODO: this is React.Flux.Outdated.ReactViewKey
type ReactListKey = JSString  -- do not move this to Frontend.Types, importing this here creates a cycle.

data IconSize
  = S
  | M
  | L
  | XL
  | XXL
  deriving (Eq, Show)

instance CssClass IconSize where
  showCssClass = ("iconsize-" <>) . cs . fmap toLower . show

type IconDescription = (JSString, JSString)


-- | `viewport` is the browser window (the visible part of the `document`, or `page`).  See
-- `/docs/frontend/offsets.pdf`.
newtype OffsetFromViewportTop = OffsetFromViewportTop { _unOffsetFromViewportTop :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

-- | Distance between document top and viewport top.  See `/docs/frontend/offsets.pdf`.
newtype ScrollOffsetOfViewport = ScrollOffsetOfViewport { _unScrollOffsetOfViewport :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

-- | Distance between document top and node (e.g., `<mark>` or `</mark>`).
newtype OffsetFromDocumentTop = OffsetFromDocumentTop { _unOffsetFromDocumentTop :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

makeRefineType ''OffsetFromViewportTop
makeRefineType ''ScrollOffsetOfViewport
makeRefineType ''OffsetFromDocumentTop


-- | 'Range' contains the 'SelectionState' from draftjs, plus some measurements about the window
-- scroll state and size.
--
-- Some thoughts on the applicability of 'SelectionState':
--
-- a selection state is only valid wrt. a specific RawContent, as the block keys are used to
-- identify locations in the dom.  this means that if the content changes during edits, teh
-- selection state may get outdated.
--
-- when creating comments, the text remains read-only during creation of a comment, so the selection
-- will be valid on submit.  good, no issue here.
--
-- when creating edits, the selection state applies to the previous version, so we're fine, too.
-- BUT: as long as we convert selection state to chunk range to store it in the (outdated) backend,
-- we need to get the conversion right.
--
-- so we need to store the chunkrange in Range for now instead of the SelectionState, and replace it
-- with SelectionState once ChunkRange and the Chunk module get completely removed from
-- refine-common.
--
-- CAVEAT: we make some assumptions here about the block keys being stable: *iff* the editor never
-- changes block keys for lines once they have one *and* we never store rawcontent values that are
-- *not* already decorated with block keys by draft, *then* we're good.
data Range = Range
    { _rangeSelectionState :: ChunkRange
    , _rangeDocTopOffset   :: OffsetFromDocumentTop
    , _rangeTopOffset      :: OffsetFromViewportTop
    , _rangeBottomOffset   :: OffsetFromViewportTop
    , _rangeScrollOffset   :: ScrollOffsetOfViewport
    }
    deriving (Show, Eq, Generic)

makeRefineType ''Range
