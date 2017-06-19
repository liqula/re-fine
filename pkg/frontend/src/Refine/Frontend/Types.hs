{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Frontend.Prelude

import Refine.Common.Types


-- | FIXME: use React.Flux.Outdated.ReactViewKey instead (slightly more sophisticated).
type ReactListKey = JSString


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


-- | 'Range' contains a position range representing a 'SelectionState' from draftjs (without the
-- direction), plus some measurements about the window scroll state and size.
--
-- Some thoughts on the applicability of @Range Position@:
--
-- a selection state is only valid wrt. a specific RawContent, as the block keys are used to
-- identify locations in the dom.  this means that if the content changes during edits, the
-- selection state may get outdated.
--
-- when creating comments, the text remains read-only during creation of a comment, so the selection
-- will be valid on submit.  good, no issue here.
--
-- when creating edits, the selection state applies to the previous version, so we're fine, too.
--
-- CAVEAT: we make some further assumptions here about the block keys being stable: *iff* the editor
-- never changes block keys for lines once they have one *and* we never store rawcontent values that
-- are *not* already decorated with block keys by draft, *then* we're good.
data SelectionStateWithPx = SelectionStateWithPx
    { _sstSelectionState :: Selection Position
    , _sstDocTopOffset   :: OffsetFromDocumentTop
    , _sstTopOffset      :: OffsetFromViewportTop
    , _sstBottomOffset   :: OffsetFromViewportTop
    , _sstScrollOffset   :: ScrollOffsetOfViewport
    }
    deriving (Show, Eq, Generic)

makeRefineType ''SelectionStateWithPx
