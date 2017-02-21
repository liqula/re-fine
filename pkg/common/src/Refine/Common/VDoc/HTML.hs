-- | Code for handling `VDocVersions` with HTML content.
module Refine.Common.VDoc.HTML
  ( ChunkRangeError(..)
  , canonicalizeVDocVersion    -- 'HTMLRaw" to 'HTMLCanonical'
  , downgradeRawVDocVersion    -- 'HTMLCanonical' to 'HTMLRaw' (benign, but we may not actually need it)
  , insertMarks                -- 'HTMLCanonical' to 'HTMLWithMarks'
  , insertMoreMarks            -- 'HTMLWithMarks' to 'HTMLWithMarks' (if you want to call it incrementally).
  , chunkRangeErrors           -- chunk range validation
  , highlightRange, removeHighlights
  ) where

import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Canonicalize
import Refine.Common.VDoc.HTML.Splice
