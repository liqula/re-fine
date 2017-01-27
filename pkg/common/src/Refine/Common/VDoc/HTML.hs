-- | Code for handling `VDocVersions` with HTML content.
module Refine.Common.VDoc.HTML
  ( VDocHTMLError(..)
  , canonicalizeVDocVersion    -- 'HTMLRaw" to 'HTMLCanonical'
  , decanonicalizeVDocVersion  -- 'HTMLCanonical' to 'HTMLRaw' (benign, but we may not actually need it)
  , insertMarks                -- 'HTMLCanonical' to 'HTMLWithMarks'
  , insertMoreMarks            -- 'HTMLWithMarks' to 'HTMLWithMarks' (if you want to call it incrementally).
  , addUIInfoToForest          -- for 'ChunkRange' calculation (probably in the frontend)
  ) where

import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Canonicalize
import Refine.Common.VDoc.HTML.Enhance
import Refine.Common.VDoc.HTML.Splice
