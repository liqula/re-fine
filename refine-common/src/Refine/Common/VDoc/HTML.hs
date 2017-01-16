-- | Code for handling `VDocVersions` with HTML content.
module Refine.Common.VDoc.HTML
  ( VDocHTMLError(..)
  , canonicalizeVDocVersion
  , decanonicalizeVDocVersion
  , insertMarks
  ) where

import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Canonicalize
import Refine.Common.VDoc.HTML.Splice
