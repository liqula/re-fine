module Refine.Common.Types (module R, Contribution(..)) where

import Refine.Common.Types.Prelude as R
import Refine.Common.Types.Chunk   as R
import Refine.Common.Types.Comment as R
import Refine.Common.Types.User    as R
import Refine.Common.Types.VDoc    as R
import Refine.Common.Types.Vote    as R


-- | Type to define terminology (it's ok if it is not used anywhere else in the code).  This is just a
-- list of everything that is deemed a 'Contribution'.
--
-- Note: first edit to a document is also a contribution.  As the initial version of the document it
-- may be a special thing in the UI metaphor, but it is not in the backend types: here there is just
-- an edit on the empty document.
data Contribution =
    ContribNote Note
  | ContribQuestion Question
  | ContribDiscussion Discussion
  | ContribEdit Edit
