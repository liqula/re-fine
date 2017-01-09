module Refine.Backend.DocRepo.Darcs where

import Darcs.Patch()
import Refine.Backend.DocRepo.Core
import Refine.Common.VDoc


create :: DocRepo Repository
create = do
  pure $ Repository "repo" "repo-1"

commit :: Repository -> Version -> DocRepo Commit
commit _r _d = pure $ Commit "ffffff"

