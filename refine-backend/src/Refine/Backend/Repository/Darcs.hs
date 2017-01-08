module Refine.Backend.Repository.Darcs where

import Darcs.Patch()
import Refine.Backend.Repository.Core
import Refine.Common.VDoc


create :: Repo Repository
create = do
  pure $ Repository "repo" "repo-1"

commit :: Repository -> Document -> Repo Commit
commit _r _d = pure $ Commit "ffffff"

