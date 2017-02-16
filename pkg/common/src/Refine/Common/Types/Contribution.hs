{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Refine.Common.Types.Contribution where

import Data.Char (toLower)
import Data.Typeable (Typeable, typeOf)
import Data.String.Conversions (cs, (<>))
import GHC.Generics (Generic)
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))

import Refine.Common.Types.Chunk
import Refine.Common.Types.Comment
import Refine.Common.Types.VDoc
import Refine.Prelude.TH (makeRefineType)


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

class IsContribution a

instance IsContribution Note
instance IsContribution Question
instance IsContribution Discussion
instance IsContribution Edit

-- | This is the @data-contribution-kind@ attribute value of the @mark@ tag, which links to the
-- 'ChunkRange' in the dom, which in turn links to a contribution.
data ContributionKind =
    ContribKindNote
  | ContribKindQuestion
  | ContribKindDiscussion
  | ContribKindEdit
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

makeRefineType ''ContributionKind

instance ToHttpApiData ContributionKind where
  toUrlPiece ContribKindNote       = "note"
  toUrlPiece ContribKindQuestion   = "question"
  toUrlPiece ContribKindDiscussion = "discussion"
  toUrlPiece ContribKindEdit       = "edit"

instance FromHttpApiData ContributionKind where
  parseUrlPiece "note"       = Right ContribKindNote
  parseUrlPiece "question"   = Right ContribKindQuestion
  parseUrlPiece "discussion" = Right ContribKindDiscussion
  parseUrlPiece "edit"       = Right ContribKindEdit
  parseUrlPiece bad          = Left $ "instance FromHttpApiData ContributionKind: no parse for " <> cs (show bad)


contributionKind :: Contribution -> ContributionKind
contributionKind (ContribNote _)       = ContribKindNote
contributionKind (ContribQuestion _)   = ContribKindQuestion
contributionKind (ContribDiscussion _) = ContribKindDiscussion
contributionKind (ContribEdit _)       = ContribKindEdit

-- | This is safe iff @a@ has a constructor in 'Contribution'.
chunkRangeKind :: forall a . (Typeable a, IsContribution a) => ChunkRange a -> ContributionKind
chunkRangeKind _ = (\(Right v) -> v) . parseUrlPiece . cs . fmap toLower . show $ typeOf (undefined :: a)
