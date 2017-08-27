{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Backend.Database.Types where
#include "import_backend.hs"

import           Refine.Common.Types
import qualified Refine.Common.OT as OT


type family CreateDB a = b | b -> a

-- | This type is used in 'MetaInfo' table.  We use the primary key from the tables for 'Note',
-- 'Question' etc., but those are not unique (each table has its own ID namespace).  'MetaInfoID'
-- makes the primary key unique for the 'MetaInfo' table.
--
-- (We could also just use @show . Data.Typeable.typeOf@ to the same end, but this way gives us a
-- comprehensive list of everything that can occur in the table.  If it gets to much work to
-- maintain this, we can refactor it to be more dynamic later.)
data MetaInfoID
  = MetaDiscussion (ID Discussion)
  | MetaStatement  (ID Statement)
  | MetaGroup      (ID Group)
  | MetaUser       (ID User)
  | MetaVDoc       (ID VDoc)
  | MetaEdit       (ID Edit)
  deriving (Eq, Show, Generic)

class HasMetaInfo a where
  metaInfoType :: ID a -> MetaInfoID

instance HasMetaInfo Discussion  where metaInfoType = MetaDiscussion
instance HasMetaInfo Statement   where metaInfoType = MetaStatement
instance HasMetaInfo Group       where metaInfoType = MetaGroup
instance HasMetaInfo User        where metaInfoType = MetaUser
instance HasMetaInfo VDoc        where metaInfoType = MetaVDoc
instance HasMetaInfo Edit        where metaInfoType = MetaEdit

newtype RawContentEdit = RawContentEdit {unRawContentEdit :: OT.Edit RawContent}
  deriving (ToJSON, FromJSON, Monoid)

newtype RangePositions = RangePositions {unRangePositions :: NonEmpty (Range Position)}
  deriving (ToJSON, FromJSON)

newtype RangePosition = RangePosition {unRangePosition :: Range Position}
  deriving (ToJSON, FromJSON)

newtype DBVotes = DBVotes {unDBVotes :: Votes}
  deriving (ToJSON, FromJSON)

makeRefineType ''MetaInfoID
