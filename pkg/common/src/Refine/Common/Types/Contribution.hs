{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.Contribution where

import Data.String.Conversions (ST, cs, (<>))
import Data.Int
import GHC.Generics (Generic)
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))
import Text.Read (readEither)
import qualified Data.Text as ST

import Refine.Common.Types.Chunk
import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
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

-- | For places where we need heterogeneous lists of different 'ID's, we can use this type.
--
-- | FUTUREWORK: It would be nice to just use @ID Contribution@ instead of 'ContributionID', but
-- that changes the case switch implementation, and I'm not sure right now if it'll still be as
-- straight-forward.
data ContributionID =
    ContribIDNote (ID Note)
  | ContribIDQuestion (ID Question)
  | ContribIDDiscussion (ID Discussion)
  | ContribIDEdit (ID Edit)
  | ContribIDHighlightMark
  deriving (Eq, Ord, Show, Read, Generic)

-- | In the frontend, for replacing the browser selection range with a mark when an editor overlay
-- opens, we need a 'Void'-like contribution kind that cannot have a contribution value.
data HighlightMark

-- | This is the @data-contribution-kind@ attribute value of the @mark@ tag, which links to the
-- 'ChunkRange' in the dom, which in turn links to a contribution.
data ContributionKind =
    ContribKindNote
  | ContribKindQuestion
  | ContribKindDiscussion
  | ContribKindEdit
  | ContribKindHighlightMark
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

makeRefineType ''ContributionKind


contributionKind :: Contribution -> ContributionKind
contributionKind (ContribNote _)       = ContribKindNote
contributionKind (ContribQuestion _)   = ContribKindQuestion
contributionKind (ContribDiscussion _) = ContribKindDiscussion
contributionKind (ContribEdit _)       = ContribKindEdit


class IsContribution a where
  contribKind :: a -> ContributionKind
  contribID :: ID a -> ContributionID

instance IsContribution Note where
  contribKind _ = ContribKindNote
  contribID = ContribIDNote

instance IsContribution Question where
  contribKind _ = ContribKindQuestion
  contribID = ContribIDQuestion

instance IsContribution Discussion where
  contribKind _ = ContribKindDiscussion
  contribID = ContribIDDiscussion

instance IsContribution Edit where
  contribKind _ = ContribKindEdit
  contribID = ContribIDEdit

instance IsContribution HighlightMark where
  contribKind _ = ContribKindHighlightMark
  contribID _ = ContribIDHighlightMark

chunkRangeKind :: forall a . (IsContribution a) => ChunkRange a -> ContributionKind
chunkRangeKind _ = contribKind (undefined :: a)

contributionToUrlPiece :: IsContribution a => a -> ST
contributionToUrlPiece = toUrlPiece . contribKind

instance ToHttpApiData ContributionKind where
  toUrlPiece ContribKindNote          = "note"
  toUrlPiece ContribKindQuestion      = "question"
  toUrlPiece ContribKindDiscussion    = "discussion"
  toUrlPiece ContribKindEdit          = "edit"
  toUrlPiece ContribKindHighlightMark = "highlight"

instance FromHttpApiData ContributionKind where
  parseUrlPiece "note"       = Right ContribKindNote
  parseUrlPiece "question"   = Right ContribKindQuestion
  parseUrlPiece "discussion" = Right ContribKindDiscussion
  parseUrlPiece "edit"       = Right ContribKindEdit
  parseUrlPiece "highlight"  = Right ContribKindHighlightMark
  parseUrlPiece bad          = Left $ "instance FromHttpApiData ContributionKind: no parse for " <> cs (show bad)


makeRefineType ''ContributionID


instance ToHttpApiData ContributionID where
  toUrlPiece (ContribIDNote (ID i))       = "n" <> cs (show i)
  toUrlPiece (ContribIDQuestion (ID i))   = "q" <> cs (show i)
  toUrlPiece (ContribIDDiscussion (ID i)) = "d" <> cs (show i)
  toUrlPiece (ContribIDEdit (ID i))       = "e" <> cs (show i)
  toUrlPiece ContribIDHighlightMark       = "h"

instance FromHttpApiData ContributionID where
  parseUrlPiece piece = case ST.splitAt 1 piece of
    (ks, is) -> do
      i :: Int64 <- either (Left . cs) Right . readEither @Int64 . cs $ is
      case ks of
        "n" -> Right $ ContribIDNote (ID i)
        "q" -> Right $ ContribIDQuestion (ID i)
        "d" -> Right $ ContribIDDiscussion (ID i)
        "e" -> Right $ ContribIDEdit (ID i)
        "h" -> Right ContribIDHighlightMark
        bad -> Left . cs $ "FromHttpApiData ContributionID: no parse: " <> show bad
