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
  | ContribHighlightMark ChunkRange
  deriving (Eq, Ord, Show, Read, Generic)

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


class IsContribution a where
  contribID :: ID a -> ContributionID

instance IsContribution Note where
  contribID = ContribIDNote

instance IsContribution Question where
  contribID = ContribIDQuestion

instance IsContribution Discussion where
  contribID = ContribIDDiscussion

instance IsContribution Edit where
  contribID = ContribIDEdit

instance IsContribution HighlightMark where
  contribID _ = ContribIDHighlightMark

contributionIDToKindST :: ContributionID -> ST
contributionIDToKindST (ContribIDNote _)       = "note"
contributionIDToKindST (ContribIDQuestion _)   = "question"
contributionIDToKindST (ContribIDDiscussion _) = "discussion"
contributionIDToKindST (ContribIDEdit _)       = "edit"
contributionIDToKindST ContribIDHighlightMark  = "highlight"


makeRefineType ''ContributionID

-- This can probably solved with lenses, but we don't know how...
getNoteID :: ContributionID -> Maybe (ID Note)
getNoteID (ContribIDNote i)       = Just i
getNoteID (ContribIDQuestion _)   = Nothing
getNoteID (ContribIDDiscussion _) = Nothing
getNoteID (ContribIDEdit _)       = Nothing
getNoteID  ContribIDHighlightMark = Nothing

-- This can probably solved with lenses, but we don't know how...
getDiscussionID :: ContributionID -> Maybe (ID Discussion)
getDiscussionID (ContribIDNote _)       = Nothing
getDiscussionID (ContribIDQuestion _)   = Nothing
getDiscussionID (ContribIDDiscussion i) = Just i
getDiscussionID (ContribIDEdit _)       = Nothing
getDiscussionID  ContribIDHighlightMark = Nothing


instance ToHttpApiData ContributionID where
  toUrlPiece (ContribIDNote (ID i))       = "n" <> cs (show i)
  toUrlPiece (ContribIDQuestion (ID i))   = "q" <> cs (show i)
  toUrlPiece (ContribIDDiscussion (ID i)) = "d" <> cs (show i)
  toUrlPiece (ContribIDEdit (ID i))       = "e" <> cs (show i)
  toUrlPiece ContribIDHighlightMark       = "h"

instance FromHttpApiData ContributionID where
  parseUrlPiece piece = case ST.splitAt 1 piece of
    (ks, is) -> do
      let i = either (Left . cs) Right . readEither @Int64 . cs $ is
      case ks of
        "n" -> ContribIDNote . ID <$> i
        "q" -> ContribIDQuestion . ID <$> i
        "d" -> ContribIDDiscussion . ID <$> i
        "e" -> ContribIDEdit . ID <$> i
        "h" -> pure ContribIDHighlightMark
        bad -> Left . cs $ "FromHttpApiData ContributionID: no parse: " <> show bad
