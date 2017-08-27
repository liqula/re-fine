{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Header.Types where
#include "import_frontend.hs"

import GHC.Generics (Generic)
import Refine.Common.Types
import Refine.Frontend.Login.Types


data HeaderAction =
    ToggleCommentToolbarExtension
  | ToggleIndexToolbarExtension
  | StartTextSpecificComment
  | StartEdit
  | CloseToolbarExtension
  | ToggleReadOnly
  | ScrollToPageTop
  | ScrollToBlockKey BlockKey
  -- ScrollToDocumentTop
  | OpenEditToolbarLinkEditor ST
  | ToggleDiscussionFlatView
  deriving (Show, Eq, Generic)

data ToolbarExtensionStatus =
    ToolbarExtensionClosed
  | CommentToolbarExtensionWithoutRange
  | CommentToolbarExtensionWithRange
  | EditToolbarLinkEditor ST
  | IndexToolbarExtension
  deriving (Show, Eq, Generic)

data HeaderState = HeaderState
  { _hsReadOnly               :: Bool
  , _hsToolbarExtensionStatus :: ToolbarExtensionStatus
  , _hsDiscussionFlatView     :: Bool
  } deriving (Show, Eq, Generic)

emptyHeaderState :: HasCallStack => HeaderState
emptyHeaderState = HeaderState False ToolbarExtensionClosed False

newtype AddLinkFormState = AddLinkFormState
  { _addLinkFormState :: ST
  } deriving (Show, Eq, Generic)


newtype TopMenuBarProps = TopMenuBarProps
  { _currentUser :: CurrentUser
  } deriving (Eq, Generic)


type ToolbarProps = VDoc


type IndexToolbarProps = Maybe [IndexItem]

data IndexItem = IndexItem
  { _indexItemBlockKey :: BlockKey
  , _indexItemTitle    :: ST
  , _indexItemDepth    :: Int
  } deriving (Show, Eq, Generic)


data DiffToolbarProps = DiffToolbarProps
  { _diffToolbarPropsEditID :: ID Edit
  , _diffToolbarIndex       :: EditIndex
  , _diffToolbarEditKind    :: EditKind
  , _diffToolbarPropsVotes  :: VoteCount
  , _diffToolbarCollapsed   :: Bool
  , _diffToolbarEditable    :: Bool
  } deriving (Show, Eq, Generic)


data EditIsInitial = EditIsInitial | EditIsNotInitial
  deriving (Eq, Show, Generic)

data EditToolbarProps = EditToolbarProps
  { _editToolbarPropsInitial    :: EditIsInitial
  , _editToolbarPropsLinkEditor :: LinkEditorProps
  }
  deriving (Eq, Show, Generic)

data LinkEditorProps
    = LinkButtonDisabled
    | LinkButtonDeletes
    | LinkButtonAdds ST
  deriving (Eq, Show, Generic)


data EditIndex = EditIndex
  { _editIndexNumOfEdits :: Int
  , _editIndex           :: Int  -- 0, 1, 2 ... editIndexNumOfEdits - 1
  }
  deriving (Show, Eq, Generic)

mkEditIndex :: HasCallStack => Edit -> ID Edit -> EditIndex
mkEditIndex e i = EditIndex (Set.size es) (fromMaybe (error "impossible - mkEditIndex") $ Set.lookupIndex i es)
  where
    es = e ^. editChildren


data DiscussionToolbarProps = DiscussionToolbarProps
  { _discToolbarDiscussionID :: Maybe (ID Discussion)
  , _discToolbarFlatView     :: Bool
  , _discToolbarIsNote       :: Bool
  , _discToolbarPropsVotes   :: VoteCount
  }
  deriving (Show, Eq, Generic)

makeRefineTypes [ ''HeaderAction, ''ToolbarExtensionStatus, ''HeaderState, ''AddLinkFormState
                , ''DiffToolbarProps, ''TopMenuBarProps, ''EditIndex, ''IndexItem
                , ''DiscussionToolbarProps, ''EditToolbarProps, ''EditIsInitial, ''LinkEditorProps
                ]
