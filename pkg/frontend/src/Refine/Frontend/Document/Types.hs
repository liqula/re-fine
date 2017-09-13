{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Document.Types where
#include "import_frontend.hs"

import           Language.Css.Build hiding (s)
import           Language.Css.Syntax hiding (Value)

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util


newtype EditorStore = EditorStore EditorState
  deriving (Eq, Show, Generic)

data EditorStoreAction
  = UpdateEditorStore EditorState
  | DocumentToggleStyle Style
  | DocumentToggleBlockType BlockType
  | DocumentRemoveLink
  | DocumentCreateLink ST
  | DocumentUndo
  | DocumentRedo
  | DocumentRequestSave EditIsInitial
  deriving (Eq, Show, Generic)


data DocumentAction =
    UpdateDocumentStateView
  | DocumentUpdateEditInfo (EditInfo (Maybe EditKind))
  | DocumentSave (FormActionWith (EditIsInitial, EditorState) (EditInfo EditKind, EditorState))
  | DocumentCancelSave
  | ToggleCollapseDiff
  | ReplyStatement Bool{-replace-} (ID Statement) (FormAction CreateStatement)
  deriving (Show, Eq, Generic)

data DocumentState_ editable{-() or Bool-} rawcontent edit discussion =
    DocumentStateView
      { _documentStateContent  :: rawcontent
      }
  | DocumentStateDiff
      { _documentStateDiffIndex     :: EditIndex
      , _documentStateContent       :: rawcontent
      , _documentStateDiff          :: edit
      , _documentStateDiffCollapsed :: Bool
      , _documentStateDiffEditable  :: editable  -- ^ derived in global state, so it is () there
      }
  | DocumentStateEdit
      { _documentStateEditInfo :: EditInfo (Maybe EditKind)  -- ^ 'editInput_' dialog state lives here between close / re-open.
      , _documentBaseEdit      :: Maybe (ID Edit)  -- ^ Just if we are updating and edit
      }
  | DocumentStateDiscussion
      { _documentDiscussion    :: discussion
      }
  deriving (Show, Eq, Generic, Functor)

mapDocumentState :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> DocumentState_ a b c d -> DocumentState_ a' b' c' d'
mapDocumentState fa fb fc fd = \case
  DocumentStateView a -> DocumentStateView (fb a)
  DocumentStateDiff i a e y ed -> DocumentStateDiff i (fb a) (fc e) y (fa ed)
  DocumentStateEdit y be -> DocumentStateEdit y be
  DocumentStateDiscussion d -> DocumentStateDiscussion (fd d)

-- | The document state variant for 'DocumentProps'.  TODO:c rename to 'DocumentStateProps' ('DocumentProps' is already taken).
type DocumentState = DocumentState_ Bool RawContent Edit DiscussionProps

-- | The document state for 'GlobalState'.  TODO:c rename to 'DocumentState'
type GlobalDocumentState = DocumentState_ () () (ID Edit) (ID Discussion, Maybe StatementEditorProps)

data WipedDocumentState =
    WipedDocumentStateView
  | WipedDocumentStateDiff
      { _wipedDocumentStateDiffIndex     :: EditIndex
      , _wipedDocumentStateDiff          :: Edit
      , _wipedDocumentStateDiffCollapsed :: Bool
      , _wipedDocumentStateDiffEditable  :: Bool
      }
  | WipedDocumentStateEdit EditToolbarProps
  | WipedDocumentStateDiscussion DiscussionToolbarProps
  deriving (Show, Eq)

globalDocumentState :: HasCallStack => DocumentState -> GlobalDocumentState
globalDocumentState
  = mapDocumentState
    (const ())
    (const ())
    (^. editID)
    (\d -> (either id ((^. discussionID) . snd) $ d ^. discPropsDiscussion, Nothing))

-- | The boolean 'eidChanged' indicates whether the edit currently in
-- focus has changed and the context (like the edit that we diff
-- against) does not apply any more.  If true, always switch to view
-- mode; otherwise, stay in whichever mode we are.
refreshDocumentStateView :: Edit -> Bool -> GlobalDocumentState -> GlobalDocumentState
refreshDocumentStateView ed eidChanged = if eidChanged then viewMode else sameMode
  where
    viewMode _ = DocumentStateView ()

    sameMode = \case
      DocumentStateView ()                           -> DocumentStateView ()
      DocumentStateDiff _ () edit collapsed editable -> DocumentStateDiff (mkEditIndex ed edit) () edit collapsed editable
      DocumentStateEdit kind Nothing                 -> DocumentStateEdit kind Nothing
      dst@(DocumentStateEdit _ Just{})               -> dst -- FIXME: not sure about this
      DocumentStateDiscussion did                    -> DocumentStateDiscussion did

emptyEditorStore :: HasCallStack => EditorStore
emptyEditorStore = EditorStore createEmpty

emptyDocumentState :: HasCallStack => GlobalDocumentState
emptyDocumentState = DocumentStateView ()

data DocumentProps = DocumentProps
  { _dpDocumentState     :: DocumentState
  , _dpContributionState :: ContributionState
  }
  deriving (Show, Eq, Generic)

emptyDocumentProps :: HasCallStack => DocumentProps
emptyDocumentProps = DocumentProps
  { _dpDocumentState     = DocumentStateView emptyRawContent
  , _dpContributionState = emptyContributionState
  }

data EditorProps = EditorProps
  { _editorStyleMap       :: Value
  , _editorReadOnly       :: Bool
  }
  deriving (Show, Eq, Generic)

mkDocumentStyleMap :: HasCallStack => [MarkID] -> Maybe RawContent -> Value
mkDocumentStyleMap _ Nothing = object []
mkDocumentStyleMap actives (Just rawContent) = object . mconcat $ go <$> marks
  where
    marks :: [Style]
    marks = fmap snd . mconcat $ view blockStyles <$> (rawContent ^. rawContentBlocks . to NEL.toList)

    go :: Style -> [Pair]
    go s@(Mark cid)   = [styleToST s .:= declsToJSON (mouseover cid <> mkMarkSty cid)]
    go s@StyleDeleted = [styleToST s .:= declsToJSON (bg 255 0   0 0.3)]
    go s@StyleAdded   = [styleToST s .:= declsToJSON (bg 0   255 0 0.3)]
    go s@StyleChanged = [styleToST s .:= declsToJSON (bg 255 255 0 0.3)]
    go _ = []

    mouseover :: MarkID -> [Decl]
    mouseover cid = [decl "borderBottom" [expr $ Px 2, expr $ Ident "solid", expr $ Ident "rgba(255, 89, 0, 1)"]
                    | any (cid `matches`) actives]

    matches :: MarkID -> MarkID -> Bool
    matches (MarkContribution c _) (MarkContribution c' _) = c == c'
    matches m m' = m == m'

    mkMarkSty :: MarkID -> [Decl]
    mkMarkSty MarkCurrentSelection  = bg 255 255 0 0.3
    mkMarkSty (MarkContribution x _) = case x of
      ContribIDDiscussion True  _ -> bg   0 255 0 0.3
      ContribIDDiscussion False _ -> bg   0 255 0 0.3
      ContribIDEdit _             -> bg   0 255 0 0.3

    bg :: Int -> Int -> Int -> Double -> [Decl]
    bg r g b a = ["background" `decl` Ident (mconcat ["rgba(", show r, ", ", show g, ", ", show b, ", ", show a, ")"])]


deriveClasses
  [ ([''EditorStore, ''EditorStoreAction, ''DocumentAction, ''DocumentState_, ''EditorProps], allClass)
  , ([''DocumentProps, ''WipedDocumentState], [''Lens'])
  ]

isReadOnlyDocumentState :: DocumentState_ e r i d -> Bool
isReadOnlyDocumentState st = has _DocumentStateView st || has _DocumentStateDiff st
