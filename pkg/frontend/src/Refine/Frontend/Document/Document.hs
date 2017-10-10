{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Document.Document
  ( document
  , document_
  , emptyEditorProps
  , defaultEditorProps
  , mkDocumentStyleMap

    -- * for testing only
  , documentRender
  ) where
#include "import_frontend.hs"

import Language.Css.Syntax hiding (Value)
import React.Flux.Internal (HandlerArg(HandlerArg))

import           Refine.Common.Types
import           Refine.Common.VDoc.OT (showEditAsRawContentWithMarks, hideUnchangedParts)
import           Refine.Frontend.Contribution.Discussion
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Document.Store
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (draftEditor_)
import           Refine.Frontend.Util


-- | Note on draft vs. readOnly vs. selection events: in readOnly mode, draft's onChange event is not
-- fired at all, not even for selection updates.  (There is a hack based on handleBeforeInput, but
-- it only inhibits new characters, not backspace, delete, cut&paste, ...  See
-- https://github.com/facebook/draft-js/issues/690#issuecomment-282824570 for details.)  Our
-- approach is to listen to onMouseEnd, onTouchUp, on the surrounding article_ tag, and recovering
-- the draft coordinates (block keys, block offsets) in 'getDraftSelectionStateViaBrowser'.
document :: HasCallStack => React.ReactView DocumentProps
document = React.defineLifecycleView "Document" () React.lifecycleConfig
  { React.lRender = documentRender
  , React.lComponentDidMount = Just $ \this _ _ -> documentComponentDidMountOrUpdate this
  , React.lComponentDidUpdate = Just $ \this _ _ _ _ -> documentComponentDidMountOrUpdate this
  }

documentRender :: HasCallStack => () -> DocumentProps -> ReactElementM ('StatefulEventHandlerCode st) ()
documentRender () props = liftViewToStateHandler . articleWrap $ case dstate of
    DocumentStateView rawContent
        -> mkEditor (Just rawContent)

    DocumentStateDiff _ rawcontent edit collapsed _
        -> mkEditor (Just rawContentDiffView)
      where
        rawContentDiffView :: RawContent
        rawContentDiffView = mcollapse $ diffit (edit ^. editSource)

        mcollapse rc
          | collapsed = hideUnchangedParts rc 0 0  -- FUTUREWORK: make these numbers adjustable by the user
          | otherwise = rc

        diffit (EditSource []) = error "impossible - diffit"
        diffit (EditSource ((otedit, _): _)) = showEditAsRawContentWithMarks otedit rawcontent
        -- FUTUREWORK: allow for choosing a parent

    DocumentStateEdit _ _ -> mkEditor Nothing

    DocumentStateDiscussion dprops -> discussion_ dprops
  where
    dstate = props ^. dpDocumentState
    readOnly = isReadOnlyDocumentState dstate

    sendMouseUpIfReadOnly :: EventHandlerTypeWithMods 'EventHandlerCode
    sendMouseUpIfReadOnly =
        simpleHandler $ mconcat [ dispatch $ ContributionAction RequestSetRange | readOnly ]

    articleWrap = article_
         [ "id" $= "vdocValue"  -- FIXME: are we still using this?
         , "style" @@= [zindex ZIxArticle, decl "overflow" (Ident "visible")]
         , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
         , onMouseUp  $ \_ _me -> sendMouseUpIfReadOnly
         , onTouchEnd $ \_ _te -> sendMouseUpIfReadOnly
         ]

    mkEditor :: Maybe RawContent -> ReactElementM 'EventHandlerCode ()
    mkEditor mrc = editor_ eprops
      where
        eprops :: EditorProps
        eprops = EditorProps documentStyleMap readOnly

        documentStyleMap :: Value
        documentStyleMap = mkDocumentStyleMap active mrc
          where
            active = props ^. dpContributionState . csHighlightedMarkAndBubble


editor :: HasCallStack => View '[EditorProps]
editor = mkControllerView @'[StoreArg EditorStore] "editor" $
  \(EditorStore estate) props -> draftEditor_
      [ "editorState" &= estate
      , "editorKey" $= "dt2h6"  -- (this makes the styleguide generation deterministic.)
      , onChange (editorOnChange estate)
      , onFocus editorOnFocus
      , onBlur editorOnBlur
      , "customStyleMap" &= (props ^. editorStyleMap)
      , "readOnly" &= (props ^. editorReadOnly)
      ] mempty

editor_ :: HasCallStack => EditorProps -> ReactElementM 'EventHandlerCode ()
editor_ = view_ editor "editor"


-- | Handle editor change events.
--
-- Under the condition that `onBlur` and `onChange` do not update the `EditorState` with a new hasFocus value,
-- we do not need to know in which order the editor event handlers 'editorOnChange', 'editorOnBlur',
-- 'editorOnFocus' are called.  (To decide whether the other two have been *or* will be called, we
-- could look at the focus in the old and new editor state, resp., but we don't need that
-- information.)
--
-- NOTE: We are cheating here: the event we get from draft.js is really not a 'HandlerArg' that
-- can be parsed into an 'Event', but an 'EditorState'.  So if we attempt to access 'Event' fields
-- like 'evtType', we will get ugly error messages.  As long as we stick to the 'evtHandlerArg' part
-- of the structure, things should be fine.
--
-- Note that the fact that we don't have an 'Event' here also means that we can't modify it.
editorOnChange :: EditorState -> Event -> EventHandlerTypeWithMods 'EventHandlerCode
editorOnChange estate (evtHandlerArg -> HandlerArg (mkEditorState -> estate')) =
  simpleHandler . dispatch . UpdateEditorStore $ estate'
  where
    (_isBlur, _isFocus) = (oldfoc && not newfoc, not oldfoc && newfoc)
      where
        oldfoc = estate  ^. to getSelection . selectionStateHasFocus
        newfoc = estate' ^. to getSelection . selectionStateHasFocus

editorOnBlur :: Event -> FocusEvent -> EventHandlerTypeWithMods 'EventHandlerCode
editorOnBlur _ _ = ([], [])

editorOnFocus :: Event -> FocusEvent -> EventHandlerTypeWithMods 'EventHandlerCode
editorOnFocus _ _ = ([], [])


documentComponentDidMountOrUpdate :: HasCallStack => React.LPropsAndState DocumentProps () -> IO ()
documentComponentDidMountOrUpdate _getPropsAndState = do
  dispatchAndExec . ContributionAction $ RequestSetAllVerticalSpanBounds

document_ :: HasCallStack => DocumentProps -> ReactElementM eventHandler ()
document_ props = React.viewWithSKey document "document" props mempty


emptyEditorProps :: HasCallStack => [PropertyOrHandler handler]
emptyEditorProps = ["editorState" &= createEmpty]

defaultEditorProps :: HasCallStack => ConvertibleStrings s JSString => s -> [PropertyOrHandler handler]
defaultEditorProps txt = ["editorState" &= (createWithContent . createFromText . cs) txt]
