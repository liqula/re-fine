{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | for more info, see also https://github.com/nikgraf/awesome-draft-js
module Refine.Frontend.Document.FFI
  ( -- * types
    EditorState
  , ContentState
  , mkEditorState
  , updateEditorState

    -- * https://draftjs.org/docs/api-reference-data-conversion.html
  , convertFromRaw
  , convertToRaw
  , convertFromHtml

    -- * https://draftjs.org/docs/api-reference-editor-state.html
  , createEmpty
  , createWithContent
  , getCurrentContent
  , traceEditorState
  , traceContentState
  , traceContentInEditorState

    -- * https://draftjs.org/docs/api-reference-content-state.html
  , createFromText

    -- * contrib
  , stateToHTML

    -- * editor state actions
  , documentToggleBold
  , documentToggleItalic

    -- * selections
  , getSelection

    -- * marks
  , getMarkSelectorBound
  ) where

import Refine.Frontend.Prelude

import qualified Data.Aeson as Aeson
import           Data.String.Conversions
import           GHCJS.Types (JSString, JSVal)

import qualified Refine.Common.VDoc.Draft as Draft
import           Refine.Frontend.CS ()
import           Refine.Frontend.Document.FFI.Types
import           Refine.Prelude.Aeson (NoJSONRep(NoJSONRep))


-- * https://draftjs.org/docs/api-reference-data-conversion.html

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromraw
convertFromRaw :: Draft.RawContent -> ContentState
convertFromRaw = js_convertFromRaw . cs . Aeson.encode

foreign import javascript unsafe
    "Draft.convertFromRaw(JSON.parse($1))"
    js_convertFromRaw :: JSString -> ContentState

-- | https://draftjs.org/docs/api-reference-data-conversion.html#converttoraw
convertToRaw :: ContentState -> Draft.RawContent
convertToRaw = either (error . ("convertToRaw: " <>)) id . Aeson.eitherDecode . cs . js_convertToRaw

foreign import javascript unsafe
    "JSON.stringify(Draft.convertToRaw($1))"
    js_convertToRaw :: ContentState -> JSString

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromhtml
convertFromHtml :: JSString -> ContentState
convertFromHtml = js_convertFromHtml

foreign import javascript unsafe
    "refine$editorContentFromHtml($1)"
    js_convertFromHtml :: JSString -> ContentState


-- * https://draftjs.org/docs/api-reference-editor-state.html

-- | https://draftjs.org/docs/api-reference-editor-state.html#createempty
createEmpty :: EditorState
createEmpty = js_ES_createEmpty

foreign import javascript unsafe
    "Draft.EditorState.createEmpty()"
    js_ES_createEmpty :: EditorState

-- | https://draftjs.org/docs/api-reference-editor-state.html#createwithcontent
createWithContent :: ContentState -> EditorState
createWithContent = js_ES_createWithContent

foreign import javascript unsafe
    "Draft.EditorState.createWithContent($1)"
    js_ES_createWithContent :: ContentState -> EditorState

-- | https://draftjs.org/docs/api-reference-editor-state.html#getcurrentcontent
getCurrentContent :: EditorState -> ContentState
getCurrentContent = js_ES_getCurrentContent

foreign import javascript unsafe
    "$1.getCurrentContent()"
    js_ES_getCurrentContent :: EditorState -> ContentState


-- * logging

traceEditorState :: EditorState -> IO ()
traceEditorState s = do () <- js_ES_traceEditorState s; pure ()

foreign import javascript unsafe
    "console.log('traceEditorState', $1)"
    js_ES_traceEditorState :: EditorState -> IO ()

traceContentState :: ContentState -> IO ()
traceContentState s = do () <- js_ES_traceContentState s; pure ()

foreign import javascript unsafe
    "console.log('traceContentState', Draft.convertToRaw($1))"
    js_ES_traceContentState :: ContentState -> IO ()

traceContentInEditorState :: EditorState -> IO ()
traceContentInEditorState s = do () <- js_ES_traceContentInEditorState s; pure ()

foreign import javascript unsafe
    "console.log('traceContentInEditorState', Draft.convertToRaw($1.getCurrentContent()))"
    js_ES_traceContentInEditorState :: EditorState -> IO ()


-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
createFromText :: JSString -> ContentState
createFromText = js_CS_createFromText

foreign import javascript unsafe
    "Draft.ContentState.createFromText($1)"
    js_CS_createFromText :: JSString -> ContentState


-- * contrib

-- | https://github.com/sstur/draft-js-export-html
stateToHTML :: ContentState -> JSString
stateToHTML = js_Draft_stateToHTML

foreign import javascript unsafe
    "DraftStateToHTML($1)"
    js_Draft_stateToHTML :: ContentState -> JSString


-- * editor state actions

-- | toggle bold style on current selection
--
-- FIXME: replace this by @documentToggleStyle :: Draft.Style -> EditorState -> EditorState@, and
-- replace 'DocumentToggleBold', 'DocumentToggleItalic' etc by @DocumentToggleStyle Draft.Style@.
-- (i tried this before, but there was no effect any more.  perhaps i got the encoding wrong?)
documentToggleBold :: EditorState -> EditorState
documentToggleBold st = js_ES_toggleInlineStyle st "BOLD"

-- | toggle italic style on current selection
documentToggleItalic :: EditorState -> EditorState
documentToggleItalic st = js_ES_toggleInlineStyle st "ITALIC"

-- | https://draftjs.org/docs/api-reference-rich-utils.html#content
foreign import javascript unsafe
    "Draft.RichUtils.toggleInlineStyle($1,$2)"
    js_ES_toggleInlineStyle :: EditorState -> JSString -> EditorState


-- * selections

-- | https://draftjs.org/docs/api-reference-editor-state.html#getselection
--
-- Draft never actually nulls this field.  There is always have a selection, but start and end point
-- may be identical.  See 'selectionIsEmpty', 'getRangeAction' for context.
getSelection :: EditorState -> Draft.SelectionState
getSelection (js_ES_getSelection -> sel) =
  Draft.SelectionState
    (js_ES_getSelectionIsBackward sel)
    (Draft.SelectionPoint (Draft.BlockKey . cs $ js_ES_getSelectionStartKey sel) (js_ES_getSelectionStartOffset sel))
    (Draft.SelectionPoint (Draft.BlockKey . cs $ js_ES_getSelectionEndKey sel)   (js_ES_getSelectionEndOffset sel))

foreign import javascript unsafe
    "$1.getSelection()"
    js_ES_getSelection :: EditorState -> JSVal

foreign import javascript unsafe
    "$1.getIsBackward()"
    js_ES_getSelectionIsBackward :: JSVal -> Bool

foreign import javascript unsafe
    "$1.getStartKey()"
    js_ES_getSelectionStartKey :: JSVal -> JSString

foreign import javascript unsafe
    "$1.getStartOffset()"
    js_ES_getSelectionStartOffset :: JSVal -> Int

foreign import javascript unsafe
    "$1.getEndKey()"
    js_ES_getSelectionEndKey :: JSVal -> JSString

foreign import javascript unsafe
    "$1.getEndOffset()"
    js_ES_getSelectionEndOffset :: JSVal -> Int


-- * marks

getMarkSelectorBound :: Draft.MarkSelector -> IO Int
getMarkSelectorBound mark@(Draft.MarkSelector side _ _) = js_getBoundingBox (renderSide side) (renderMarkSelector mark)
  where
    renderSide Draft.MarkSelectorTop = "top"
    renderSide Draft.MarkSelectorBottom = "bottom"
    renderSide Draft.MarkSelectorUnknownSide = error $ "getMarkSelectorBound: mark with bad side: " <> show mark

    renderMarkSelector :: Draft.MarkSelector -> JSString
    renderMarkSelector (Draft.MarkSelector _ (Draft.BlockKey b) i) =
      "article span[data-offset-key=\"" <> cs b <> "-0-" <> cs (show i) <> "\""

foreign import javascript unsafe
    "document.querySelector($2).getBoundingClientRect()[$1]"
     js_getBoundingBox :: JSString -> JSString -> IO Int
