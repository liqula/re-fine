{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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
  , setCurrentContent
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
  , forceSelection

    -- * marks
  , getMarkSelectorBound
  ) where

import Refine.Frontend.Prelude

import qualified Refine.Common.Types.Core as Draft
import           Refine.Frontend.Document.FFI.Types
import           Refine.Prelude.Aeson (NoJSONRep(NoJSONRep))


-- * https://draftjs.org/docs/api-reference-data-conversion.html

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromraw
convertFromRaw :: Draft.RawContent -> ContentState
convertFromRaw = js_convertFromRaw . cs . encode

-- | https://draftjs.org/docs/api-reference-data-conversion.html#converttoraw
convertToRaw :: ContentState -> Draft.RawContent
convertToRaw = either (error . ("convertToRaw: " <>)) id . eitherDecode . cs . js_convertToRaw

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromhtml
convertFromHtml :: JSString -> ContentState
convertFromHtml = js_convertFromHtml

-- * https://draftjs.org/docs/api-reference-editor-state.html

-- | https://draftjs.org/docs/api-reference-editor-state.html#createempty
createEmpty :: EditorState
createEmpty = js_ES_createEmpty

-- | https://draftjs.org/docs/api-reference-editor-state.html#createwithcontent
createWithContent :: ContentState -> EditorState
createWithContent = js_ES_createWithContent

-- | https://draftjs.org/docs/api-reference-editor-state.html#getcurrentcontent
getCurrentContent :: EditorState -> ContentState
getCurrentContent = js_ES_getCurrentContent

setCurrentContent :: EditorState -> ContentState -> EditorState
setCurrentContent = js_ES_setCurrentContent

-- * logging

traceEditorState :: EditorState -> IO ()
traceEditorState s = do () <- js_ES_traceEditorState s; pure ()

traceContentState :: ContentState -> IO ()
traceContentState s = do () <- js_ES_traceContentState s; pure ()

traceContentInEditorState :: EditorState -> IO ()
traceContentInEditorState s = do () <- js_ES_traceContentInEditorState s; pure ()

-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
createFromText :: JSString -> ContentState
createFromText = js_CS_createFromText

-- * contrib

-- | https://github.com/sstur/draft-js-export-html
stateToHTML :: ContentState -> JSString
stateToHTML = js_Draft_stateToHTML

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

-- | https://draftjs.org/docs/api-reference-editor-state.html#forceselection
forceSelection :: EditorState -> Draft.SelectionState -> EditorState
forceSelection es (cs . encode -> sel) = js_ES_forceSelection es sel

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

#ifdef __GHCJS__

foreign import javascript unsafe
    "Draft.convertFromRaw(JSON.parse($1))"
    js_convertFromRaw :: JSString -> ContentState

foreign import javascript unsafe
    "JSON.stringify(Draft.convertToRaw($1))"
    js_convertToRaw :: ContentState -> JSString

foreign import javascript unsafe
    "refine$editorContentFromHtml($1)"
    js_convertFromHtml :: JSString -> ContentState

foreign import javascript unsafe
    "Draft.EditorState.createEmpty()"
    js_ES_createEmpty :: EditorState

foreign import javascript unsafe
    "Draft.EditorState.createWithContent($1)"
    js_ES_createWithContent :: ContentState -> EditorState

foreign import javascript unsafe
    "$1.getCurrentContent()"
    js_ES_getCurrentContent :: EditorState -> ContentState

foreign import javascript unsafe
    "Draft.EditorState.set($1, { currentContent: $2 })"
    js_ES_setCurrentContent :: EditorState -> ContentState -> EditorState

foreign import javascript unsafe
    "console.log('traceEditorState', $1)"
    js_ES_traceEditorState :: EditorState -> IO ()

foreign import javascript unsafe
    "console.log('traceContentState', Draft.convertToRaw($1))"
    js_ES_traceContentState :: ContentState -> IO ()

foreign import javascript unsafe
    "console.log('traceContentInEditorState', Draft.convertToRaw($1.getCurrentContent()))"
    js_ES_traceContentInEditorState :: EditorState -> IO ()

foreign import javascript unsafe
    "Draft.ContentState.createFromText($1)"
    js_CS_createFromText :: JSString -> ContentState

foreign import javascript unsafe
    "DraftStateToHTML($1)"
    js_Draft_stateToHTML :: ContentState -> JSString

-- | https://draftjs.org/docs/api-reference-rich-utils.html#content
foreign import javascript unsafe
    "Draft.RichUtils.toggleInlineStyle($1,$2)"
    js_ES_toggleInlineStyle :: EditorState -> JSString -> EditorState

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

foreign import javascript unsafe
    "refine$setSelectionState($1, JSON.parse($2))"
    js_ES_forceSelection :: EditorState -> JSString -> EditorState

foreign import javascript unsafe
    "document.querySelector($2).getBoundingClientRect()[$1]"
     js_getBoundingBox :: JSString -> JSString -> IO Int

#else

{-# ANN js_convertFromRaw ("HLint: ignore Use camelCase" :: String) #-}
js_convertFromRaw :: JSString -> ContentState
js_convertFromRaw = error "javascript FFI not available in GHC"

{-# ANN js_convertToRaw ("HLint: ignore Use camelCase" :: String) #-}
js_convertToRaw :: ContentState -> JSString
js_convertToRaw = error "javascript FFI not available in GHC"

{-# ANN js_convertFromHtml ("HLint: ignore Use camelCase" :: String) #-}
js_convertFromHtml :: JSString -> ContentState
js_convertFromHtml = error "javascript FFI not available in GHC"

{-# ANN js_ES_createEmpty ("HLint: ignore Use camelCase" :: String) #-}
js_ES_createEmpty :: EditorState
js_ES_createEmpty = error "javascript FFI not available in GHC"

{-# ANN js_ES_createWithContent ("HLint: ignore Use camelCase" :: String) #-}
js_ES_createWithContent :: ContentState -> EditorState
js_ES_createWithContent = error "javascript FFI not available in GHC"

{-# ANN js_ES_getCurrentContent ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getCurrentContent :: EditorState -> ContentState
js_ES_getCurrentContent = error "javascript FFI not available in GHC"

{-# ANN js_ES_setCurrentContent ("HLint: ignore Use camelCase" :: String) #-}
js_ES_setCurrentContent :: EditorState -> ContentState -> EditorState
js_ES_setCurrentContent = error "javascript FFI not available in GHC"

{-# ANN js_ES_traceEditorState ("HLint: ignore Use camelCase" :: String) #-}
js_ES_traceEditorState :: EditorState -> IO ()
js_ES_traceEditorState = error "javascript FFI not available in GHC"

{-# ANN js_ES_traceContentState ("HLint: ignore Use camelCase" :: String) #-}
js_ES_traceContentState :: ContentState -> IO ()
js_ES_traceContentState = error "javascript FFI not available in GHC"

{-# ANN js_ES_traceContentInEditorState ("HLint: ignore Use camelCase" :: String) #-}
js_ES_traceContentInEditorState :: EditorState -> IO ()
js_ES_traceContentInEditorState = error "javascript FFI not available in GHC"

{-# ANN js_CS_createFromText ("HLint: ignore Use camelCase" :: String) #-}
js_CS_createFromText :: JSString -> ContentState
js_CS_createFromText = error "javascript FFI not available in GHC"

{-# ANN js_Draft_stateToHTML ("HLint: ignore Use camelCase" :: String) #-}
js_Draft_stateToHTML :: ContentState -> JSString
js_Draft_stateToHTML = error "javascript FFI not available in GHC"

{-# ANN js_ES_toggleInlineStyle ("HLint: ignore Use camelCase" :: String) #-}
js_ES_toggleInlineStyle :: EditorState -> JSString -> EditorState
js_ES_toggleInlineStyle = error "javascript FFI not available in GHC"

{-# ANN js_ES_getSelection ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getSelection :: EditorState -> JSVal
js_ES_getSelection = error "javascript FFI not available in GHC"

{-# ANN js_ES_getSelectionIsBackward ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getSelectionIsBackward :: JSVal -> Bool
js_ES_getSelectionIsBackward = error "javascript FFI not available in GHC"

{-# ANN js_ES_getSelectionStartKey ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getSelectionStartKey :: JSVal -> JSString
js_ES_getSelectionStartKey = error "javascript FFI not available in GHC"

{-# ANN js_ES_getSelectionStartOffset ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getSelectionStartOffset :: JSVal -> Int
js_ES_getSelectionStartOffset = error "javascript FFI not available in GHC"

{-# ANN js_ES_getSelectionEndKey ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getSelectionEndKey :: JSVal -> JSString
js_ES_getSelectionEndKey = error "javascript FFI not available in GHC"

{-# ANN js_ES_getSelectionEndOffset ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getSelectionEndOffset :: JSVal -> Int
js_ES_getSelectionEndOffset = error "javascript FFI not available in GHC"

{-# ANN js_ES_forceSelection ("HLint: ignore Use camelCase" :: String) #-}
js_ES_forceSelection :: EditorState -> JSString -> EditorState
js_ES_forceSelection = error "javascript FFI not available in GHC"

{-# ANN js_getBoundingBox ("HLint: ignore Use camelCase" :: String) #-}
js_getBoundingBox :: JSString -> JSString -> IO Int
js_getBoundingBox = error "javascript FFI not available in GHC"

#endif
