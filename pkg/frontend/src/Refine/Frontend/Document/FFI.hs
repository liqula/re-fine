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
{-# LANGUAGE TypeApplications           #-}
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
  , createWithRawContent
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
  , documentToggleStyle
  , documentToggleBlockType
  , documentAddLink
  , documentRemoveLink
  , documentUndo
  , documentRedo

    -- * selections
  , getSelection
  , forceSelection
  , getDraftSelectionStateViaBrowser

    -- * marks
  , getLeafSelectorBound
  ) where

import System.IO.Unsafe (unsafePerformIO)

import qualified Refine.Common.Types.Core as Draft
import           Refine.Frontend.Document.FFI.Types
import           Refine.Frontend.Orphans ()
import           Refine.Frontend.Prelude


-- * https://draftjs.org/docs/api-reference-data-conversion.html

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromraw
--
-- See also: 'convertToRaw'.
convertFromRaw :: HasCallStack => Draft.RawContent -> ContentState
convertFromRaw = js_convertFromRaw . unsafePerformIO . toJSVal

-- | https://draftjs.org/docs/api-reference-data-conversion.html#converttoraw
--
-- The internal call to 'unsafePerformIO' is ok iff 'fromJSVal' is actually pure (and just doesn't
-- care to show it in the type).
convertToRaw :: HasCallStack => ContentState -> Draft.RawContent
convertToRaw = fromMaybe (error "convertToRaw") . unsafePerformIO . fromJSVal . js_convertToRaw

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromhtml
convertFromHtml :: HasCallStack => JSString -> ContentState
convertFromHtml = js_convertFromHtml

-- * https://draftjs.org/docs/api-reference-editor-state.html

-- | https://draftjs.org/docs/api-reference-editor-state.html#createempty
createEmpty :: HasCallStack => EditorState
createEmpty = js_ES_createEmpty

-- | https://draftjs.org/docs/api-reference-editor-state.html#createwithcontent
createWithContent :: HasCallStack => ContentState -> EditorState
createWithContent = js_ES_createWithContent

createWithRawContent :: HasCallStack => Draft.RawContent -> EditorState
createWithRawContent = createWithContent . convertFromRaw

-- | https://draftjs.org/docs/api-reference-editor-state.html#getcurrentcontent
getCurrentContent :: HasCallStack => EditorState -> ContentState
getCurrentContent = js_ES_getCurrentContent

setCurrentContent :: HasCallStack => EditorState -> ContentState -> EditorState
setCurrentContent = js_ES_setCurrentContent

-- * logging

traceEditorState :: HasCallStack => EditorState -> IO ()
traceEditorState s = do () <- js_ES_traceEditorState s; pure ()

traceContentState :: HasCallStack => ContentState -> IO ()
traceContentState s = do () <- js_ES_traceContentState s; pure ()

traceContentInEditorState :: HasCallStack => EditorState -> IO ()
traceContentInEditorState s = do () <- js_ES_traceContentInEditorState s; pure ()

-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
createFromText :: HasCallStack => JSString -> ContentState
createFromText = js_CS_createFromText

-- * contrib

-- | https://github.com/sstur/draft-js-export-html
stateToHTML :: HasCallStack => ContentState -> JSString
stateToHTML = js_Draft_stateToHTML

-- * editor state actions

-- | toggle bold style on current selection
documentToggleStyle :: HasCallStack => Draft.Style -> EditorState -> EditorState
documentToggleStyle sty st = js_ES_toggleInlineStyle st (cs $ Draft.styleToST sty)

-- | toggle italic style on current selection
documentToggleBlockType :: HasCallStack => Draft.BlockType -> EditorState -> EditorState
documentToggleBlockType bt st = js_ES_toggleBlockType st (cs $ Draft.blockTypeToST bt)

-- | Turn the current selection to a link with the given url
--
-- FUTUREWORK: this module should be about concepts known to draft.js, which excludes @LINK@.
-- 'documentAddLink' should be replaced by the more general @documentAddEntity@ that all the
-- arguments to the @createEntity@ method.  (Same with 'documentRemoveLink' and the foreign
-- functions below.)
documentAddLink :: HasCallStack => String -> EditorState -> EditorState
documentAddLink link st = js_ES_toggleLink st' (js_ES_getSelection st') entitykey
  where
    st' = setCurrentContent st content
    entitykey = js_ES_getLastCreatedEntityKey content
    content = js_ES_createLink (getCurrentContent st) (cs link)

-- | Remove links in the current selection
documentRemoveLink :: HasCallStack => EditorState -> EditorState
documentRemoveLink st = js_ES_removeLink st (js_ES_getSelection st)

documentUndo :: HasCallStack => EditorState -> EditorState
documentUndo = js_ES_undo

documentRedo :: HasCallStack => EditorState -> EditorState
documentRedo = js_ES_redo

-- * selections

-- | https://draftjs.org/docs/api-reference-editor-state.html#getselection
--
-- The selection state in js cannot be interpreted as json, instead we need to call some methods to
-- get to the data we need.  Therefore, we do not use 'FromJSVal' or 'FromJSON' to decode it, but
-- this strange cascade of ffi calls.
--
-- Draft never actually nulls this field.  There is always have a selection, but start and end point
-- may be identical.  See 'isEmptyRange', 'getRangeAction' for context.
getSelection :: HasCallStack => EditorState -> Draft.SelectionState
getSelection (js_ES_getSelection -> sel) = Draft.SelectionState .
  (if js_ES_getSelectionIsBackward sel then Draft.toBackwardSelection else Draft.toSelection) $ Draft.Range
    (Draft.Position (Draft.BlockKey . cs $ js_ES_getSelectionStartKey sel) (js_ES_getSelectionStartOffset sel))
    (Draft.Position (Draft.BlockKey . cs $ js_ES_getSelectionEndKey sel)   (js_ES_getSelectionEndOffset sel))

-- | https://draftjs.org/docs/api-reference-editor-state.html#forceselection
forceSelection :: HasCallStack => EditorState -> Draft.SelectionState -> EditorState
forceSelection es (cs . encode -> sel) = js_ES_forceSelection es sel

-- | The shape of the selection object is determined by the generic aeson instances of the haskell
-- type.  If that changes, you need to adjust the test cases in "Refine.Frontend.OrphansSpec" and
-- @refine$getDraftSelectionStateViaBrowser@ in js.
getDraftSelectionStateViaBrowser :: HasCallStack => (MonadIO m, MonadError String m) => m Draft.SelectionState
getDraftSelectionStateViaBrowser = do
  v :: Maybe (Either JSString Draft.SelectionState)
    <- liftIO (js_getDraftSelectionStateViaBrowser >>= fromJSVal)
  case v of
    Just (Right r) -> pure r
    Just (Left e)  -> throwError (cs e)
    Nothing        -> throwError "internal error: no parse!"


-- * marks

getLeafSelectorBound :: HasCallStack => LeafSelectorSide -> Draft.LeafSelector -> IO Int
getLeafSelectorBound side mark =
  js_getBoundingBox (cs $ renderLeafSelectorSide side) (cs $ Draft.renderLeafSelector mark)


-- * foreign

#ifdef __GHCJS__

foreign import javascript safe
  "refine$getDraftSelectionStateViaBrowser()"
  js_getDraftSelectionStateViaBrowser :: IO JSVal

foreign import javascript safe
  "Draft.convertFromRaw($1)"
  js_convertFromRaw :: JSVal -> ContentState

foreign import javascript safe
  "Draft.convertToRaw($1)"
  js_convertToRaw :: ContentState -> JSVal

foreign import javascript safe
  "refine$editorContentFromHtml($1)"
  js_convertFromHtml :: JSString -> ContentState

foreign import javascript safe
  "Draft.EditorState.createEmpty(refine$linkDecorator)"
  js_ES_createEmpty :: EditorState

foreign import javascript safe
  "Draft.EditorState.createWithContent($1, refine$linkDecorator)"
  js_ES_createWithContent :: ContentState -> EditorState

foreign import javascript safe
  "$1.getCurrentContent()"
  js_ES_getCurrentContent :: EditorState -> ContentState

foreign import javascript safe
  "Draft.EditorState.set($1, { currentContent: $2 })"
  js_ES_setCurrentContent :: EditorState -> ContentState -> EditorState

foreign import javascript safe
  "$1.createEntity('LINK', 'MUTABLE', { url: $2 })"
  js_ES_createLink :: ContentState -> JSString -> ContentState

foreign import javascript safe
  "$1.getLastCreatedEntityKey()"
  js_ES_getLastCreatedEntityKey :: ContentState -> JSString

foreign import javascript safe
  "console.log('traceEditorState', $1)"
  js_ES_traceEditorState :: EditorState -> IO ()

foreign import javascript safe
  "console.log('traceContentState', Draft.convertToRaw($1))"
  js_ES_traceContentState :: ContentState -> IO ()

foreign import javascript safe
  "console.log('traceContentInEditorState', Draft.convertToRaw($1.getCurrentContent()))"
  js_ES_traceContentInEditorState :: EditorState -> IO ()

foreign import javascript safe
  "Draft.ContentState.createFromText($1)"
  js_CS_createFromText :: JSString -> ContentState

foreign import javascript safe
  "DraftStateToHTML($1)"
  js_Draft_stateToHTML :: ContentState -> JSString

-- | https://draftjs.org/docs/api-reference-rich-utils.html#content
foreign import javascript safe
  "Draft.RichUtils.toggleInlineStyle($1,$2)"
  js_ES_toggleInlineStyle :: EditorState -> JSString -> EditorState

-- | https://draftjs.org/docs/api-reference-rich-utils.html#content
foreign import javascript safe
  "Draft.RichUtils.toggleLink($1,$2,$3)"
  js_ES_toggleLink :: EditorState -> JSVal{-SelectionState-} -> JSString{-EntityKey-} -> EditorState

foreign import javascript safe
  "Draft.RichUtils.toggleLink($1,$2,null)"
  js_ES_removeLink :: EditorState -> JSVal{-SelectionState-} -> EditorState

foreign import javascript safe
  "Draft.EditorState.undo($1)"
  js_ES_undo :: EditorState -> EditorState

foreign import javascript safe
  "Draft.EditorState.redo($1)"
  js_ES_redo :: EditorState -> EditorState

foreign import javascript safe
  "Draft.RichUtils.toggleBlockType($1,$2)"
  js_ES_toggleBlockType :: EditorState -> JSString -> EditorState

foreign import javascript safe
  "$1.getSelection()"
  js_ES_getSelection :: EditorState -> JSVal

foreign import javascript safe
  "$1.getIsBackward()"
  js_ES_getSelectionIsBackward :: JSVal -> Bool

foreign import javascript safe
  "$1.getStartKey()"
  js_ES_getSelectionStartKey :: JSVal -> JSString

foreign import javascript safe
  "$1.getStartOffset()"
  js_ES_getSelectionStartOffset :: JSVal -> Int

foreign import javascript safe
  "$1.getEndKey()"
  js_ES_getSelectionEndKey :: JSVal -> JSString

foreign import javascript safe
  "$1.getEndOffset()"
  js_ES_getSelectionEndOffset :: JSVal -> Int

foreign import javascript safe
  "refine$setSelectionState($1, JSON.parse($2))"
  js_ES_forceSelection :: EditorState -> JSString -> EditorState

foreign import javascript safe
  "document.querySelector($2).getBoundingClientRect()[$1]"
  js_getBoundingBox :: JSString -> JSString -> IO Int

#else

{-# ANN js_getDraftSelectionStateViaBrowser ("HLint: ignore Use camelCase" :: String) #-}
js_getDraftSelectionStateViaBrowser :: IO JSVal
js_getDraftSelectionStateViaBrowser = error "javascript FFI not available in GHC"

{-# ANN js_convertFromRaw ("HLint: ignore Use camelCase" :: String) #-}
js_convertFromRaw :: JSVal -> ContentState
js_convertFromRaw = error "javascript FFI not available in GHC"

{-# ANN js_convertToRaw ("HLint: ignore Use camelCase" :: String) #-}
js_convertToRaw :: ContentState -> JSVal
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

{-# ANN js_ES_createLink ("HLint: ignore Use camelCase" :: String) #-}
js_ES_createLink :: ContentState -> JSString -> ContentState
js_ES_createLink = error "javascript FFI not available in GHC"

{-# ANN js_ES_getLastCreatedEntityKey ("HLint: ignore Use camelCase" :: String) #-}
js_ES_getLastCreatedEntityKey :: ContentState -> JSString
js_ES_getLastCreatedEntityKey = error "javascript FFI not available in GHC"

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

{-# ANN js_ES_toggleLink ("HLint: ignore Use camelCase" :: String) #-}
js_ES_toggleLink :: EditorState -> JSVal{-SelectionState-} -> JSString{-EntityKey-} -> EditorState
js_ES_toggleLink = error "javascript FFI not available in GHC"

{-# ANN js_ES_removeLink ("HLint: ignore Use camelCase" :: String) #-}
js_ES_removeLink :: EditorState -> JSVal{-SelectionState-} -> EditorState
js_ES_removeLink = error "javascript FFI not available in GHC"

{-# ANN js_ES_undo ("HLint: ignore Use camelCase" :: String) #-}
js_ES_undo :: EditorState -> EditorState
js_ES_undo = error "javascript FFI not available in GHC"

{-# ANN js_ES_redo ("HLint: ignore Use camelCase" :: String) #-}
js_ES_redo :: EditorState -> EditorState
js_ES_redo = error "javascript FFI not available in GHC"

{-# ANN js_ES_toggleBlockType ("HLint: ignore Use camelCase" :: String) #-}
js_ES_toggleBlockType :: EditorState -> JSString -> EditorState
js_ES_toggleBlockType = error "javascript FFI not available in GHC"

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
