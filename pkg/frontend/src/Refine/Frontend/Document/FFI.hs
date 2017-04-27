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
  , createWithContent
  , getCurrentContent
  , traceCurrentEditorState
  , traceCurrentContent

    -- * https://draftjs.org/docs/api-reference-content-state.html
  , createFromText

    -- * contrib
  , stateToHTML

    -- * editor state actions
  , documentToggleBold
  , documentToggleItalic
  ) where

import qualified Data.Aeson as Aeson
import           Data.String.Conversions
import           GHCJS.Types (JSString)
import           Text.HTML.Parser

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
convertFromHtml :: [Token] -> ContentState
convertFromHtml = js_convertFromHtml . cs . renderTokens

foreign import javascript unsafe
    "refine$editorContentFromHtml($1)"
    js_convertFromHtml :: JSString -> ContentState


-- * https://draftjs.org/docs/api-reference-editor-state.html

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

traceCurrentEditorState :: EditorState -> IO ()
traceCurrentEditorState s = do () <- js_ES_traceCurrentEditorState s; pure ()

foreign import javascript unsafe
    "console.log('traceCurrentEditorState', $1)"
    js_ES_traceCurrentEditorState :: EditorState -> IO ()

traceCurrentContent :: EditorState -> IO ()
traceCurrentContent s = do () <- js_ES_traceCurrentContent s; pure ()

foreign import javascript unsafe
    "console.log('traceCurrentContent', Draft.convertToRaw($1.getCurrentContent()))"
    js_ES_traceCurrentContent :: EditorState -> IO ()


-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
createFromText :: JSString -> ContentState
createFromText = js_CS_createFromText

foreign import javascript unsafe
    "Draft.ContentState.createFromText($1)"
    js_CS_createFromText :: JSString -> ContentState


-- * contrib

-- | https://github.com/sstur/draft-js-export-html
stateToHTML :: ContentState -> [Token]
stateToHTML = parseTokens . cs . js_Draft_stateToHTML

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
    "RichUtils.toggleInlineStyle($1,$2)"
    js_ES_toggleInlineStyle :: EditorState -> JSString -> EditorState
