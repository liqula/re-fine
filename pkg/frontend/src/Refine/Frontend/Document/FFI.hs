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


module Refine.Frontend.Document.FFI
  ( -- * types
    EditorState(..)
  , ContentState(..)

    -- * https://draftjs.org/docs/api-reference-data-conversion.html
  , convertFromRaw
  , convertToRaw
  , convertFromHtml

    -- * https://draftjs.org/docs/api-reference-editor-state.html
  , createWithContent
  , getCurrentContent
  , traceCurrentContent

    -- * https://draftjs.org/docs/api-reference-content-state.html
  , createFromText

    -- * contrib
  , stateToHTML
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
createWithContent :: ContentState -> IO EditorState
createWithContent = js_ES_createWithContent

foreign import javascript unsafe
    "Draft.EditorState.createWithContent($1)"
    js_ES_createWithContent :: ContentState -> IO EditorState

-- | https://draftjs.org/docs/api-reference-editor-state.html#getcurrentcontent
getCurrentContent :: EditorState -> ContentState
getCurrentContent = js_ES_getCurrentContent

foreign import javascript unsafe
    "$1.getCurrentContent()"
    js_ES_getCurrentContent :: EditorState -> ContentState

-- | Convenient wrapper for 'getCurrentContent'.
traceCurrentContent :: EditorState -> IO ()
traceCurrentContent s = do () <- js_ES_traceCurrentContent s; pure ()

foreign import javascript unsafe
    "console.log('Editor.getCurrentContent() == ', Draft.convertToRaw($1.getCurrentContent()))"
    js_ES_traceCurrentContent :: EditorState -> IO ()


-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
createFromText :: JSString -> IO ContentState
createFromText = js_CS_createFromText

foreign import javascript unsafe
    "Draft.ContentState.createFromText($1)"
    js_CS_createFromText :: JSString -> IO ContentState


-- * contrib

-- | https://github.com/sstur/draft-js-export-html
stateToHTML :: ContentState -> [Token]
stateToHTML = parseTokens . cs . js_Draft_stateToHTML

foreign import javascript unsafe
    "DraftStateToHTML($1)"
    js_Draft_stateToHTML :: ContentState -> JSString
