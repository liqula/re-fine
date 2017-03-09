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


module Refine.Frontend.Document.Store
  ( documentStateUpdate

  -- * https://draftjs.org/docs/api-reference-data-conversion.html
  , convertFromRaw
  , convertToRaw

  -- * https://draftjs.org/docs/api-reference-editor-state.html
  , js_traceEditorState

  -- * https://draftjs.org/docs/api-reference-content-state.html
  , js_CS_createFromText
  ) where

import           Control.Lens ((&), (%~))
import qualified Data.Aeson as Aeson
import           Data.String.Conversions
import           GHCJS.Types ( JSString, JSVal )
import           React.Flux ()
import           System.IO.Unsafe (unsafePerformIO)

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft as Draft
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Types
import           Refine.Prelude.Aeson (NoJSONRep(..))


documentStateUpdate :: RefineAction -> DocumentState -> DocumentState
documentStateUpdate action state =
  let newState = state
                  & dsEditMode         %~ editModeUpdate action
                  & dsEditorState      %~ editorStateUpdate action
  in newState

---------------------------------------------------------------------------

editModeUpdate :: RefineAction -> Maybe EditKind -> Maybe EditKind
editModeUpdate action state = case action of
    HeaderAction (StartEdit kind) -> Just kind
    _ -> state

editorStateUpdate :: RefineAction -> Maybe EditorState -> Maybe EditorState
editorStateUpdate action state = case action of
    OpenDocument openedVDoc               -> Just . createEditorState $ _compositeVDocVersion openedVDoc
    DocumentAction (UpdateEditorState es) -> Just es
    _ -> state


createEditorState :: VDocVersion 'HTMLWithMarks -> EditorState
createEditorState vers = unsafePerformIO $ do
  let content = convertFromRaw $ vDocVersionToRawContent vers
  estate <- js_ES_createWithContent content

  js_traceEditorState estate `seq` pure ()

  pure . EditorState . NoJSONRep $ estate

foreign import javascript unsafe
    "refine$traceEditorState($1)"
    js_traceEditorState :: JSVal -> ()


-- * https://draftjs.org/docs/api-reference-data-conversion.html

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromraw
convertFromRaw :: RawContent -> JSVal
convertFromRaw = js_convertFromRaw . cs . Aeson.encode

foreign import javascript unsafe
    "Draft.convertFromRaw(JSON.parse($1))"
    js_convertFromRaw :: JSString -> JSVal

-- | https://draftjs.org/docs/api-reference-data-conversion.html#converttoraw
convertToRaw :: JSVal -> RawContent
convertToRaw = either (error . ("convertToRaw: " <>)) id . Aeson.eitherDecode . cs . js_convertToRaw

foreign import javascript unsafe
    "JSON.stringify(Draft.convertToRaw($1))"
    js_convertToRaw :: JSVal -> JSString


-- * https://draftjs.org/docs/api-reference-editor-state.html

-- | https://draftjs.org/docs/api-reference-editor-state.html#createwithcontent
foreign import javascript unsafe
    "Draft.EditorState.createWithContent($1)"
    js_ES_createWithContent :: JSVal -> IO JSVal


-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
foreign import javascript unsafe
    "Draft.ContentState.createFromText($1)"
    js_CS_createFromText :: JSString -> IO JSVal
