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
  , createEditorState

  -- * https://draftjs.org/docs/api-reference-data-conversion.html
  , convertFromRaw
  , convertToRaw

  -- * https://draftjs.org/docs/api-reference-editor-state.html
  , js_traceEditorState

  -- * https://draftjs.org/docs/api-reference-content-state.html
  , js_CS_createFromText
  ) where

import           Control.Lens ((&), (.~))
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


documentStateUpdate :: RefineAction -> Maybe (VDocVersion 'HTMLWithMarks) -> DocumentState -> DocumentState
documentStateUpdate (HeaderAction (StartEdit kind)) (Just vdocvers) _state
  = DocumentStateEdit (createEditorState kind vdocvers)

documentStateUpdate (DocumentAction (UpdateEditorState es)) (Just _) state
  = state & _DocumentStateEdit .~ es

documentStateUpdate _ _ state
  = state


createEditorState :: EditKind -> VDocVersion 'HTMLWithMarks -> EditorState
createEditorState kind vers = unsafePerformIO $ do
  let content = convertFromRaw $ vDocVersionToRawContent vers
  estate <- js_ES_createWithContent content

  js_traceEditorState estate `seq` pure ()

  pure $ EditorState kind (NoJSONRep estate)

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
