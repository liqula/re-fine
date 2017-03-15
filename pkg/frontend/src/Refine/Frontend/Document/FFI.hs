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
  ( -- * https://draftjs.org/docs/api-reference-data-conversion.html
    convertFromRaw
  , convertToRaw
  , convertFromHtml

    -- * https://draftjs.org/docs/api-reference-editor-state.html
  , js_ES_createWithContent
  , js_ES_getCurrentContent
  , js_ES_traceCurrentContent

    -- * https://draftjs.org/docs/api-reference-content-state.html
  , js_CS_createFromText

    -- * contrib
  , js_Draft_stateToHTML
  ) where

import qualified Data.Aeson as Aeson
import           Data.String.Conversions
import           Data.JSString ()  -- instance IsString JSString
import           GHCJS.Types (JSString, JSVal)
import           Text.HTML.Parser

import qualified Refine.Common.VDoc.Draft as Draft


-- * https://draftjs.org/docs/api-reference-data-conversion.html

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromraw
convertFromRaw :: Draft.RawContent -> JSVal
convertFromRaw = js_convertFromRaw . cs . Aeson.encode

foreign import javascript unsafe
    "Draft.convertFromRaw(JSON.parse($1))"
    js_convertFromRaw :: JSString -> JSVal

-- | https://draftjs.org/docs/api-reference-data-conversion.html#converttoraw
convertToRaw :: JSVal -> Draft.RawContent
convertToRaw = either (error . ("convertToRaw: " <>)) id . Aeson.eitherDecode . cs . js_convertToRaw

foreign import javascript unsafe
    "JSON.stringify(Draft.convertToRaw($1))"
    js_convertToRaw :: JSVal -> JSString

-- | https://draftjs.org/docs/api-reference-data-conversion.html#convertfromhtml
convertFromHtml :: [Token] -> JSVal
convertFromHtml = js_convertFromHtml . cs . renderTokens

foreign import javascript unsafe
    "refine$editorContentFromHtml($1)"
    js_convertFromHtml :: JSString -> JSVal


-- * https://draftjs.org/docs/api-reference-editor-state.html

-- | https://draftjs.org/docs/api-reference-editor-state.html#createwithcontent
foreign import javascript unsafe
    "Draft.EditorState.createWithContent($1)"
    js_ES_createWithContent :: JSVal -> IO JSVal

-- | https://draftjs.org/docs/api-reference-editor-state.html#getcurrentcontent
foreign import javascript unsafe
    "$1.getCurrentContent()"
    js_ES_getCurrentContent :: JSVal -> IO JSVal

-- | Convenient wrapper for 'js_ES_getCurrentContent'.
foreign import javascript unsafe
    "console.log('Editor.getCurrentContent() == ', Draft.convertToRaw($1.getCurrentContent()))"
    js_ES_traceCurrentContent :: JSVal -> ()


-- * https://draftjs.org/docs/api-reference-content-state.html

-- | https://draftjs.org/docs/api-reference-content-state.html#createfromtext
foreign import javascript unsafe
    "Draft.ContentState.createFromText($1)"
    js_CS_createFromText :: JSString -> IO JSVal


-- * contrib

-- | https://github.com/sstur/draft-js-export-html
foreign import javascript unsafe
    "DraftStateToHTML($1)"
    js_Draft_stateToHTML :: JSVal -> JSString  -- ('IO'?  let's hope this is pure enough...)
