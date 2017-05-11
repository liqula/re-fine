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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Document.Document
  ( document
  , document_
  , emptyEditorProps
  , defaultEditorProps
  ) where

import           Control.Lens ((^.), (.~), (&), has)
import           Data.Aeson
import           Data.String.Conversions
import           GHCJS.Types
import           React.Flux

import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (editor_)


document :: View '[DocumentProps]
document = mkView "Document" $ \props ->
  article_ [ "id" $= "vdocValue"  -- FIXME: do we still use this?
           , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
           ] $ do
    let dstate = props ^. dpDocumentState
    editor_
      [ "editorState" &= (dstate ^. documentStateVal)
      , "customStyleMap" &= documentStyleMap
      , setReadOnly (has _DocumentStateView dstate)
      , onChange $ \evt ->
          let dstate' :: DocumentState
              dstate' = dstate & documentStateVal .~ updateEditorState evt
          in dispatch . DocumentAction . DocumentUpdate $ dstate'
      ] mempty

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ !props = view_ document "document_" props


-- | in read-only mode, onchange is not fired even on selection.
-- https://github.com/facebook/draft-js/issues/690#issuecomment-282824570
setReadOnly :: forall handler. Bool -> PropertyOrHandler handler
setReadOnly ro = "handleBeforeInput" &= js_setReadOnly ro

foreign import javascript unsafe
  "function() { return $1 ? 'handled' : 'unhandled'; }"
  js_setReadOnly :: Bool -> JSVal


documentStyleMap :: Value
documentStyleMap = object
  [ "CUSTOM_RANGE_COMMENT"
    .= object [ "background" .= String "rgba(255, 0, 0, 0.3)"
              ]
  , "CUSTOM_RANGE_EDIT"
    .= object [ "background" .= String "rgba(0, 255, 0, 0.3)"
              ]
  ]


emptyEditorProps :: [PropertyOrHandler handler]
emptyEditorProps = ["editorState" &= createEmpty]

defaultEditorProps :: ConvertibleStrings s JSString => s -> [PropertyOrHandler handler]
defaultEditorProps txt = ["editorState" &= (createWithContent . createFromText . cs) txt]
