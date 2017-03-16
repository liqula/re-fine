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
  , editorStateToVDocVersion
  ) where

import           Control.Lens ((&), (.~), (^.))
import           Data.JSString (unpack)
import           Data.String.Conversions
import           GHCJS.Types (JSVal, JSString)
import           System.IO.Unsafe (unsafePerformIO)
import           Text.HTML.Tree (tokensFromForest)

import           Refine.Common.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Types
import           Refine.Prelude.Aeson (NoJSONRep(..), unNoJSONRep)


documentStateUpdate :: RefineAction -> Maybe (VDocVersion 'HTMLWithMarks) -> DocumentState -> DocumentState
documentStateUpdate (HeaderAction (StartEdit kind)) (Just vdocvers) _state
  = DocumentStateEdit (createEditorState kind vdocvers)

documentStateUpdate (DocumentAction (DocumentEditStart es)) (Just _) state
  = state & _DocumentStateEdit .~ es

documentStateUpdate (DocumentAction DocumentEditSave) _ _
  = DocumentStateView

documentStateUpdate _ _ state
  = state


createEditorState :: EditKind -> VDocVersion 'HTMLWithMarks -> EditorState
createEditorState kind (VDocVersion vers) = unsafePerformIO $ do
  let content = convertFromHtml $ tokensFromForest vers
  estate <- js_ES_createWithContent content

  js_ES_traceCurrentContent estate `seq` pure ()

  pure $ EditorState kind (NoJSONRep estate)


editorStateToVDocVersion :: EditorState -> Either String (VDocVersion 'HTMLWithMarks)
editorStateToVDocVersion estate = result
  where
    stateval :: JSVal
    stateval = estate ^. editorStateVal . unNoJSONRep

    curcontent :: JSVal
    curcontent = js_ES_getCurrentContent stateval

    curhtml :: JSString
    curhtml = js_Draft_stateToHTML curcontent

    result :: Either String (VDocVersion 'HTMLWithMarks)
    result = vdocVersionFromSTSafe . cs . unpack $ curhtml
