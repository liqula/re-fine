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
  ) where

import           Control.Lens ((&), (.~))
import           React.Flux ()  -- instance IsString JSString
import           System.IO.Unsafe (unsafePerformIO)
import           Text.HTML.Tree (tokensFromForest)

import           Refine.Common.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Types
import           Refine.Prelude.Aeson (NoJSONRep(..))


documentStateUpdate :: RefineAction -> Maybe (VDocVersion 'HTMLWithMarks) -> DocumentState -> DocumentState
documentStateUpdate (HeaderAction (StartEdit kind)) (Just vdocvers) _state
  = DocumentStateEdit (createEditorState kind vdocvers)

documentStateUpdate (DocumentAction (DocumentEditStart es)) (Just _) state
  = state & _DocumentStateEdit .~ es

documentStateUpdate _ _ state
  = state


createEditorState :: EditKind -> VDocVersion 'HTMLWithMarks -> EditorState
createEditorState kind (VDocVersion vers) = unsafePerformIO $ do
  let content = convertFromHtml $ tokensFromForest vers
  estate <- js_ES_createWithContent content

  js_ES_traceCurrentContent estate `seq` pure ()

  pure $ EditorState kind (NoJSONRep estate)
