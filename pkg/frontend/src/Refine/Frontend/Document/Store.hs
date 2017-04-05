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
import           System.IO.Unsafe (unsafePerformIO)
import           Text.HTML.Tree (tokensFromForest, tokensToForest, ParseTokenForestError)

import           Refine.Common.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types


documentStateUpdate :: GlobalAction -> Maybe (VDocVersion 'HTMLWithMarks) -> DocumentState -> DocumentState
documentStateUpdate (HeaderAction (StartEdit kind)) (Just vdocvers) _state
  = DocumentStateEdit (createEditorState kind vdocvers)

documentStateUpdate (DocumentAction (DocumentEditUpdate es)) (Just _) state
  = state & _DocumentStateEdit .~ es

documentStateUpdate (DocumentAction DocumentEditSave) _ _
  = DocumentStateView

documentStateUpdate _ _ state
  = state


{-# NOINLINE createEditorState #-}
createEditorState :: EditKind -> VDocVersion 'HTMLWithMarks -> DocumentEditState
createEditorState kind (VDocVersion vers) = unsafePerformIO $ do
  let content = convertFromHtml $ tokensFromForest vers
  estate <- createWithContent content
  pure $ DocumentEditState kind estate


-- | FIXME: there is no validation here.
editorStateToVDocVersion :: DocumentEditState -> Either ParseTokenForestError (VDocVersion 'HTMLWithMarks)
editorStateToVDocVersion estate =
  VDocVersion <$> (tokensToForest . stateToHTML . getCurrentContent $ estate ^. documentEditStateVal)
