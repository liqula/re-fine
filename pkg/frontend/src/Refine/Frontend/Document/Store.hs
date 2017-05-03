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
  , editorStateToVDocVersion
  , editorStateFromVDocVersion
  ) where

import           Control.Lens ((&), (%~), (^?!), _Just)

import           Refine.Common.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types


documentStateUpdate :: GlobalAction -> GlobalState -> DocumentState -> DocumentState
documentStateUpdate (OpenDocument cvdoc) _ _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (DocumentAction DocumentSave) ((^?! gsVDoc . _Just) -> cvdoc) _state
  = mkDocumentStateView $ rawContentFromCompositeVDoc cvdoc

documentStateUpdate (HeaderAction (StartEdit kind)) _ (DocumentStateView _ estate)
  = DocumentStateEdit estate kind

documentStateUpdate (DocumentAction (DocumentUpdate state')) _ _state
  = state'

documentStateUpdate (DocumentAction DocumentToggleBold) _ state
  = state & documentStateVal %~ documentToggleBold

documentStateUpdate (DocumentAction DocumentToggleItalic) _ state
  = state & documentStateVal %~ documentToggleItalic

documentStateUpdate _ _ state
  = state


editorStateToVDocVersion :: EditorState -> VDocVersion
editorStateToVDocVersion = rawContentToVDocVersion . convertToRaw . getCurrentContent

editorStateFromVDocVersion :: VDocVersion -> EditorState
editorStateFromVDocVersion = createWithContent . convertFromRaw . rawContentFromVDocVersion
