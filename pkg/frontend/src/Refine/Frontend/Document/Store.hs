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
  , vdocVersionToRawContent
  , vdocVersionFromRawContent
  , vdocVersionToContent
  , vdocVersionFromContent
  ) where

import           Control.Lens ((&), (%~), (^.))
import           Data.Aeson (encode, eitherDecode)
import           Data.String.Conversions (cs, (<>))

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Store.Types


documentStateUpdate :: GlobalAction -> Maybe VDocVersion -> DocumentState -> DocumentState
documentStateUpdate (OpenDocument vdocvers) _ _state
  = DocumentStateView (editorStateFromVDocVersion (vdocvers ^. compositeVDocVersion))

documentStateUpdate (HeaderAction (StartEdit kind)) (Just _) (DocumentStateView estate)
  = DocumentStateEdit estate kind

documentStateUpdate (DocumentAction (DocumentUpdate state')) (Just _) _state
  = state'

documentStateUpdate (DocumentAction DocumentToggleBold) (Just _) state
  = state & documentStateVal %~ documentToggleBold

documentStateUpdate (DocumentAction DocumentToggleItalic) (Just _) state
  = state & documentStateVal %~ documentToggleItalic

documentStateUpdate (DocumentAction DocumentSave) (Just vdocvers) _state
  = DocumentStateView (editorStateFromVDocVersion vdocvers)

documentStateUpdate _ _ state
  = state


-- TODO: 'VDocVersion' will be entirely replaced by 'RawContent', the following functions will go away.

editorStateToVDocVersion :: EditorState -> VDocVersion
editorStateToVDocVersion = vdocVersionFromContent . getCurrentContent

editorStateFromVDocVersion :: VDocVersion -> EditorState
editorStateFromVDocVersion = createWithContent . vdocVersionToContent


vdocVersionToRawContent :: VDocVersion -> RawContent
vdocVersionToRawContent (VDocVersion st) = case eitherDecode $ cs st of
  Right v -> v
  Left msg -> error $ "vdocVersionToRawContent: " <> show (msg, st)

vdocVersionFromRawContent :: RawContent -> VDocVersion
vdocVersionFromRawContent = VDocVersion . cs . encode


vdocVersionToContent :: VDocVersion -> ContentState
vdocVersionToContent = convertFromRaw . vdocVersionToRawContent

vdocVersionFromContent :: ContentState -> VDocVersion
vdocVersionFromContent = vdocVersionFromRawContent . convertToRaw
