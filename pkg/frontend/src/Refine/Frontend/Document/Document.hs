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


module Refine.Frontend.Document.Document where

import           Control.Lens ((^.), (.~), (&), to)
import           React.Flux
import           React.Flux.Internal (HandlerArg(..), PropertyOrHandler(..))

import           Refine.Frontend.Document.FFI.Types (unsafeMkEditorState, unEditorState)
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Document.VDoc (vdocToHTML)
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (editor_)


document :: View '[DocumentProps]
document = mkView "Document" $ \props ->
  case props ^. dpDocumentState of
    DocumentStateEdit editorState
      -> article_ ["className" $= "gr-20 gr-14@desktop editor_wrapper"] $ do
            editorWrapper_ $ EditorWrapperProps editorState
    DocumentStateView
      -> article_ [ "id" $= "vdocValue"
                  , "className" $= "gr-20 gr-14@desktop"
                  , onMouseUp  $ \_ me -> dispatch $
                      TriggerUpdateSelection (OffsetFromDocumentTop $ mousePageY me) (props ^. dpToolbarStatus)
                        -- <-- relative to webpage | relative to viewport -> mouseClientY me
                  , onTouchEnd $ \_ te -> dispatch $
                      TriggerUpdateSelection (OffsetFromDocumentTop . touchPageY . head $ touches te)
                                                (props ^. dpToolbarStatus)
                  ] $ do
           -- leftover from p'2016:
           -- div_ ["className" $= "c-vdoc-overlay"] $ do
             -- div_ ["className" $= "c-vdoc-overlay__inner"] $ do
           div_ ["className" $= "c-article-content"] $ do
             vdocToHTML (props ^. dpContributionState) (props ^. dpVDocVersion)

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ !props = view_ document "document_" props


-- TODO: `git grep -Hni editorwrapper`; drop the "wrapper"
-- TODO: indentation in the following lines.

editorWrapper :: View '[EditorWrapperProps]
editorWrapper = mkView "EditorWrapper" $ \(EditorWrapperProps estate) ->
      editor_ [ property "editorState" (estate ^. documentEditStateVal . to unEditorState)
              , CallbackPropertyWithSingleArgument "onChange" $  -- 'onChange' or 'on' do not match the type we need.
                  \(HandlerArg evt) -> dispatch . DocumentAction . DocumentEditUpdate $ estate & documentEditStateVal .~ unsafeMkEditorState evt
              ] mempty

editorWrapper_ :: EditorWrapperProps -> ReactElementM eventHandler ()
editorWrapper_ !props = view_ editorWrapper "editorWrapper_" props
