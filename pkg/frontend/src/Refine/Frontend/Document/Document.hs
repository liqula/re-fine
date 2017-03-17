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

import           Control.Lens ((^.))
import           React.Flux
import           React.Flux.Internal (HandlerArg(..), PropertyOrHandler(..))

import           Refine.Frontend.Document.Types
import           Refine.Frontend.Document.FFI (js_ES_traceCurrentContent)
import           Refine.Frontend.Document.VDoc (vdocToHTML)
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.ThirdPartyViews (editor_)
import qualified Refine.Frontend.Types as RS
import           Refine.Prelude.Aeson (NoJSONRep(..))


document :: ReactView DocumentProps
document = defineView "Document" $ \props ->
  case props ^. dpDocumentState of
    DocumentStateEdit editorState
      -> article_ ["className" $= "gr-20 gr-14@desktop editor_wrapper"] $ do
            editorWrapper_ $ EditorWrapperProps editorState
    DocumentStateView
      -> article_ [ "id" $= "vdocValue"
                  , "className" $= "gr-20 gr-14@desktop"
                  , onMouseUp  $ \_ me -> RS.dispatch $
                      RS.TriggerUpdateSelection (SC.OffsetFromDocumentTop $ mousePageY me) (props ^. dpToolbarStatus)
                        -- <-- relative to webpage | relative to viewport -> mouseClientY me
                  , onTouchEnd $ \_ te -> RS.dispatch $
                      RS.TriggerUpdateSelection (SC.OffsetFromDocumentTop . touchPageY . head $ touches te)
                                                (props ^. dpToolbarStatus)
                  ] $ do
           -- leftover from p'2016:
           -- div_ ["className" $= "c-vdoc-overlay"] $ do
             -- div_ ["className" $= "c-vdoc-overlay__inner"] $ do
           div_ ["className" $= "c-article-content"] $ do
             vdocToHTML (props ^. dpContributionState) (props ^. dpVDocVersion)

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ props = view document props mempty


newtype EditorWrapperProps = EditorWrapperProps
  { _ewpEditorState       :: EditorState
  }

editorWrapper :: ReactView EditorWrapperProps
editorWrapper = defineView "EditorWrapper" $ \(EditorWrapperProps (EditorState kind (NoJSONRep editorState))) ->
      editor_ [ property "editorState" editorState
              , CallbackPropertyWithSingleArgument "onChange" $  -- 'onChange' or 'on' do not match the type we need.
                  \(HandlerArg evt) -> js_ES_traceCurrentContent `seq`
                                       (RS.dispatch . RS.DocumentAction . DocumentEditStart . EditorState kind . NoJSONRep $ evt)
              ] mempty

editorWrapper_ :: EditorWrapperProps -> ReactElementM eventHandler ()
editorWrapper_ props = view editorWrapper props mempty
