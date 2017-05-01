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

import           Control.Lens ((^.), (.~), (&), has, view)
import           React.Flux

import           Refine.Frontend.Contribution.Types (ContributionAction(TriggerUpdateRange))
import           Refine.Frontend.Document.FFI (updateEditorState)
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (editor_)
import           Refine.Frontend.Types


document :: View '[DocumentProps]
document = mkView "Document" $ \props ->
  article_ [ "id" $= "vdocValue"  -- FIXME: do we still use this?
           , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
           ] $ do
    editor_
      [ "editorState" &= (props ^. dpDocumentState . documentStateVal)
      , "readOnly" &= readonly props
      , onChange $ \evt ->
          let newDocState :: DocumentState
              newDocState = (props ^. dpDocumentState) & documentStateVal .~ updateEditorState evt
          in dispatch . DocumentAction . DocumentUpdate $ newDocState

      , onMouseUp  $ \_ me -> dispatchTriggerUpdateRange (mousePageY me)
      , onTouchEnd $ \_ te -> dispatchTriggerUpdateRange (touchPageY . head . touches $ te)
        -- '{mouse,touch}PageY': relative to article top; '{mouse,touch}ClientY': relative to window top
      ] mempty
  where
    dispatchTriggerUpdateRange :: Int -> ViewEventHandler
    dispatchTriggerUpdateRange = dispatch . ContributionAction . TriggerUpdateRange . OffsetFromDocumentTop

    readonly :: DocumentProps -> Bool
    readonly = has _DocumentStateView . view dpDocumentState

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ !props = view_ document "document_" props
