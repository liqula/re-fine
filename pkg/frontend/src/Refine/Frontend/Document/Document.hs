{-# LANGUAGE NoImplicitPrelude          #-}
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

    -- * for testing only
  , documentRender
  ) where

import Refine.Frontend.Prelude

import qualified Data.List.NonEmpty as NEL
import qualified React.Flux.Outdated as Outdated
import           Language.Css.Build hiding (s)
import           Language.Css.Syntax hiding (Value)

import           Refine.Common.Types
import           Refine.Common.VDoc.OT (showEditAsRawContent, hideUnchangedParts)
import           Refine.Common.VDoc.Draft (deleteMarksFromRawContent)
import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Store
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (editor_)
import           Refine.Frontend.Util

-- | Note on draft vs. readOnly vs. selection events: in readOnly mode, draft's onChange event is not
-- fired at all, not even for selection updates.  (There is a hack based on handleBeforeInput, but
-- it only inhibits new characters, not backspace, delete, cut&paste, ...  See
-- https://github.com/facebook/draft-js/issues/690#issuecomment-282824570 for details.)  Our
-- approach is to listen to onMouseEnd, onTouchUp, on the surrounding article_ tag, and recovering
-- the draft coordinates (block keys, block offsets) in 'getDraftSelectionStateViaBrowser'.
document :: HasCallStack => Outdated.ReactView DocumentProps
document = Outdated.defineLifecycleView "Document" () Outdated.lifecycleConfig
  { Outdated.lRender = documentRender
  , Outdated.lComponentDidMount = Just documentComponentDidMount
  }

documentRender :: HasCallStack => () -> DocumentProps -> ReactElementM (StatefulViewEventHandler st) ()
documentRender() props = liftViewToStateHandler $ do
  let dstate = props ^. dpDocumentState

      sendMouseUpIfReadOnly :: [SomeStoreAction]
      sendMouseUpIfReadOnly =
        mconcat [ dispatch $ ContributionAction RequestSetRange | has _DocumentStateView dstate ]

      editorState :: EditorState
      editorState = maybe (dstate ^. documentStateVal) (createWithContent . convertFromRaw) rawContentDiffView

      documentStyleMap :: Value
      documentStyleMap = mkDocumentStyleMap active rawContent
        where
          active = props ^. dpContributionState . csHighlightedMarkAndBubble

      rawContent :: Maybe RawContent
      rawContent = rawContentDiffView <|> (dstate ^? documentStateContent)

      rawContentDiffView :: Maybe RawContent
      rawContentDiffView
          = fmap mcollapse
          $ diffit =<< (props ^? dpDocumentState . documentStateDiff . editSource)
        where
          mcollapse rc = if props ^? dpDocumentState . documentStateDiffCollapsed == Just True
            then hideUnchangedParts rc 0 0  -- FUTUREWORK: make these numbers adjustable by the user
            else rc

          -- FIXME: show the relevant diff
          diffit (EditSource []) = Nothing
          diffit (EditSource ((otedit, _): _)) = showEditAsRawContent otedit
                                       . deleteMarksFromRawContent  -- (edit inline styles do not work in combination with marks.)
                                     <$> (dstate ^? documentStateContent)

  article_ [ "id" $= "vdocValue"  -- FIXME: do we still use this?
           , "style" @@= [zindex ZIxArticle, decl "overflow" (Ident "visible")]
           , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
           , onMouseUp  $ \_ _me -> sendMouseUpIfReadOnly
           , onTouchEnd $ \_ _te -> sendMouseUpIfReadOnly
           ] $ do
    editor_
      [ "editorState" &= editorState
      , "customStyleMap" &= documentStyleMap
      , "readOnly" &= has _DocumentStateView dstate
      , onChange $ \evt ->
          let dstate' :: DocumentState
              dstate' = dstate & documentStateVal .~ updateEditorState evt
          in dispatchMany [DocumentAction (DocumentUpdate dstate'), ContributionAction RequestSetAllVerticalSpanBounds]
          -- TODO: #371
          --
          -- when clicking on a button in the edit toolbar and this handler triggers, the button
          -- click is not actioned.  if we leave the handler in and dispatch [], it's all good
          -- (except that this event is missing).  does that mean that actions are sometimes
          -- swallowed?
          --
          -- this can be reproduced may times in a row, by creating new selections.  i think that's
          -- a new thing, didn't happen a while ago.
      ] mempty

documentComponentDidMount :: HasCallStack => Outdated.LPropsAndState DocumentProps () -> _ldom -> _setState -> IO ()
documentComponentDidMount getPropsAndState _ldom _setState = do
  props <- Outdated.lGetProps getPropsAndState
  ()    <- Outdated.lGetState getPropsAndState  -- (just to show there's nothing there)
  dispatchAndExec . ContributionAction =<< setAllVerticalSpanBounds (props ^. dpDocumentState)

document_ :: HasCallStack => DocumentProps -> ReactElementM eventHandler ()
document_ props = Outdated.view document props mempty


mkDocumentStyleMap :: HasCallStack => [ContributionID] -> Maybe RawContent -> Value
mkDocumentStyleMap _ Nothing = object []
mkDocumentStyleMap actives (Just rawContent) = object . mconcat $ go <$> marks
  where
    marks :: [Style]
    marks = fmap snd . mconcat $ view blockStyles <$> (rawContent ^. rawContentBlocks . to NEL.toList)

    go :: Style -> [Pair]
    go s@(Mark cid)   = [styleToST s .:= declsToJSON (mouseover cid <> mkMarkSty cid)]
    go s@StyleDeleted = [styleToST s .:= declsToJSON (bg 255 0   0 0.3)]
    go s@StyleAdded   = [styleToST s .:= declsToJSON (bg 0   255 0 0.3)]
    go s@StyleChanged = [styleToST s .:= declsToJSON (bg 255 255 0 0.3)]
    go _ = []

    mouseover :: ContributionID -> [Decl]
    mouseover cid = [decl "borderBottom" [expr $ Px 2, expr $ Ident "solid", expr Color.VDocRollover] | cid `elem` actives]

    mkMarkSty :: ContributionID -> [Decl]
    mkMarkSty (ContribIDNote _)       = bg   0 255 0 0.3
    mkMarkSty (ContribIDQuestion _)   = bg   0 255 0 0.3
    mkMarkSty (ContribIDDiscussion _) = bg   0 255 0 0.3
    mkMarkSty (ContribIDEdit _)       = bg   0 255 0 0.3
    mkMarkSty ContribIDHighlightMark  = bg 255 255 0 0.3

    bg :: Int -> Int -> Int -> Double -> [Decl]
    bg r g b a = ["background" `decl` Color.RGBA r g b a]


emptyEditorProps :: HasCallStack => [PropertyOrHandler handler]
emptyEditorProps = ["editorState" &= createEmpty]

defaultEditorProps :: HasCallStack => ConvertibleStrings s JSString => s -> [PropertyOrHandler handler]
defaultEditorProps txt = ["editorState" &= (createWithContent . createFromText . cs) txt]
