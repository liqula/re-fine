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
import           React.Flux.Internal (HandlerArg(HandlerArg))

import           Refine.Common.Types
import           Refine.Common.VDoc.OT (showEditAsRawContentWithMarks, hideUnchangedParts)
import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
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
  , Outdated.lComponentDidMount = Just $ \this _ _ -> documentComponentDidMountOrUpdate this
  , Outdated.lComponentDidUpdate = Just $ \this _ _ _ _ -> documentComponentDidMountOrUpdate this
  }

documentRender :: HasCallStack => () -> DocumentProps -> ReactElementM ('StatefulEventHandlerCode st) ()
documentRender() props = liftViewToStateHandler $ do
  let dstate = props ^. dpDocumentState

      sendMouseUpIfReadOnly :: HandlerWithEventModifications 'EventHandlerCode
      sendMouseUpIfReadOnly =
        simpleHandler $ mconcat [ dispatch $ ContributionAction RequestSetRange | has _DocumentStateView dstate ]

      editorState :: EditorState
      editorState = undefined  -- ok, here we really need to store *something* in GlobalState, but
                               -- could we store the component rather than its state?

      documentStyleMap :: Value
      documentStyleMap = mkDocumentStyleMap active rawContent
        where
          active = props ^. dpContributionState . csHighlightedMarkAndBubble

      rawContent :: Maybe RawContent
      rawContent = undefined  -- derive from editorState above?  rawContentDiffView <|> (dstate ^? documentStateContent)

      _rawContentDiffView :: Maybe RawContent
      _rawContentDiffView
          = fmap mcollapse
          $ diffit =<< (props ^? dpDocumentState . documentStateDiff . editSource)
        where
          mcollapse rc = if props ^? dpDocumentState . documentStateDiffCollapsed == Just True
            then hideUnchangedParts rc 0 0  -- FUTUREWORK: make these numbers adjustable by the user
            else rc

          -- FIXME: show the relevant diff
          diffit (EditSource []) = Nothing
          diffit (EditSource ((otedit, _): _))
            = showEditAsRawContentWithMarks otedit <$> undefined -- (dstate ^? documentStateContent)

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
      , onChange $ editorOnChange dstate
--      , onFocus $ ...  -- (block event, also print it.  is it triggered next to onChange, or solo?  does it help if we block this?)
--      , onBlur $ ...
      ] mempty


-- | FIXME: We are cheating here: the event we get from draft.js is really not a 'HandlerArg' that
-- can be parsed into an 'Event', but an 'EditorState'.  So if we attempt to access 'Event' fields
-- like 'evtType', we will get ugly error messages.  As long as we stick to the 'evtHandlerArg' part
-- of the structure, things should be fine.
--
-- TODO: what do we use the EditorState in DocumentState for?  can we just toss it, and get a fresh
-- one when we need one?  from where?  i suspect that may be both faster and more robust.
--
-- hum.  i tried to use @boring = ((==) `on` convertToRaw . getCurrentContent) (dstate
-- ^. documentStateVal) estate'@ to decide whether to dispatch actions or not, but that caused the
-- selection state in the editorstate to snap back to outdated selection states.  so that's not a
-- good solution.
--
-- TODO: #371
--
-- when clicking on a button in the edit toolbar and this handler triggers, the button
-- click is not actioned.  if we leave the handler in and dispatch [], it's all good
-- (except that this event is missing).  does that mean that actions are sometimes
-- swallowed?
--
-- this can be reproduced may times in a row, by creating new selections.  i think that's
-- a new thing, didn't happen a while ago.
editorOnChange :: DocumentState -> Event -> (ViewEventHandler, [EventModification])
editorOnChange _dstate (evtHandlerArg -> HandlerArg (mkEditorState -> _estate')) = simpleHandler $ dispatchMany updateActions
  where
    updateActions =
      [ ContributionAction RequestSetAllVerticalSpanBounds
      ]


documentComponentDidMountOrUpdate :: HasCallStack => Outdated.LPropsAndState DocumentProps () -> IO ()
documentComponentDidMountOrUpdate _getPropsAndState = do
  dispatchAndExec . ContributionAction $ RequestSetAllVerticalSpanBounds

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
