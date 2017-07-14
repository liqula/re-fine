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
          diffit (EditSource ((otedit, _): _))
            = showEditAsRawContentWithMarks otedit <$> (dstate ^? documentStateContent)

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
{-
It is also possible to install onBlur and onFocus events which receives
proper Event values.

Question: Can we be sure about the ordering of onChange+onBlur / onChange+onFocus events?

Question: How can we reliably cancel onChange if onBlur/onFocus is fired too?
          (Think about the order and state change of these events.)

Question: What are the right EventModification values for onChange/onBlur?

--      , onBlur
--      , onFocus
-}
      ] mempty

-- | Handle editor change events.
--
-- Ignores boring changes.  Boring changes are those that do not show in 'RawContent' or
-- 'SelectionState'.  We may have missed something here; if that's the case you will notice that the
-- draft component will jump back to stale 'EditorState's (the ones that we neglected to register in
-- 'GlobalState' after some other component updated it (e.g. the 'bold' button in the toolbar).
-- Should be easy to fix once it happens.
--
-- Because both 'ContentState' and 'SelectionState' come from an immutable object, they can be
-- compared as 'JSVal's with '(===)'.
--
-- FIXME: We are cheating here: the event we get from draft.js is really not a 'HandlerArg' that
-- can be parsed into an 'Event', but an 'EditorState'.  So if we attempt to access 'Event' fields
-- like 'evtType', we will get ugly error messages.  As long as we stick to the 'evtHandlerArg' part
-- of the structure, things should be fine.
editorOnChange :: DocumentState -> Event -> (ViewEventHandler, [EventModification])
editorOnChange dstate (evtHandlerArg -> HandlerArg (mkEditorState -> estate')) =
  (dispatch updateAction, mods)
  where
    oldfoc = (dstate ^. documentStateVal . to getSelection . selectionStateHasFocus)
    newfoc = (estate' ^. to getSelection . selectionStateHasFocus)

    mods | oldfoc && newfoc = []
         | oldfoc && not newfoc = [] -- if this is [PreventDefault] then there is an uncaught exception but focus out seems to have the right effect; does not work if combined with the hack in the next line
         | not oldfoc && newfoc = [] -- if this is [StopPropagation] then there is an uncaught exception but focus in seems to have the right effect
         | otherwise = error "nah..."

    updateAction =
       DocumentAction . DocumentUpdate
          . globalDocumentState $ dstate & documentStateVal .~ if oldfoc && not newfoc
         then forceSelection estate' (getSelection estate') -- this should highlight the selection even after focus about, but probably this is not what we want
         else estate'


documentComponentDidMountOrUpdate :: HasCallStack => Outdated.LPropsAndState DocumentProps () -> IO ()
documentComponentDidMountOrUpdate _getPropsAndState = do
  dispatchAndExec . ContributionAction $ RequestSetAllVerticalSpanBounds

document_ :: HasCallStack => DocumentProps -> ReactElementM eventHandler ()
document_ props = Outdated.viewWithSKey document "document" props mempty


mkDocumentStyleMap :: HasCallStack => [MarkID] -> Maybe RawContent -> Value
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

    mouseover :: MarkID -> [Decl]
    mouseover cid = [decl "borderBottom" [expr $ Px 2, expr $ Ident "solid", expr Color.VDocRollover] | any (cid `matches`) actives]

    matches :: MarkID -> MarkID -> Bool
    matches (MarkContribution c _) (MarkContribution c' _) = c == c'
    matches m m' = m == m'

    mkMarkSty :: MarkID -> [Decl]
    mkMarkSty MarkCurrentSelection  = bg 255 255 0 0.3
    mkMarkSty (MarkContribution x _) = case x of
      ContribIDNote _       -> bg   0 255 0 0.3
      ContribIDQuestion _   -> bg   0 255 0 0.3
      ContribIDDiscussion _ -> bg   0 255 0 0.3
      ContribIDEdit _       -> bg   0 255 0 0.3

    bg :: Int -> Int -> Int -> Double -> [Decl]
    bg r g b a = ["background" `decl` Color.RGBA r g b a]


emptyEditorProps :: HasCallStack => [PropertyOrHandler handler]
emptyEditorProps = ["editorState" &= createEmpty]

defaultEditorProps :: HasCallStack => ConvertibleStrings s JSString => s -> [PropertyOrHandler handler]
defaultEditorProps txt = ["editorState" &= (createWithContent . createFromText . cs) txt]
