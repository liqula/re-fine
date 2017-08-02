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
  , mkDocumentStyleMap

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
documentRender () props = liftViewToStateHandler . articleWrap $ case dstate of
    DocumentStateView estate rawcontent
        -> mkEditor (Just rawcontent) estate

    DocumentStateDiff _ _estate rawcontent edit collapsed _
        -> mkEditor (Just rawContentDiffView) (createWithContent $ convertFromRaw rawContentDiffView)
      where
        rawContentDiffView :: RawContent
        rawContentDiffView = mcollapse $ diffit (edit ^. editSource)

        mcollapse rc
          | collapsed = hideUnchangedParts rc 0 0  -- FUTUREWORK: make these numbers adjustable by the user
          | otherwise = rc

        diffit (EditSource []) = error "impossible"
        -- FUTUREWORK: make possible to choose a parent
        diffit (EditSource ((otedit, _): _)) = showEditAsRawContentWithMarks otedit rawcontent

    DocumentStateEdit estate _ _ -> mkEditor Nothing estate

    DocumentStateDiscussion _ -> error "TODO"
  where
    dstate = props ^. dpDocumentState

    sendMouseUpIfReadOnly :: EventHandlerTypeWithMods 'EventHandlerCode
    sendMouseUpIfReadOnly =
        simpleHandler $ mconcat [ dispatch $ ContributionAction RequestSetRange | has _DocumentStateView dstate ]

    articleWrap = article_
         [ "id" $= "vdocValue"  -- FIXME: do we still use this?
         , "style" @@= [zindex ZIxArticle, decl "overflow" (Ident "visible")]
         , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
         , onMouseUp  $ \_ _me -> sendMouseUpIfReadOnly
         , onTouchEnd $ \_ _te -> sendMouseUpIfReadOnly
         ]

    mkEditor rawContent editorState = editor_
        [ "editorState" &= editorState
        , "customStyleMap" &= documentStyleMap
        , "readOnly" &= has _DocumentStateView dstate
        , onChange (editorOnChange editorState)
        , onBlur editorOnBlur
        , onFocus editorOnFocus
        ] mempty
      where
        documentStyleMap :: Value
        documentStyleMap = mkDocumentStyleMap active rawContent
          where
            active = props ^. dpContributionState . csHighlightedMarkAndBubble


-- | Handle editor change events.
--
-- If `onBlur` and `onChange` do not update the `EditorState` with a new hasFocus value,
-- we do not need to know in which order the editor event handlers 'editorOnChange', 'editorOnBlur',
-- 'editorOnFocus' are called.  (To decide whether the other two have been *or* will be called, we
-- could look at the focus in the old and new editor state, resp., but we don't need that
-- information.)
--
-- FIXME: We are cheating here: the event we get from draft.js is really not a 'HandlerArg' that
-- can be parsed into an 'Event', but an 'EditorState'.  So if we attempt to access 'Event' fields
-- like 'evtType', we will get ugly error messages.  As long as we stick to the 'evtHandlerArg' part
-- of the structure, things should be fine.
--
-- Note that the fact that we don't have an 'Event' here also means that we can't modify it.
editorOnChange :: EditorState -> Event -> EventHandlerTypeWithMods 'EventHandlerCode
editorOnChange estate (evtHandlerArg -> HandlerArg (mkEditorState -> estate')) =
  simpleHandler $ dispatch updateAction
  where
    updateAction = DocumentAction $ UpdateEditorState estate'

    (_isBlur, _isFocus) = (oldfoc && not newfoc, not oldfoc && newfoc)
      where
        oldfoc = estate  ^. to getSelection . selectionStateHasFocus
        newfoc = estate' ^. to getSelection . selectionStateHasFocus

editorOnBlur :: Event -> FocusEvent -> EventHandlerTypeWithMods 'EventHandlerCode
editorOnBlur _ _ = ([], [])

editorOnFocus :: Event -> FocusEvent -> EventHandlerTypeWithMods 'EventHandlerCode
editorOnFocus _ _ = ([], [])


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
