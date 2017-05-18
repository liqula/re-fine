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
  ) where

import Refine.Frontend.Prelude

import           Control.Lens (ix)
import qualified Data.Text as ST
import qualified React.Flux.Outdated as Outdated

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Common.VDoc.OT (showEditAsRawContent)
import qualified Refine.Frontend.Colors as Color
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Store
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (editor_)


-- | Note on draft vs. readOnly vs. selection events: in readOnly mode, draft's onChange event is not
-- fired at all, not even for selection updates.  (There is a hack based on handleBeforeInput, but
-- it only inhibits new characters, not backspace, delete, cut&paste, ...  See
-- https://github.com/facebook/draft-js/issues/690#issuecomment-282824570 for details.)  Our
-- approach is to listen to onMouseEnd, onTouchUp, on the surrounding article_ tag, and recovering
-- the draft coordinates (block keys, block offsets) in 'getDraftSelectionStateViaBrowser'.
document :: Outdated.ReactView DocumentProps
document = Outdated.defineLifecycleView "Document" () Outdated.lifecycleConfig
  { Outdated.lRender = \() props -> liftViewToStateHandler $ do
      let dstate = props ^. dpDocumentState

          sendMouseUpIfReadOnly :: [SomeStoreAction]
          sendMouseUpIfReadOnly =
            mconcat [ dispatch $ ContributionAction RequestSetRange | has _DocumentStateView dstate ]

          documentStyleMap :: Value
          documentStyleMap = mkDocumentStyleMap active rawContent
            where
              active     = props ^. dpContributionState . csHighlightedMarkAndBubble
              rawContent = dstate ^? documentStateContent

          diffs :: Maybe RawContent
          diffs = case props ^. dpContributionState . csDisplayedContributionID of
            Just (ContribIDEdit eid) -> case props ^? dpCompositeVDoc . compositeVDocEdits . ix eid of
                Just edit -> case edit ^. editSource of
                    InitialEdit -> error "impossible"
                    MergeOfEdits{} -> error "not implemented"
                    EditOfEdit otedit _ -> showEditAsRawContent otedit <$> (dstate ^? documentStateContent)
                Nothing -> error "impossible"
            Just _  -> Nothing
            Nothing -> Nothing

      article_ [ "id" $= "vdocValue"  -- FIXME: do we still use this?
               , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
               , onMouseUp  $ \_ _me -> sendMouseUpIfReadOnly
               , onTouchEnd $ \_ _te -> sendMouseUpIfReadOnly
               ] $ do
        editor_
          [ "editorState" &= maybe (dstate ^. documentStateVal) (createWithContent . convertFromRaw) diffs
          , "customStyleMap" &= documentStyleMap
          , "readOnly" &= has _DocumentStateView dstate
          , onChange $ \evt ->
              let dstate' :: DocumentState
                  dstate' = dstate & documentStateVal .~ updateEditorState evt
              in dispatchMany [DocumentAction (DocumentUpdate dstate'), ContributionAction RequestSetMarkPositions]
          ] mempty

  , Outdated.lComponentDidMount = Just $ \getPropsAndState _ldom _setState -> do
      props <- Outdated.lGetProps getPropsAndState
      ()    <- Outdated.lGetState getPropsAndState  -- (just to show there's nothing there)
      dispatchAndExec . ContributionAction =<< setMarkPositions (props ^. dpDocumentState)
  }

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ props = Outdated.view document props mempty


mkDocumentStyleMap :: Maybe ContributionID -> Maybe RawContent -> Value
mkDocumentStyleMap _ Nothing = object []
mkDocumentStyleMap mactive (Just rawContent) = object . mconcat $ go <$> marks
  where
    marks :: [Style]
    marks = map snd . mconcat $ view blockStyles <$> (rawContent ^. rawContentBlocks)

    go :: Style -> [Pair]
    go s@(Mark cid) = [markToST s .:= object (mouseover cid <> mksty cid)]
    go _ = []

    mouseover :: ContributionID -> [Pair]
    mouseover cid = ["border-bottom" .:= String ("2px solid " <> cs Color.VDocRollover) | mactive == Just cid]

    mksty :: ContributionID -> [Pair]
    mksty (ContribIDNote i)       = bg   0 255 (shade i) 0.3
    mksty (ContribIDQuestion i)   = bg   0 255 (shade i) 0.3
    mksty (ContribIDDiscussion i) = bg   0 255 (shade i) 0.3
    mksty (ContribIDEdit i)       = bg   0 255 (shade i) 0.3
    mksty ContribIDHighlightMark  = bg 255 255 0         0.3

    bg :: Int -> Int -> Int -> Double -> [Pair]
    bg r g b t = ["background" .:= String s]
      where
        s = "rgba(" <> ST.intercalate ", " ((cs . show <$> [r, g, b]) <> [cs . show $ t]) <> ")"

    shade :: ID a -> Int
    shade _ = 0


-- | FIXME: this should be delivered from where the instance ToJSON Style is defined, and that
-- instance should be defined in terms of this.
markToST :: Style -> ST
markToST s = case toJSON s of
  String txt -> txt
  _ -> error "impossible."


emptyEditorProps :: [PropertyOrHandler handler]
emptyEditorProps = ["editorState" &= createEmpty]

defaultEditorProps :: ConvertibleStrings s JSString => s -> [PropertyOrHandler handler]
defaultEditorProps txt = ["editorState" &= (createWithContent . createFromText . cs) txt]
