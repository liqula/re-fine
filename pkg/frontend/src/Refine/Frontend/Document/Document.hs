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

import qualified Data.List.NonEmpty as NEL
import qualified React.Flux.Outdated as Outdated
import           Text.Show.Pretty (ppShow)
import           Language.Css.Build hiding (s)
import           Language.Css.Syntax hiding (Value)

import           Refine.Common.Types
import           Refine.Common.VDoc.OT (showEditAsRawContent)
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
document :: Outdated.ReactView DocumentProps
document = Outdated.defineLifecycleView "Document" () Outdated.lifecycleConfig
  { Outdated.lRender = \() props -> liftViewToStateHandler $ do
      let dstate = props ^. dpDocumentState

          editorState :: EditorState
          editorState = maybe (dstate ^. documentStateVal) (createWithContent . convertFromRaw) rawContentDiffView

          documentStyleMap :: Value
          documentStyleMap = mkDocumentStyleMap active rawContent
            where
              active = props ^. dpContributionState . csHighlightedMarkAndBubble

          rawContent :: Maybe RawContent
          rawContent = rawContentDiffView <|> (dstate ^? documentStateContent)

          rawContentDiffView :: Maybe RawContent
          rawContentDiffView = diffit =<< (props ^? dpDocumentState . documentStateDiff . editSource)
            where
              diffit InitialEdit           = error "impossible"
              diffit MergeOfEdits{}        = error "not implemented"
              diffit (EditOfEdit otedit _) = showEditAsRawContent otedit
                                           . deleteMarksFromRawContent  -- (edit inline styles do not work in combination with marks.)
                                         <$> (dstate ^? documentStateContent)

      article_ [ "id" $= "vdocValue"  -- FIXME: do we still use this?
               , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
               ] $ do
        editor_
          [ "editorState" &= editorState
          , "customStyleMap" &= documentStyleMap
          , "readOnly" &= has _DocumentStateView dstate
          , onChange $ \evt ->
              let dstate' :: DocumentState
                  dstate' = dstate & documentStateVal .~ updateEditorState evt
              in dispatchMany [DocumentAction (DocumentUpdate dstate'), ContributionAction RequestSetMarkPositions]
          ] mempty

        -- when showing an edit, show meta info dump for debugging.  FIXME: this information should
        -- be accessible elsewhere in the app.
        case rawContentDiffView of
          Nothing -> pure ()
          Just _ -> pre_ [ "style" @= object [ "background"   .:= String "rgb(255, 100, 150)"
                                             , "border"       .:= String "6px dashed black"
                                             , "padding"      .:= String "20px"
                                             ]
                         ] $ do
                      elemString . ppShow $ props ^? dpDocumentState . documentStateDiff

  , Outdated.lComponentDidMount = Just $ \getPropsAndState _ldom _setState -> do
      props <- Outdated.lGetProps getPropsAndState
      ()    <- Outdated.lGetState getPropsAndState  -- (just to show there's nothing there)
      dispatchAndExec . ContributionAction =<< setMarkPositions (props ^. dpDocumentState)
  }

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ props = Outdated.view document props mempty


mkDocumentStyleMap :: [ContributionID] -> Maybe RawContent -> Value
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


-- | FIXME: this should be delivered from where the instance ToJSON Style is defined, and that
-- instance should be defined in terms of this.
styleToST :: Style -> ST
styleToST s = case toJSON s of
  String txt -> txt
  _ -> error "impossible."


emptyEditorProps :: [PropertyOrHandler handler]
emptyEditorProps = ["editorState" &= createEmpty]

defaultEditorProps :: ConvertibleStrings s JSString => s -> [PropertyOrHandler handler]
defaultEditorProps txt = ["editorState" &= (createWithContent . createFromText . cs) txt]
