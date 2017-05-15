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

import           Control.Lens ((^?), (^.), (.~), (&), has, view)
import           Data.Aeson
import           Data.Aeson.Types (Pair)
import           Data.String.Conversions
import qualified Data.Text as ST
import           GHCJS.Types
import           React.Flux
import qualified React.Flux.Outdated as Outdated

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Store
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (editor_)


document :: Outdated.ReactView DocumentProps
document = Outdated.defineLifecycleView "Document" () Outdated.lifecycleConfig
  { Outdated.lRender = \() props -> liftViewToStateHandler $ do
      article_ [ "id" $= "vdocValue"  -- FIXME: do we still use this?
               , "className" $= "gr-20 gr-14@desktop editor_wrapper c-article-content"
               ] $ do
        let dstate = props ^. dpDocumentState
        editor_
          [ "editorState" &= (dstate ^. documentStateVal)
          , "customStyleMap" &= documentStyleMap (dstate ^? documentStateContent)
          , setReadOnly (has _DocumentStateView dstate)
          , onChange $ \evt ->
              let dstate' :: DocumentState
                  dstate' = dstate & documentStateVal .~ updateEditorState evt
              in dispatch . DocumentAction . DocumentUpdate $ dstate'
          ] mempty

  , Outdated.lComponentDidMount = Just $ \getPropsAndState _ldom _setState -> do
      props <- Outdated.lGetProps getPropsAndState
      ()    <- Outdated.lGetState getPropsAndState  -- (just to show there's nothing there)
      dispatchAndExec . ContributionAction =<< setMarkPositions (props ^. dpDocumentState)
  }

document_ :: DocumentProps -> ReactElementM eventHandler ()
document_ props = Outdated.view document props mempty


-- | in read-only mode, onchange is not fired even on selection.
-- https://github.com/facebook/draft-js/issues/690#issuecomment-282824570
setReadOnly :: forall handler. Bool -> PropertyOrHandler handler
setReadOnly ro = "handleBeforeInput" &= js_setReadOnly ro

foreign import javascript unsafe
  "function() { return $1 ? 'handled' : 'unhandled'; }"
  js_setReadOnly :: Bool -> JSVal


documentStyleMap :: Maybe RawContent -> Value
documentStyleMap Nothing = object []
documentStyleMap (Just rawContent) = object . mconcat $ go <$> marks
  where
    marks :: [Style]
    marks = map snd . mconcat $ view blockStyles <$> (rawContent ^. rawContentBlocks)

    go :: Style -> [Pair]
    go s@(Mark cid) = [markToST s .= mksty cid]
    go _ = []

    mksty :: ContributionID -> Value
    mksty (ContribIDNote i)       = bg   0 255 (shade i) 0.3
    mksty (ContribIDQuestion i)   = bg   0 255 (shade i) 0.3
    mksty (ContribIDDiscussion i) = bg   0 255 (shade i) 0.3
    mksty (ContribIDEdit i)       = bg 255   0 (shade i) 0.3
    mksty ContribIDHighlightMark  = bg 255 255 0         0.3

    bg :: Int -> Int -> Int -> Double -> Value
    bg r g b t = object ["background" .= String s]
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
