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
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Bubbles.Bubble where

import           Control.Lens (makeLenses, (^.))
import           Data.Int
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux

import qualified Refine.Frontend.Screen.Calculations as SC
import qualified Refine.Frontend.Screen.Types as SC
import           Refine.Frontend.Style
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets


data BubbleProps = BubbleProps
  { _bubblePropsDataChunkId :: Int64
  , _bubblePropsDataContentType :: String
  , _bubblePropsIconSide :: String
  , _bubblePropsIconStyle :: IconDescription
  , _bubblePropsMarkPosition :: Maybe (Int, Int)
  , _bubblePropsScreenState :: SC.ScreenState
  }

makeLenses ''BubbleProps

bubble :: ReactView BubbleProps
bubble = defineView "Bubble" $ \props ->
        case props ^. bubblePropsMarkPosition of
            Nothing -> mempty
            Just (pos, scroll) ->
                div_ ["data-chunk-id" $= fromString (show (props ^. bubblePropsDataChunkId))
                    , "data-content-type" $= fromString (props ^. bubblePropsDataContentType)
                    , "className" $= fromString ("o-snippet o-snippet--" <> props ^. bubblePropsDataContentType)  -- RENAME: snippet => bubble
                    , "style" @= [Style "top" (SC.offsetIntoText pos scroll (props ^. bubblePropsScreenState))]
                    ] $ do
                    div_ ["className" $= fromString ("o-snippet__icon-bg o-snippet__icon-bg--" <> props ^. bubblePropsIconSide)] $ do  -- RENAME: snippet => bubble
                        icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) M)  -- RENAME: snippet => bubble
                    div_ ["className" $= "o-snippet__content"] childrenPassedToView  -- RENAME: snippet => bubble

bubble_ :: BubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
bubble_ = view bubble


discussionBubble :: ReactView (Int64, Maybe (Int, Int), SC.ScreenState)
discussionBubble = defineView "DiscussionBubble" $ \(dataChunkId, markPosition, screenState) ->
    bubble_ (BubbleProps dataChunkId "discussion" "left" ("icon-Discussion", "bright") markPosition screenState) childrenPassedToView

discussionBubble_ :: Int64 -> Maybe (Int, Int) -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
discussionBubble_ dataChunkId markPosition screenState = view discussionBubble (dataChunkId, markPosition, screenState)

questionBubble :: ReactView (Int64, RS.MarkPositions, SC.ScreenState)
questionBubble = defineView "QuestionBubble" $ \(dataChunkId, RS.MarkPositions markPositions, screenState) ->
    bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") (M.lookup dataChunkId markPositions) screenState) childrenPassedToView

questionBubble_ :: Int64 -> RS.MarkPositions -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
questionBubble_ dataChunkId markPositions screenState = view questionBubble (dataChunkId, markPositions, screenState)

noteBubble :: ReactView (Int64, Maybe (Int, Int), SC.ScreenState)
noteBubble = defineView "NoteBubble" $ \(dataChunkId, markPosition, screenState) ->
    bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") markPosition screenState) childrenPassedToView

noteBubble_ :: Int64 -> Maybe (Int, Int) -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
noteBubble_ dataChunkId markPosition screenState = view noteBubble (dataChunkId, markPosition, screenState)

editBubble :: ReactView (Int64, RS.MarkPositions, SC.ScreenState)
editBubble = defineView "EditBubble" $ \(dataChunkId, RS.MarkPositions markPositions, screenState) ->
    bubble_ (BubbleProps dataChunkId "edit" "right" ("icon-Edit", "dark") (M.lookup dataChunkId markPositions) screenState) childrenPassedToView

editBubble_ :: Int64 -> RS.MarkPositions -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editBubble_ dataChunkId markPositions screenState = view editBubble (dataChunkId, markPositions, screenState)
