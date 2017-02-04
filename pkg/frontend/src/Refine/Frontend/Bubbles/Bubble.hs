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

--import           Control.Lens ((^.))
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
  { _dataHunkId2 :: Int64
  , _dataContentType2 :: String
  , _iconSide :: String
  , _iconStyle :: IconDescription
  , _markPosition :: Maybe (Int, Int)
  , _screenState :: SC.ScreenState
  }

bubble :: ReactView BubbleProps
bubble = defineView "Bubble" $ \props ->
        case _markPosition props of
            Nothing -> mempty
            Just (pos, scroll) ->
                div_ ["data-chunk-id" $= fromString (show (_dataHunkId2 props))
                    , "data-content-type" $= fromString (_dataContentType2 props)
                    , "className" $= fromString ("o-snippet o-snippet--" <> _dataContentType2 props)  -- RENAME: snippet => bubble
                    , "style" @= [Style "top" (SC.offsetIntoText pos scroll (_screenState props))]
                    ] $ do
                    div_ ["className" $= fromString ("o-snippet__icon-bg o-snippet__icon-bg--" <> _iconSide props)] $ do  -- RENAME: snippet => bubble
                        icon_ (IconProps "o-snippet" False (_iconStyle props) M)  -- RENAME: snippet => bubble
                    div_ ["className" $= "o-snippet__content"] childrenPassedToView  -- RENAME: snippet => bubble

bubble_ :: BubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
bubble_ = view bubble


discussionBubble :: ReactView (Int64, Maybe (Int, Int), SC.ScreenState)
discussionBubble = defineView "DiscussionBubble" $ \(dataHunkId, markPosition, screenState) ->
    bubble_ (BubbleProps dataHunkId "discussion" "left" ("icon-Discussion", "bright") markPosition screenState) childrenPassedToView

discussionBubble_ :: Int64 -> Maybe (Int, Int) -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
discussionBubble_ dataHunkId markPosition screenState = view discussionBubble (dataHunkId, markPosition, screenState)

questionBubble :: ReactView (Int64, RS.MarkPositions, SC.ScreenState)
questionBubble = defineView "QuestionBubble" $ \(dataHunkId, RS.MarkPositions markPositions, screenState) ->
    bubble_ (BubbleProps dataHunkId "question" "left" ("icon-Question", "dark") (M.lookup dataHunkId markPositions) screenState) childrenPassedToView

questionBubble_ :: Int64 -> RS.MarkPositions -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
questionBubble_ dataHunkId markPositions screenState = view questionBubble (dataHunkId, markPositions, screenState)

noteBubble :: ReactView (Int64, Maybe (Int, Int), SC.ScreenState)
noteBubble = defineView "NoteBubble" $ \(dataHunkId, markPosition, screenState) ->
    bubble_ (BubbleProps dataHunkId "question" "left" ("icon-Question", "dark") markPosition screenState) childrenPassedToView

noteBubble_ :: Int64 -> Maybe (Int, Int) -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
noteBubble_ dataHunkId markPosition screenState = view noteBubble (dataHunkId, markPosition, screenState)

editBubble :: ReactView (Int64, RS.MarkPositions, SC.ScreenState)
editBubble = defineView "EditBubble" $ \(dataHunkId, RS.MarkPositions markPositions, screenState) ->
    bubble_ (BubbleProps dataHunkId "edit" "right" ("icon-Edit", "dark") (M.lookup dataHunkId markPositions) screenState) childrenPassedToView

editBubble_ :: Int64 -> RS.MarkPositions -> SC.ScreenState -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editBubble_ dataHunkId markPositions screenState = view editBubble (dataHunkId, markPositions, screenState)
