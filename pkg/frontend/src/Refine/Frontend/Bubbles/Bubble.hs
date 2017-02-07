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
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux hiding (callback)

import           Refine.Common.Types
import           Refine.Frontend.Bubbles.Types
import qualified Refine.Frontend.Screen.Calculations as SC
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import qualified Refine.Frontend.Types as RT
import           Refine.Frontend.UtilityWidgets


data BubbleProps = BubbleProps
  { _bubblePropsDataChunkId :: Int64
  , _bubblePropsDataContentType :: String
  , _bubblePropsIconSide :: String
  , _bubblePropsIconStyle :: IconDescription
  , _bubblePropsMarkPosition :: Maybe (Int, Int)
  , _bubblePropsClickHandler :: ClickHandler
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
                    , onClick $ const . (props ^. bubblePropsClickHandler)
                    ] $ do
                    div_ ["className" $= fromString ("o-snippet__icon-bg o-snippet__icon-bg--" <> props ^. bubblePropsIconSide)] $ do  -- RENAME: snippet => bubble
                        icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) M)  -- RENAME: snippet => bubble
                    div_ ["className" $= "o-snippet__content"] childrenPassedToView  -- RENAME: snippet => bubble

bubble_ :: BubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
bubble_ = view bubble

data SpecialBubbleProps = SpecialBubbleProps
  { _specialBubblePropsDataChunkId :: Int64             -- This is the contents of the ID Discussion / ID Note etc.
  , _specialBubblePropsMarkPosition :: Maybe (Int, Int)
  , _specialBubblePropsScreenState :: SC.ScreenState
  }

discussionBubble :: ReactView SpecialBubbleProps
discussionBubble = defineView "DiscussionBubble" $ \(SpecialBubbleProps dataChunkId markPosition screenState) ->
    let clickHandler _ = RS.dispatch (RT.BubblesAction (ShowDiscussionOverlay (ID dataChunkId)))
    in bubble_ (BubbleProps dataChunkId "discussion" "left" ("icon-Discussion", "bright") markPosition clickHandler screenState) childrenPassedToView

discussionBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
discussionBubble_ = view discussionBubble

questionBubble :: ReactView SpecialBubbleProps
questionBubble = defineView "QuestionBubble" $ \(SpecialBubbleProps dataChunkId markPosition screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") markPosition clickHandler screenState) childrenPassedToView

questionBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
questionBubble_ = view questionBubble

noteBubble :: ReactView SpecialBubbleProps
noteBubble = defineView "NoteBubble" $ \(SpecialBubbleProps dataChunkId markPosition screenState) ->
    let clickHandler _ = RS.dispatch (RT.BubblesAction (ShowNoteOverlay (ID dataChunkId)))
    in bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") markPosition clickHandler screenState) childrenPassedToView

noteBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
noteBubble_ = view noteBubble

editBubble :: ReactView SpecialBubbleProps
editBubble = defineView "EditBubble" $ \(SpecialBubbleProps dataChunkId markPosition screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "edit" "right" ("icon-Edit", "dark") markPosition clickHandler screenState) childrenPassedToView

editBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editBubble_ = view editBubble
