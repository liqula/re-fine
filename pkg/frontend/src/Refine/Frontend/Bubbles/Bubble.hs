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

import           Control.Lens ((^.), makeLenses, to)
import           Data.Maybe (isJust, fromJust)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.Void
import           React.Flux hiding (callback)

import           Refine.Common.Types
import qualified Refine.Frontend.Bubbles.Types as RT
import qualified Refine.Frontend.Screen.Calculations as SC
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import qualified Refine.Frontend.Types as RT
import           Refine.Frontend.UtilityWidgets
import           Refine.Prelude (ClearTypeParameter(..))


data BubbleProps = BubbleProps
  { _bubblePropsDataChunkId :: ID Void
  , _bubblePropsDataContentType :: String
  , _bubblePropsIconSide :: String
  , _bubblePropsIconStyle :: IconDescription
  , _bubblePropsMarkPosition :: Maybe (SC.OffsetFromViewportTop, SC.ScrollOffsetOfViewport)
  , _bubblePropsHighlightedBubble :: Maybe (ID Void)
  , _bubblePropsClickHandler :: ClickHandler
  , _bubblePropsScreenState :: SC.ScreenState
  }

makeLenses ''BubbleProps

bubble :: ReactView BubbleProps
bubble = defineView "Bubble" $ \props ->
        case props ^. bubblePropsMarkPosition of
            Nothing -> mempty
            Just (topOffset, scrollOffset) ->
                div_ ["data-contribution-id" $= fromString (show (props ^. bubblePropsDataChunkId . unID))
                    , "data-content-type" $= fromString (props ^. bubblePropsDataContentType)
                    -- RENAME: snippet => bubble
                    , classNames [ ("o-snippet", True)
                                  , (fromString $ "o-snippet--" <> props ^. bubblePropsDataContentType, True)
                                  , ("o-snippet--hover", isJust (props ^. bubblePropsHighlightedBubble)
                                         && props ^. bubblePropsDataChunkId . unID == props ^. bubblePropsHighlightedBubble . to fromJust . unID)
                                  ]
                    , "style" @= [Style "top" (SC.offsetIntoText topOffset scrollOffset (props ^. bubblePropsScreenState))]
                    , onClick $ const . (props ^. bubblePropsClickHandler)
                    , onMouseEnter $ \_ _ -> RS.dispatch . RT.BubblesAction . RT.HighlightMarkAndBubble . clearTypeParameter $ props ^. bubblePropsDataChunkId
                    , onMouseLeave $ \_ _ -> RS.dispatch $ RT.BubblesAction RT.UnhighlightMarkAndBubble
                    ] $ do
                    div_ ["className" $= fromString ("o-snippet__icon-bg o-snippet__icon-bg--" <> props ^. bubblePropsIconSide)] $ do  -- RENAME: snippet => bubble
                        icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) M)  -- RENAME: snippet => bubble
                    div_ ["className" $= "o-snippet__content"] childrenPassedToView  -- RENAME: snippet => bubble

bubble_ :: BubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
bubble_ = view bubble

data SpecialBubbleProps = SpecialBubbleProps
  { _specialBubblePropsDataChunkId       :: ID Void
  , _specialBubblePropsMarkPosition      :: Maybe (SC.OffsetFromViewportTop, SC.ScrollOffsetOfViewport)
  , _specialBubblePropsHighlightedBubble :: Maybe (ID Void)
  , _specialBubblePropsScreenState       :: SC.ScreenState
  }

discussionBubble :: ReactView SpecialBubbleProps
discussionBubble = defineView "DiscussionBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = RS.dispatch (RT.BubblesAction (RT.ShowDiscussionOverlay (ID (dataChunkId ^. unID))))
    in bubble_ (BubbleProps dataChunkId "discussion" "left" ("icon-Discussion", "bright") markPosition highlight clickHandler screenState) childrenPassedToView

discussionBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
discussionBubble_ = view discussionBubble

questionBubble :: ReactView SpecialBubbleProps
questionBubble = defineView "QuestionBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") markPosition highlight clickHandler screenState) childrenPassedToView

questionBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
questionBubble_ = view questionBubble

noteBubble :: ReactView SpecialBubbleProps
noteBubble = defineView "NoteBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = RS.dispatch (RT.BubblesAction (RT.ShowNoteOverlay (ID (dataChunkId ^. unID))))
    in bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") markPosition highlight clickHandler screenState) childrenPassedToView

noteBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
noteBubble_ = view noteBubble

editBubble :: ReactView SpecialBubbleProps
editBubble = defineView "EditBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "edit" "right" ("icon-Edit", "dark") markPosition highlight clickHandler screenState) childrenPassedToView

editBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editBubble_ = view editBubble
