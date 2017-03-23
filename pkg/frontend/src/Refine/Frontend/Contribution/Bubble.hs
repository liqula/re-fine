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

module Refine.Frontend.Contribution.Bubble where

import           Control.Lens ((^.))
import           Data.Monoid ((<>))
import           Data.String.Conversions (cs)
import           React.Flux hiding (callback)
import           Web.HttpApiData (toUrlPiece)

import           Refine.Common.Types.Contribution
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Screen.Calculations
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Style
import           Refine.Frontend.Types
import           Refine.Frontend.UtilityWidgets


bubble :: ReactElementM [SomeStoreAction] () -> View '[BubbleProps]
bubble children = mkView "Bubble" $ \props ->
  case props ^. bubblePropsMarkPosition of
    Nothing -> renderBubble children props 0
        -- FIXME: should be mempty, and the contents should be accessible elsewhere.  but this is
        -- good for testing, especially stacks.
    Just (MarkPosition topOffset _) -> renderBubble children props topOffset

renderBubble :: ReactElementM [SomeStoreAction] () -> BubbleProps -> OffsetFromDocumentTop -> ReactElementM [SomeStoreAction] ()
renderBubble children props topOffset = do
  let contribKind = case props ^. bubblePropsDataContribId of
          ContribIDNote _         -> ("o-snippet--note",       True)
          ContribIDQuestion _     -> ("o-snippet--question",   True)
          ContribIDDiscussion _   -> ("o-snippet--discussion", True)
          ContribIDEdit _         -> ("o-snippet--edit",       True)
          ContribIDHighlightMark  -> ("", False)
  div_ ["data-contribution-id" $= cs (toUrlPiece $ props ^. bubblePropsDataContribId)
       , classNames [ ("o-snippet", True)  -- RENAME: snippet => bubble
                    , contribKind
                    , ("o-snippet--hover", Just (props ^. bubblePropsDataContribId) == props ^. bubblePropsHighlightedBubble)
                    ]
       , "style" @= [Style "top" (offsetIntoText topOffset (props ^. bubblePropsScreenState))]
       , onClick $ const . (props ^. bubblePropsClickHandler)
       , onMouseEnter $ \_ _ -> dispatch . ContributionAction . HighlightMarkAndBubble $ props ^. bubblePropsDataContribId
       , onMouseLeave $ \_ _ -> dispatch $ ContributionAction UnhighlightMarkAndBubble
       ] $ do
    div_ ["className" $= cs ("o-snippet__icon-bg o-snippet__icon-bg--" <> props ^. bubblePropsIconSide)] $ do  -- RENAME: snippet => bubble
      icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) M)  -- RENAME: snippet => bubble
    div_ ["className" $= "o-snippet__content"]  -- RENAME: snippet => bubble
      children

bubble_ :: BubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
bubble_ !props children = view_ (bubble children) "bubble_" props
  -- (there is React.Flux.Internal.childrenPassedToView, but doing it by hand is easier to understand.)

discussionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
discussionBubble children = mkView "DiscussionBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
    let clickHandler _ = dispatch (ContributionAction (ShowContributionDialog contributionID))
    in bubble_ (BubbleProps contributionID "left" ("icon-Discussion", "bright") markPosition highlight clickHandler screenState)
        children

discussionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
discussionBubble_ !props children = view_ (discussionBubble children) "discussionBubble_" props

questionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
questionBubble children = mkView "QuestionBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "left" ("icon-Question", "dark") markPosition highlight clickHandler screenState)
        children

questionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
questionBubble_ !props children = view_ (questionBubble children) "questionBubble_" props

noteBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
noteBubble children = mkView "NoteBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
    let clickHandler _ = dispatch (ContributionAction (ShowContributionDialog contributionID))
    in bubble_ (BubbleProps contributionID "left" ("icon-Note", "dark") markPosition highlight clickHandler screenState)
        children

noteBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
noteBubble_ !props children = view_ (noteBubble children) "noteBubble_" props

editBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
editBubble children = mkView "EditBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "right" ("icon-Edit", "dark") markPosition highlight clickHandler screenState)
        children

editBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
editBubble_ !props children = view_ (editBubble children) "editBubble_" props
