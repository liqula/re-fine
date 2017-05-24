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

module Refine.Frontend.Contribution.Bubble
  ( bubble_
  , noteBubble_
  , questionBubble_
  , discussionBubble_
  , editBubble_
  ) where

import Refine.Frontend.Prelude

import           Web.HttpApiData (toUrlPiece)
import           Language.Css.Syntax

import           Refine.Common.Types.Core
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Screen.Calculations
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util


mkClickHandler :: [ContributionAction] -> Event -> MouseEvent -> [SomeStoreAction]
mkClickHandler actions _ _ = dispatchMany $ ContributionAction <$> actions

bubble :: ReactElementM [SomeStoreAction] () -> View '[BubbleProps]
bubble children = mkView "Bubble" $ \props ->
  case props ^. bubblePropsMarkPosition of
    Nothing -> renderBubble children props 0
        -- FIXME: should be mempty, and the contents should be accessible elsewhere.  but this is
        -- good for testing, especially stacks.
        -- FIXME: 'OffsetFromDocumentTop' should be part of the props, not a separate parameter.
    Just (MarkPosition topOffset _) -> renderBubble children props topOffset

bubble_ :: BubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
bubble_ !props children = view_ (bubble children) (bubbleKey props) props
  -- (there is React.Flux.Internal.childrenPassedToView, but doing it by hand is easier to understand.)

bubbleKey :: BubbleProps -> JSString
bubbleKey props = "bubble_" <> props ^. bubblePropsContributionId . to (cs . toUrlPiece)

specialBubbleKey :: SpecialBubbleProps -> JSString
specialBubbleKey props = "bubble_" <> props ^. specialBubblePropsContributionId . to (cs . toUrlPiece)

renderBubble :: ReactElementM [SomeStoreAction] () -> BubbleProps -> OffsetFromDocumentTop -> ReactElementM [SomeStoreAction] ()
renderBubble children props topOffset = do
  let contribKind = case props ^. bubblePropsContributionId of
          ContribIDNote _         -> ("o-snippet--note",       True)
          ContribIDQuestion _     -> ("o-snippet--question",   True)
          ContribIDDiscussion _   -> ("o-snippet--discussion", True)
          ContribIDEdit _         -> ("o-snippet--edit",       True)
          ContribIDHighlightMark  -> ("", False)
  div_ ["data-contribution-id" $= cs (toUrlPiece $ props ^. bubblePropsContributionId)
       , classNamesAny
                    [ ("o-snippet", True)  -- RENAME: snippet => bubble
                    , contribKind
                    , ("o-snippet--hover", Just (props ^. bubblePropsContributionId) == props ^. bubblePropsHighlightedBubble)
                    ]
       , "style" @@= [decl "top" (Px $ offsetIntoText topOffset (props ^. bubblePropsScreenState))]
       , onClick      $ mkClickHandler (props ^. bubblePropsClickActions)
       , onMouseEnter $ mkClickHandler [HighlightMarkAndBubble $ props ^. bubblePropsContributionId]
       , onMouseLeave $ mkClickHandler [UnhighlightMarkAndBubble]
       ] $ do
    div_ ["className" $= cs ("o-snippet__icon-bg o-snippet__icon-bg--" <> props ^. bubblePropsIconSide)] $ do  -- RENAME: snippet => bubble
      icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) Medium)  -- RENAME: snippet => bubble
    div_ ["className" $= "o-snippet__content"]  -- RENAME: snippet => bubble
      children


discussionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
discussionBubble children = mkView "DiscussionBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
  bubble_ (BubbleProps contributionID "left" ("icon-Discussion", "bright") markPosition highlight
                       [ShowContributionDialog contributionID] screenState)
    children

discussionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
discussionBubble_ !props children = view_ (discussionBubble children) (specialBubbleKey props) props

questionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
questionBubble children = mkView "QuestionBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
  bubble_ (BubbleProps dataChunkId "left" ("icon-Question", "dark") markPosition highlight [] screenState)
    children

questionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
questionBubble_ !props children = view_ (questionBubble children) (specialBubbleKey props) props

noteBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
noteBubble children = mkView "NoteBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
  bubble_ (BubbleProps contributionID "left" ("icon-Note", "dark") markPosition highlight
                       [ShowContributionDialog contributionID] screenState)
    children

noteBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
noteBubble_ !props children = view_ (noteBubble children) (specialBubbleKey props) props

editBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
editBubble children = mkView "EditBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
  bubble_ (BubbleProps dataChunkId "right" ("icon-Edit", "dark") markPosition highlight [ShowContributionDialog dataChunkId] screenState)
    children

editBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
editBubble_ !props children = view_ (editBubble children) (specialBubbleKey props) props
