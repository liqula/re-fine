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

  , stackComponents, StackOrNot(..)
  ) where

import Refine.Frontend.Prelude

import           Web.HttpApiData (toUrlPiece)
import           Language.Css.Syntax

import           Refine.Common.Types.Core
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Screen.Calculations
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util


mkClickHandler :: [ContributionAction] -> Event -> MouseEvent -> [SomeStoreAction]
mkClickHandler actions _ _ = dispatchMany $ ContributionAction <$> actions

bubble :: ReactElementM [SomeStoreAction] () -> View '[BubbleProps]
bubble children = mkView "Bubble" $ \props -> do
  let contribKind = case props ^. bubblePropsContributionId of
          ContribIDNote _         -> ("o-snippet--note",       True)
          ContribIDQuestion _     -> ("o-snippet--question",   True)
          ContribIDDiscussion _   -> ("o-snippet--discussion", True)
          ContribIDEdit _         -> ("o-snippet--edit",       True)
          ContribIDHighlightMark  -> ("", False)
  div_ [ classNamesAny
                    [ ("o-snippet", True)  -- RENAME: snippet => bubble
                    , contribKind
                    , ("o-snippet--hover", Just (props ^. bubblePropsContributionId) == props ^. bubblePropsHighlightedBubble)
                    ]
       , "style" @@= rendermpos (props ^. bubblePropsMarkPosition) (props ^. bubblePropsScreenState)
       , onClick      $ mkClickHandler (props ^. bubblePropsClickActions)
       , onMouseEnter $ mkClickHandler [HighlightMarkAndBubble $ props ^. bubblePropsContributionId]
       , onMouseLeave $ mkClickHandler [UnhighlightMarkAndBubble]
       ] $ do
    div_ ["className" $= cs ("o-snippet__icon-bg o-snippet__icon-bg--" <> show (props ^. bubblePropsIconSide))] $ do  -- RENAME: snippet => bubble
      icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) Medium)  -- RENAME: snippet => bubble
    div_ ["className" $= "o-snippet__content"]  -- RENAME: snippet => bubble
      children

bubble_ :: BubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
bubble_ !props children = view_ (bubble children) (bubbleKey props) props
  -- (there is React.Flux.Internal.childrenPassedToView, but doing it by hand is easier to understand.)

bubbleKey :: BubbleProps -> JSString
bubbleKey props = "bubble_" <> props ^. bubblePropsContributionId . to (cs . toUrlPiece)

specialBubbleKey :: SpecialBubbleProps -> JSString
specialBubbleKey props = "bubble_" <> props ^. specialBubblePropsContributionId . to (cs . toUrlPiece)

rendermpos :: Maybe MarkPosition -> ScreenState -> [Decl]
rendermpos Nothing                        _  = [decl "margin-top" (Px 20)]
rendermpos (Just (MarkPosition offset _)) st = [decl "top" (Px $ offsetIntoText offset st)]


discussionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
discussionBubble children = mkView "DiscussionBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
  bubble_ (BubbleProps contributionID BubbleLeft ("icon-Discussion", "bright") markPosition highlight
                       [ShowContributionDialog contributionID] screenState)
    children

discussionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
discussionBubble_ !props children = view_ (discussionBubble children) (specialBubbleKey props) props

questionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
questionBubble children = mkView "QuestionBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
  bubble_ (BubbleProps dataChunkId BubbleLeft ("icon-Question", "dark") markPosition highlight [] screenState)
    children

questionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
questionBubble_ !props children = view_ (questionBubble children) (specialBubbleKey props) props

noteBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
noteBubble children = mkView "NoteBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
  bubble_ (BubbleProps contributionID BubbleLeft ("icon-Note", "dark") markPosition highlight
                       [ShowContributionDialog contributionID] screenState)
    children

noteBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
noteBubble_ !props children = view_ (noteBubble children) (specialBubbleKey props) props

editBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
editBubble children = mkView "EditBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
  bubble_ (BubbleProps dataChunkId BubbleRight ("icon-Edit", "dark") markPosition highlight [ShowContributionDialog dataChunkId] screenState)
    children

editBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
editBubble_ !props children = view_ (editBubble children) (specialBubbleKey props) props


-- * stacking

data StackOrNot a = Stack [a] | NoStack a
  deriving (Eq, Ord, Show, Generic)

-- | given a list of abstract components together with their absolute position and height, group all
-- overlapping components into stacks, and leave all others single.
stackComponents :: forall pos height comp. (pos ~ Int, height ~ Int)
                => (comp -> pos) -> (comp -> height)
                -> [comp] -> [StackOrNot comp]
stackComponents getPos getHeight comps = assert (all (\c -> getPos c >= 0 && getHeight c >= 1) comps)
                                       . go 0 [] . sortBy (compare `on` getPos)
                                       $ comps
  where
    go _ [] []
      = []

    -- start a new pile if the old one is empty.
    go usedHeight [] (x : xs)
      = go (updateUsedHeight usedHeight x) [x] xs

    -- if the next one *does* overlap with the (non-empty) pile, add it.
    go usedHeight overlapping@(_:_) (x : xs) | getPos x <= usedHeight
      = go (updateUsedHeight usedHeight x) (x : overlapping) xs

    -- if the next one *does not* overlap with the (non-empty) pile, emit a new 'StackOrNot'
    go usedHeight overlapping@(_:_) xs
      = save overlapping : go usedHeight [] xs

    updateUsedHeight old x = max old (getPos x + getHeight x)

    save cps@(_:_:_) = Stack (reverse cps)
    save [cp]        = NoStack cp
    save []          = error "impossible"
