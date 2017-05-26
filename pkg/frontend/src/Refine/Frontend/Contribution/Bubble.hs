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

import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Language.Css.Syntax
import           Web.HttpApiData (toUrlPiece)

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

bubbleStackStyles :: [Decl]
bubbleStackStyles = [decl "border" (Ident "3px dotted black")]  -- TODO: style this!

bubble :: ReactElementM [SomeStoreAction] () -> View '[BubbleProps]
bubble children = mkView "Bubble" $ \props -> do
  let bubbleKind = case props ^. bubblePropsContributionIds of
          NoStack (ContribIDNote _)         -> "className" $= "o-snippet--note"
          NoStack (ContribIDQuestion _)     -> "className" $= "o-snippet--question"
          NoStack (ContribIDDiscussion _)   -> "className" $= "o-snippet--discussion"
          NoStack (ContribIDEdit _)         -> "className" $= "o-snippet--edit"
          NoStack ContribIDHighlightMark    -> "style" @@= []  -- this is an internal error.
          Stack _                           -> "style" @@= bubbleStackStyles

      showMouseOver = classNamesAny [("o-snippet--hover", isit)]
        where
          isit = case (props ^. bubblePropsHighlightedBubble, props ^. bubblePropsContributionIds) of
            (Nothing,  _)            -> False
            (Just cid, NoStack cid') -> cid == cid'
            (Just cid, Stack cids')  -> cid `elem` cids'

      attrs =
       [ "className" $= "o-snippet"  -- RENAME: snippet => bubble
       , bubbleKind
       , showMouseOver
       , verticalPosition (props ^. bubblePropsMarkPosition) (props ^. bubblePropsScreenState)

       , onClick      $ mkClickHandler (props ^. bubblePropsClickActions)
       , onMouseEnter $ mkClickHandler [HighlightMarkAndBubble . stackHead $ props ^. bubblePropsContributionIds]
                                           -- TODO: send the entire list, not just the head.
       , onMouseLeave $ mkClickHandler [UnhighlightMarkAndBubble]
       ]

  div_ attrs $ do
    div_ ["className" $= cs ("o-snippet__icon-bg o-snippet__icon-bg--" <> show (props ^. bubblePropsIconSide))] $ do  -- RENAME: snippet => bubble
      icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) Medium)  -- RENAME: snippet => bubble
    div_ ["className" $= "o-snippet__content"]  -- RENAME: snippet => bubble
      children

bubble_ :: BubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
bubble_ !props children = view_ (bubble children) (bubbleKey props) props
  -- (there is React.Flux.Internal.childrenPassedToView, but doing it by hand is easier to understand.)

bubbleKey :: BubbleProps -> JSString
bubbleKey props = "bubble_" <> props ^. bubblePropsContributionIds . to (cs . toUrlPiece . stackHead)

specialBubbleKey :: SpecialBubbleProps -> JSString
specialBubbleKey props = "bubble_" <> props ^. specialBubblePropsContributionId . to (cs . toUrlPiece)

verticalPosition :: Maybe MarkPosition -> ScreenState -> PropertyOrHandler handler
verticalPosition Nothing                        _  = "style" @@= [decl "margin-top" (Px 20)]
verticalPosition (Just (MarkPosition offset _)) st = "style" @@= [decl "top" (Px $ offsetIntoText offset st)]


discussionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
discussionBubble children = mkView "DiscussionBubble" $ \(SpecialBubbleProps cid markPosition highlight screenState) ->
  bubble_ (BubbleProps (NoStack cid) BubbleLeft ("icon-Discussion", "bright") markPosition highlight
                       [ShowContributionDialog cid] screenState)
    children

discussionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
discussionBubble_ !props children = view_ (discussionBubble children) (specialBubbleKey props) props

questionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
questionBubble children = mkView "QuestionBubble" $ \(SpecialBubbleProps cid markPosition highlight screenState) ->
  bubble_ (BubbleProps (NoStack cid) BubbleLeft ("icon-Question", "dark") markPosition highlight [] screenState)
    children

questionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
questionBubble_ !props children = view_ (questionBubble children) (specialBubbleKey props) props

noteBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
noteBubble children = mkView "NoteBubble" $ \(SpecialBubbleProps cid markPosition highlight screenState) ->
  bubble_ (BubbleProps (NoStack cid) BubbleLeft ("icon-Note", "dark") markPosition highlight
                       [ShowContributionDialog cid] screenState)
    children

noteBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
noteBubble_ !props children = view_ (noteBubble children) (specialBubbleKey props) props

editBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
editBubble children = mkView "EditBubble" $ \(SpecialBubbleProps cid markPosition highlight screenState) ->
  bubble_ (BubbleProps (NoStack cid) BubbleRight ("icon-Edit", "dark") markPosition highlight
                       [ShowContributionDialog cid] screenState)
    children

editBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
editBubble_ !props children = view_ (editBubble children) (specialBubbleKey props) props


-- * stacking

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
    go usedHeight (overlapping : overlappings) xs
      = save (overlapping :| overlappings) : go usedHeight [] xs

    updateUsedHeight old x = max old (getPos x + getHeight x)

    save cps@(_ :| (_:_)) = Stack (NEL.reverse cps)
    save (cp :| _)        = NoStack cp
