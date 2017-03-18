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

import           Refine.Frontend.Contribution.Types as RT
import qualified Refine.Frontend.Screen.Calculations as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import qualified Refine.Frontend.Types as RT
import           Refine.Frontend.UtilityWidgets


bubble :: ReactElementM [SomeStoreAction] () -> View '[BubbleProps]
bubble children = mkView "Bubble" $ \props ->
  case props ^. bubblePropsMarkPosition of
      Nothing -> mempty
      Just (RT.MarkPosition topOffset _) ->
          div_ ["data-contribution-id" $= cs (toUrlPiece $ props ^. bubblePropsDataContribId)
              , "data-content-type" $= cs (props ^. bubblePropsDataContentType)
              -- RENAME: snippet => bubble
              , classNames [ ("o-snippet", True)
                            , (cs $ "o-snippet--" <> props ^. bubblePropsDataContentType, True)
                            , ("o-snippet--hover", Just (props ^. bubblePropsDataContribId) == props ^. bubblePropsHighlightedBubble)
                            ]
              , "style" @= [Style "top" (SC.offsetIntoText topOffset (props ^. bubblePropsScreenState))]
              , onClick $ const . (props ^. bubblePropsClickHandler)
              , onMouseEnter $ \_ _ -> RS.dispatch . RT.ContributionAction . RT.HighlightMarkAndBubble $ props ^. bubblePropsDataContribId
              , onMouseLeave $ \_ _ -> RS.dispatch $ RT.ContributionAction RT.UnhighlightMarkAndBubble
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
    let clickHandler _ = RS.dispatch (RT.ContributionAction (RT.ShowContributionDialog contributionID))
    in bubble_ (BubbleProps contributionID "discussion" "left" ("icon-Discussion", "bright") markPosition highlight clickHandler screenState)
        children

discussionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
discussionBubble_ !props children = view_ (discussionBubble children) "discussionBubble_" props

questionBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
questionBubble children = mkView "QuestionBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") markPosition highlight clickHandler screenState)
        children

questionBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
questionBubble_ !props children = view_ (questionBubble children) "questionBubble_" props

noteBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
noteBubble children = mkView "NoteBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
    let clickHandler _ = RS.dispatch (RT.ContributionAction (RT.ShowContributionDialog contributionID))
    in bubble_ (BubbleProps contributionID "note" "left" ("icon-Note", "dark") markPosition highlight clickHandler screenState)
        children

noteBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
noteBubble_ !props children = view_ (noteBubble children) "noteBubble_" props

editBubble :: ReactElementM [SomeStoreAction] () -> View '[SpecialBubbleProps]
editBubble children = mkView "EditBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "edit" "right" ("icon-Edit", "dark") markPosition highlight clickHandler screenState)
        children

editBubble_ :: SpecialBubbleProps -> ReactElementM [SomeStoreAction] () -> ReactElementM [SomeStoreAction] ()
editBubble_ !props children = view_ (editBubble children) "editBubble_" props
