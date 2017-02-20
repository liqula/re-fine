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

module Refine.Frontend.Contribution.Bubble where

import           Control.Lens ((^.), makeLenses)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.String.Conversions (cs)
import           React.Flux hiding (callback)
import           Web.HttpApiData (toUrlPiece)

import           Refine.Common.Types
import qualified Refine.Frontend.Contribution.Types as RT
import qualified Refine.Frontend.Screen.Calculations as SC
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import qualified Refine.Frontend.Types as RT
import           Refine.Frontend.UtilityWidgets


data BubbleProps = BubbleProps
  { _bubblePropsDataContribId :: ContributionID
  , _bubblePropsDataContentType :: String
  , _bubblePropsIconSide :: String
  , _bubblePropsIconStyle :: IconDescription
  , _bubblePropsMarkPosition :: Maybe RT.MarkPosition
  , _bubblePropsHighlightedBubble :: Maybe ContributionID
  , _bubblePropsClickHandler :: ClickHandler
  , _bubblePropsScreenState :: SC.ScreenState
  }

makeLenses ''BubbleProps

bubble :: ReactView BubbleProps
bubble = defineView "Bubble" $ \props ->
        case props ^. bubblePropsMarkPosition of
            Nothing -> mempty
            Just (RT.MarkPosition topOffset _) ->
                div_ ["data-contribution-id" $= (fromString . cs $ toUrlPiece (props ^. bubblePropsDataContribId))
                    , "data-content-type" $= fromString (props ^. bubblePropsDataContentType)
                    -- RENAME: snippet => bubble
                    , classNames [ ("o-snippet", True)
                                  , (fromString $ "o-snippet--" <> props ^. bubblePropsDataContentType, True)
                                  , ("o-snippet--hover", Just (props ^. bubblePropsDataContribId) == props ^. bubblePropsHighlightedBubble)
                                  ]
                    , "style" @= [Style "top" (SC.offsetIntoText topOffset (props ^. bubblePropsScreenState))]
                    , onClick $ const . (props ^. bubblePropsClickHandler)
                    , onMouseEnter $ \_ _ -> RS.dispatch . RT.ContributionAction . RT.HighlightMarkAndBubble $ props ^. bubblePropsDataContribId
                    , onMouseLeave $ \_ _ -> RS.dispatch $ RT.ContributionAction RT.UnhighlightMarkAndBubble
                    ] $ do
                    div_ ["className" $= fromString ("o-snippet__icon-bg o-snippet__icon-bg--" <> props ^. bubblePropsIconSide)] $ do  -- RENAME: snippet => bubble
                        icon_ (IconProps "o-snippet" False (props ^. bubblePropsIconStyle) M)  -- RENAME: snippet => bubble
                    div_ ["className" $= "o-snippet__content"] childrenPassedToView  -- RENAME: snippet => bubble

bubble_ :: BubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
bubble_ = view bubble

data SpecialBubbleProps = SpecialBubbleProps
  { _specialBubblePropsContributionId    :: ContributionID
  , _specialBubblePropsMarkPosition      :: Maybe RT.MarkPosition
  , _specialBubblePropsHighlightedBubble :: Maybe ContributionID
  , _specialBubblePropsScreenState       :: SC.ScreenState
  }

discussionBubble :: ReactView SpecialBubbleProps
discussionBubble = defineView "DiscussionBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
    let clickHandler _ = RS.dispatch (RT.ContributionAction (RT.ShowContributionDialog contributionID))
    in bubble_ (BubbleProps contributionID "discussion" "left" ("icon-Discussion", "bright") markPosition highlight clickHandler screenState) childrenPassedToView

discussionBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
discussionBubble_ = view discussionBubble

questionBubble :: ReactView SpecialBubbleProps
questionBubble = defineView "QuestionBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "question" "left" ("icon-Question", "dark") markPosition highlight clickHandler screenState) childrenPassedToView

questionBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
questionBubble_ = view questionBubble

noteBubble :: ReactView SpecialBubbleProps
noteBubble = defineView "NoteBubble" $ \(SpecialBubbleProps contributionID markPosition highlight screenState) ->
    let clickHandler _ = RS.dispatch (RT.ContributionAction (RT.ShowContributionDialog contributionID))
    in bubble_ (BubbleProps contributionID "question" "left" ("icon-Question", "dark") markPosition highlight clickHandler screenState) childrenPassedToView

noteBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
noteBubble_ = view noteBubble

editBubble :: ReactView SpecialBubbleProps
editBubble = defineView "EditBubble" $ \(SpecialBubbleProps dataChunkId markPosition highlight screenState) ->
    let clickHandler _ = []
    in bubble_ (BubbleProps dataChunkId "edit" "right" ("icon-Edit", "dark") markPosition highlight clickHandler screenState) childrenPassedToView

editBubble_ :: SpecialBubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editBubble_ = view editBubble
