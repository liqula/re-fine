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

module Refine.Frontend.Contribution.Mark where

import           Control.Lens ((^.))
import           Data.Functor.Infix ((<$$>))
import           Data.String.Conversions
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Outdated

import           Refine.Common.Types
import qualified Refine.Frontend.Screen.Types as RS
import qualified Refine.Frontend.Screen.Calculations as RS
import           Refine.Frontend.Contribution.Types as RS
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Store.Types as RS
import           Refine.Frontend.Util (classNamesAny, toProperty)
import           Refine.Prelude()


rfMark :: ReactView MarkProps
rfMark = defineLifecycleView "RefineMark" () lifecycleConfig
  { lRender = \_state props -> do
      let dataContributionId = props ^. markPropsContributionID
          statefulEventHandlers :: [PropertyOrHandler (() -> ([SomeStoreAction], Maybe ()))]
          statefulEventHandlers = (\h _ -> (h, Nothing)) <$$> (toProperty <$> props ^. markPropsAttrs)
      mark_ (statefulEventHandlers <>
           [ classNamesAny
                        [ ("o-mark", True)
                        , ("o-mark--highlight", Just dataContributionId == props ^. markPropsDisplayedContribution)
                        , ("o-mark--hover",     Just dataContributionId == props ^. markPropsHighlightedMark)
                        , (cs $ "o-mark--" <> contributionIDToKindST dataContributionId,
                                                Just dataContributionId /= props ^. markPropsDisplayedContribution)
                        ]
           , onMouseEnter $ \_ _ _ -> (RS.dispatch . RS.ContributionAction $ RS.HighlightMarkAndBubble dataContributionId, Nothing)
           , onMouseLeave $ \_ _ _ -> (RS.dispatch $ RS.ContributionAction RS.UnhighlightMarkAndBubble, Nothing)
           ]) childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             props  <- lGetProps propsandstate
             mark   <- lThis ldom
             action <- readMarkPosition (props ^. markPropsContributionID) mark
             RS.reactFluxWorkAroundForkIO $ RS.dispatchM action
   }

rfMark_ :: MarkProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
rfMark_ = view rfMark


-- | (this is also a hidden type in React.Flux.Lifecycle)
type HTMLElement = JSVal

readMarkPosition :: ContributionID -> HTMLElement -> IO GlobalAction
readMarkPosition dataContributionId element = do
  topOffset    <- js_getBoundingClientRectTop element
  bottomOffset <- js_getBoundingClientRectBottom element
  scrollOffset <- js_getScrollOffset
  let markPosition = RS.MarkPosition
        { RS._markPositionTop    = RS.offsetFromDocumentTop topOffset    scrollOffset
        , RS._markPositionBottom = RS.offsetFromDocumentTop bottomOffset scrollOffset
        }
      action = RS.ContributionAction $ RS.AddMarkPosition dataContributionId markPosition
  pure action

foreign import javascript unsafe
  "$1.getBoundingClientRect().top"
  js_getBoundingClientRectTop :: JSVal -> IO RS.OffsetFromViewportTop

foreign import javascript unsafe
  "$1.getBoundingClientRect().bottom"
  js_getBoundingClientRectBottom :: JSVal -> IO RS.OffsetFromViewportTop

foreign import javascript unsafe
  "typeof( window.pageYOffset ) === 'number' && window.pageYOffset \
  \ || document.body && document.body.scrollTop \
  \ || document.documentElement && document.documentElement.scrollTop"
  js_getScrollOffset :: IO RS.ScrollOffsetOfViewport
