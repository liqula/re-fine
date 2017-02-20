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

module Refine.Frontend.Contribution.Mark where

import           Control.Lens (makeLenses, (^.))
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.String.Conversions
import           Data.String (fromString)
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Lifecycle
import qualified Text.HTML.Parser as HTMLP
import           Web.HttpApiData

import           Refine.Common.Types
import qualified Refine.Frontend.Screen.Types as RS
import qualified Refine.Frontend.Screen.Calculations as RS
import qualified Refine.Frontend.Contribution.Types as RS
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


data MarkProps = MarkProps
  { _markPropsHTMLAttributes :: [HTMLP.Attr]
  , _markPropsHighlightedMark :: Maybe ContributionID
  }

makeLenses ''MarkProps

contributionIdFrom :: [HTMLP.Attr] -> Maybe ContributionID
contributionIdFrom attrs = either (\_ -> Nothing) Just $ do
  let cnid = parseUrlPiece $ cs (attribValueOf "data-contribution-id" attrs)
  cnkind :: ContributionKind <- parseUrlPiece $ cs (attribValueOf "data-contribution-kind" attrs)
  case cnkind of
    ContribKindNote          -> ContribIDNote       . ID <$> cnid
    ContribKindQuestion      -> ContribIDQuestion   . ID <$> cnid
    ContribKindDiscussion    -> ContribIDDiscussion . ID <$> cnid
    ContribKindEdit          -> ContribIDEdit       . ID <$> cnid
    ContribKindHighlightMark -> pure ContribIDHighlightMark


toProperties :: [HTMLP.Attr] -> [PropertyOrHandler handler]
toProperties = map (\(HTMLP.Attr key value) -> fromString (cs key) $= fromString (cs value))

attribValueOf :: String -> [HTMLP.Attr] -> String
attribValueOf _ [] = ""
attribValueOf wantedKey (HTMLP.Attr key value:_) | key == cs wantedKey = cs value
attribValueOf wantedKey (_:as) = attribValueOf wantedKey as

rfMark :: ReactView MarkProps
rfMark = defineLifecycleView "RefineMark" () lifecycleConfig
  { lRender = \_state props ->
    let maybeChunkId = contributionIdFrom (props ^. markPropsHTMLAttributes)
        dataContentType = attribValueOf "data-contribution-kind" (props ^. markPropsHTMLAttributes)
    in case (maybeChunkId, dataContentType) of
      (Nothing, _) -> mempty
      (_, "") -> mempty
      (Just dataChunkId, _) ->
        mark_ (toProperties (props ^. markPropsHTMLAttributes) <>
           [ classNames [ ("o-mark", True)
                        , (fromString $ "o-mark--" <> dataContentType, True)
                        , ("o-mark--hover", maybeChunkId == props ^. markPropsHighlightedMark)
                        ]
           , onMouseEnter $ \_ _ _ -> (RS.dispatch . RS.ContributionAction $ RS.HighlightMarkAndBubble dataChunkId, Nothing)
           , onMouseLeave $ \_ _ _ -> (RS.dispatch $ RS.ContributionAction RS.UnhighlightMarkAndBubble, Nothing)
           ]) childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             this <- lThis ldom
             topOffset    <- js_getBoundingClientRectTop this
             bottomOffset <- js_getBoundingClientRectBottom this
             scrollOffset <- js_getScrollOffset
             props <- lGetProps propsandstate
             _ <- RS.reactFluxWorkAroundForkIO $ do
               case contributionIdFrom (props ^. markPropsHTMLAttributes) of
                 Nothing -> pure ()
                 Just dataChunkId -> do
                   let actions = RS.dispatch . RS.ContributionAction $ RS.AddMarkPosition dataChunkId markPosition
                       markPosition = RS.MarkPosition
                         { RS._markPositionTop    = RS.offsetFromDocumentTop topOffset    scrollOffset
                         , RS._markPositionBottom = RS.offsetFromDocumentTop bottomOffset scrollOffset
                         }
                   forM_ actions executeAction
             pure ()
   }

rfMark_ :: MarkProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
rfMark_ = view rfMark

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
