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

module Refine.Frontend.Bubbles.Mark where

import           Control.Concurrent (forkIO)
import           Control.Lens (makeLenses, (^.))
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.String.Conversions
import           Data.Void
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Lifecycle
import qualified Text.HTML.Parser as HTMLP
import           Text.Read (readMaybe)

import           Refine.Common.Types
import qualified Refine.Frontend.Screen.Types as RS
import qualified Refine.Frontend.Bubbles.Types as RS
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


data MarkProps = MarkProps
  { _markPropsHTMLAttributes :: [HTMLP.Attr]
  , _markPropsHighlightedMark :: Maybe (ID Void)
  }

makeLenses ''MarkProps

chunkIdFrom :: [HTMLP.Attr] -> Maybe (ID Void)
chunkIdFrom attrs = ID <$> readMaybe (attribValueOf "data-chunk-id" attrs) :: Maybe (ID Void)

toProperties :: [HTMLP.Attr] -> [PropertyOrHandler handler]
toProperties = map (\(HTMLP.Attr key value) -> fromString (cs key) $= fromString (cs value))

attribValueOf :: String -> [HTMLP.Attr] -> String
attribValueOf _ [] = ""
attribValueOf wantedKey (HTMLP.Attr key value:_) | key == cs wantedKey = cs value
attribValueOf wantedKey (_:as) = attribValueOf wantedKey as

rfMark :: ReactView MarkProps
rfMark = defineLifecycleView "RefineMark" () lifecycleConfig
  { lRender = \_state props ->
    let maybeChunkId = chunkIdFrom (props ^. markPropsHTMLAttributes)
        dataContentType = attribValueOf "data-chunk-kind" (props ^. markPropsHTMLAttributes)
    in case (maybeChunkId, dataContentType) of
      (Nothing, _) -> mempty
      (_, "") -> mempty
      (Just dataChunkId, _) ->
        mark_ (toProperties (props ^. markPropsHTMLAttributes) <>
           [ classNames [ ("o-mark", True)
                        , (fromString $ "o-mark--" <> dataContentType, True)
                        , ("o-mark--hover", maybeChunkId == props ^. markPropsHighlightedMark)
                        ]
           , onMouseEnter $ \_ _ _ -> (RS.dispatch . RS.BubblesAction $ RS.HighlightMarkAndBubble dataChunkId, Nothing)
           , onMouseLeave $ \_ _ _ -> (RS.dispatch $ RS.BubblesAction RS.UnhighlightMarkAndBubble, Nothing)
           ]) childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             this <- lThis ldom
             top <- js_getBoundingClientRectTop this
             props <- lGetProps propsandstate
             _ <- forkIO $ do
               case chunkIdFrom (props ^. markPropsHTMLAttributes) of
                 Nothing -> return ()
                 Just dataChunkId -> do
                   let actions = RS.dispatch . RS.BubblesAction $ RS.AddMarkPosition dataChunkId top 0 -- we assume that no scrolling has taken place yet
                   forM_ actions executeAction
             return ()
   }

rfMark_ :: MarkProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
rfMark_ = view rfMark

foreign import javascript unsafe
  "$1.getBoundingClientRect().top"
  js_getBoundingClientRectTop :: JSVal -> IO RS.OffsetFromViewportTop
