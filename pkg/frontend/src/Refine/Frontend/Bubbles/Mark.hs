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
import           Data.Maybe (isJust, fromJust)
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
import           Refine.Prelude (ClearTypeParameter(..))


data MarkAttributes = MarkAttributes
  { _markPropsDataChunkId :: ID Void
  , _markPropsDataContentType :: String
  }

makeLenses ''MarkAttributes

toMarkAttributes :: [HTMLP.Attr] -> Maybe MarkAttributes
toMarkAttributes attrs = let maybeChunkId = ID <$> readMaybe (valueOf "data-chunk-id" attrs) :: Maybe (ID Void)
  in fmap (`MarkAttributes` valueOf "data-chunk-kind" attrs) maybeChunkId
  where
    valueOf :: String -> [HTMLP.Attr] -> String
    valueOf _ [] = ""
    valueOf wantedKey (HTMLP.Attr key value:_) | key == cs wantedKey = cs value
    valueOf wantedKey (_:as) = valueOf wantedKey as

data MarkProps = MarkProps
  { _markPropsAttributes :: Maybe MarkAttributes
  , _markPropsHighlightedMark :: Maybe (ID Void)
  }

makeLenses ''MarkProps

rfMark :: ReactView MarkProps
rfMark = defineLifecycleView "RefineMark" () lifecycleConfig
   { lRender = \_state props -> case props of
         MarkProps Nothing _ -> mempty
         MarkProps (Just p) highlight ->
           mark_ [ "data-chunk-id" $= fromString (show (p ^. markPropsDataChunkId ^. unID))
                 , classNames [ ("o-mark", True)
                              , (fromString $ "o-mark--" <> p ^. markPropsDataContentType, True)
                              , ("o-mark--hover", isJust highlight
                                     && p ^. markPropsDataChunkId ^. unID == fromJust highlight ^. unID)
                              ]
                 , onMouseEnter $ \_ _ _ -> (RS.dispatch . RS.BubblesAction . RS.HighlightMarkAndBubble . clearTypeParameter $ p ^. markPropsDataChunkId, Nothing)
                 , onMouseLeave $ \_ _ _ -> (RS.dispatch $ RS.BubblesAction RS.UnhighlightMarkAndBubble, Nothing)
                 ] childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             this <- lThis ldom
             top <- js_getBoundingClientRectTop this
             props <- lGetProps propsandstate
             _ <- forkIO $ do
                 case props of
                   MarkProps Nothing _ -> return ()
                   MarkProps (Just p) _ -> do
                     let actions = RS.dispatch . RS.BubblesAction $ RS.AddMarkPosition (p ^. markPropsDataChunkId) top 0 -- we assume that no scrolling has taken place yet
                     forM_ actions executeAction
             return ()
   }

rfMark_ :: MarkProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
rfMark_ = view rfMark

foreign import javascript unsafe
  "$1.getBoundingClientRect().top"
  js_getBoundingClientRectTop :: JSVal -> IO RS.OffsetFromViewportTop
