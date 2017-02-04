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

module Refine.Frontend.Mark where

import           Control.Concurrent (forkIO)
import           Control.Lens (makeLenses, (^.))
import           Data.Int
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.String.Conversions
import           GHCJS.Types (JSVal)
import           React.Flux
import           React.Flux.Lifecycle
import qualified Text.HTML.Parser as HTMLP
import           Text.Read (readMaybe)

import qualified Refine.Frontend.Screen.Types as RS
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


data MarkProps = MarkProps
  { _markPropsDataChunkId :: Int64
  , _markPropsDataContentType :: String
  }

makeLenses ''MarkProps

toMarkProps :: [HTMLP.Attr] -> MarkProps
toMarkProps attrs = let maybeChunkId = readMaybe $ valueOf "data-chunk-id" attrs :: Maybe Int64
  in case maybeChunkId of
    Nothing -> MarkProps (-1) ""
    Just chunkId -> MarkProps chunkId (valueOf "data-chunk-kind" attrs)
  where
    valueOf :: String -> [HTMLP.Attr] -> String
    valueOf _ [] = ""
    valueOf wantedKey (HTMLP.Attr key value:_) | key == cs wantedKey = cs value
    valueOf wantedKey (_:as) = valueOf wantedKey as


rfMark :: ReactView MarkProps
rfMark = defineLifecycleView "RefineMark" () lifecycleConfig
   { lRender = \_state props ->
         mark_ [ "data-chunk-id" $= fromString (show (props ^. markPropsDataChunkId))
               , "className" $= fromString ("o-mark o-mark--" <> props ^. markPropsDataContentType)
               ] childrenPassedToView

   , lComponentDidMount = Just $ \propsandstate ldom _ -> do
             this <- lThis ldom
             top <- js_getBoundingClientRectTop this
             props <- lGetProps propsandstate
             _ <- forkIO $ do
                 let actions = RS.dispatch $ RS.AddMarkPosition (props ^. markPropsDataChunkId) top 0 -- we assume that no scrolling has taken place yet
                 forM_ actions executeAction
             return ()
   }

rfMark_ :: MarkProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
rfMark_ = view rfMark

foreign import javascript unsafe
  "$1.getBoundingClientRect().top"
  js_getBoundingClientRectTop :: JSVal -> IO RS.OffsetFromViewportTop
