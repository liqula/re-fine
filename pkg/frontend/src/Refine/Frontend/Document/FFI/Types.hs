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
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Document.FFI.Types
  ( EditorState(..)
  , ContentState(..)
  , mkEditorState
  , updateEditorState
  ) where

import Refine.Frontend.Prelude

import           GHC.Generics (Generic)
import           GHCJS.Marshal (FromJSVal, ToJSVal)
import           GHCJS.Types (JSVal)
import           React.Flux (Event, evtHandlerArg)
import           React.Flux.Internal (HandlerArg(HandlerArg))

import           Refine.Frontend.Util ((===))
import           Refine.Prelude.Aeson (NoJSONRep(NoJSONRep))
import           Refine.Prelude.TH (makeRefineType)


-- | Javascript representation of editor state.  It looks something like this:
--
-- >>> EditorState: Record
-- >>>   _immutable: Record
-- >>>     _map: Map
-- >>>       allowUndo: true
-- >>>       currentContent: ContentState
-- >>>         ...
-- >>>       decorator: null
-- >>>       directionMap: OrderedMap
-- >>>         ...
-- >>>       forceSelection: false
-- >>>       inCompositionMode: false
-- >>>       inlineStyleOverride: null
-- >>>       lastChangeType: null
-- >>>       length: 13
-- >>>       nativelyRenderedContent: null
-- >>>       redoStack: Stack
-- >>>         ...
-- >>>       undoStack: Stack
-- >>>         ...
-- >>>       selection: SelectionState
-- >>>         _map: Map
-- >>>           anchorKey: "cgd0m"  // b._immutable._map.get('selection')._map.get('anchorKey')
-- >>>           anchorOffset: 0
-- >>>           focusKey: "cgd0m"
-- >>>           focusOffset: 0
-- >>>           hasFocus: false
-- >>>           isBackward: false
-- >>>           length: 6
-- >>>       treeMap: OrderedMap
-- >>>         ...
newtype EditorState = EditorState (NoJSONRep JSVal)
  deriving (Show, Generic, ToJSVal, FromJSVal)

instance Eq EditorState where
  EditorState (NoJSONRep js) == EditorState (NoJSONRep js') = js === js'

newtype ContentState = ContentState (NoJSONRep JSVal)
  deriving (Show, Generic, ToJSVal, FromJSVal)

makeRefineType ''EditorState
makeRefineType ''ContentState

mkEditorState :: JSVal -> EditorState
mkEditorState = EditorState . NoJSONRep

updateEditorState :: Event -> EditorState
updateEditorState (evtHandlerArg -> HandlerArg evt) = EditorState $ NoJSONRep evt
