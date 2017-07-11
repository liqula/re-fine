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
  , LeafSelectorSide(..)
  , renderLeafSelectorSide
  ) where

import Refine.Frontend.Prelude

import           GHC.Generics (Generic)
import           GHCJS.Marshal (FromJSVal, ToJSVal)
import           GHCJS.Types (JSVal)

import           Refine.Frontend.Util ((===))
import           Refine.Prelude.Aeson (NoJSONRep(NoJSONRep))


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

makeRefineTypes [''EditorState, ''ContentState]

mkEditorState :: HasCallStack => JSVal -> EditorState
mkEditorState = EditorState . NoJSONRep


data LeafSelectorSide = LeafSelectorTop | LeafSelectorBottom
  deriving (Eq, Ord, Show, Generic)

renderLeafSelectorSide :: HasCallStack => LeafSelectorSide -> ST
renderLeafSelectorSide LeafSelectorTop    = "top"
renderLeafSelectorSide LeafSelectorBottom = "bottom"
