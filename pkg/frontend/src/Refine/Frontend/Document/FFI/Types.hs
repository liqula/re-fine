{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Document.FFI.Types
  ( EditorState(..)
  , ContentState(..)
  , mkEditorState
  , LeafSelectorSide(..)
  , renderLeafSelectorSide
  ) where
#include "import_frontend.hs"

import           GHCJS.Marshal (FromJSVal, ToJSVal)
import           GHCJS.Types (JSVal)
import           System.IO.Unsafe (unsafePerformIO)

import           Refine.Frontend.Util ((===))


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

instance PFromJSVal EditorState where pFromJSVal = fromJust . unsafePerformIO . fromJSVal
instance PToJSVal EditorState where pToJSVal = unsafePerformIO . toJSVal

instance Eq EditorState where
  EditorState (NoJSONRep js) == EditorState (NoJSONRep js') = js === js'

newtype ContentState = ContentState (NoJSONRep JSVal)
  deriving (Show, Generic, ToJSVal, FromJSVal)

instance PFromJSVal ContentState where pFromJSVal = fromJust . unsafePerformIO . fromJSVal
instance PToJSVal ContentState where pToJSVal = unsafePerformIO . toJSVal

makeRefineTypes [''EditorState, ''ContentState]

mkEditorState :: HasCallStack => JSVal -> EditorState
mkEditorState = EditorState . NoJSONRep


data LeafSelectorSide = LeafSelectorTop | LeafSelectorBottom
  deriving (Eq, Ord, Show, Generic)

renderLeafSelectorSide :: HasCallStack => LeafSelectorSide -> ST
renderLeafSelectorSide LeafSelectorTop    = "top"
renderLeafSelectorSide LeafSelectorBottom = "bottom"
