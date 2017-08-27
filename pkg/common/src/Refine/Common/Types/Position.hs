{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- pattern completeness checker has problems with pattern synonyms

module Refine.Common.Types.Position where

#include "import_common.hs"

import           Data.Int
import           GHC.Generics (Generic)


-- * Position

-- | Generic position
data GPosition blockIndex columnIndex = Position
    { _blockIndex  :: blockIndex
    , _columnIndex :: columnIndex
    }
    deriving (Eq, Ord, Show, Read, Generic, Functor)

-- this is called `first` at https://hackage.haskell.org/package/bifunctors-3.2.0.1/docs/Data-Bifunctor.html
mapBlockIndex :: (a -> b) -> GPosition a c -> GPosition b c
mapBlockIndex f (Position a c) = Position (f a) c

-- ** Specific Positions

-- The preferred position to use
type Position = GPosition BlockIndex Int

-- | `key` attribute of the 'Block'.  'SelectionState' uses this to refer to blocks.  If in doubt
-- leave it 'Nothing'.
newtype BlockKey = BlockKey { _unBlockKey :: ST }
  deriving (Eq, Ord, Show, Generic, Monoid)

-- | Semantically this is an Int, with a block key cached
data BlockIndex = BlockIndex
    { _blockIndexIndex :: Int
    , _blockIndexKey   :: BlockKey  -- ^ cached block key
    }
    deriving (Show, Generic)

instance Eq BlockIndex where (==) = (==) `on` _blockIndexIndex
instance Ord BlockIndex where compare = compare `on` _blockIndexIndex

-- | use only when interfacing with Draft; use Position otherwise
type SelectionPoint = GPosition BlockKey Int

toSelectionPoint :: Position -> SelectionPoint
toSelectionPoint = mapBlockIndex _blockIndexKey


-- * StylePosition

-- style positions are positions between stylable elements (stylable characters)
-- so two style positions are equal iff their positions are separated by unstylable characters only
--
-- currently all characters but '\n' are stylable
data StylePosition = StylePosition
    { spBasePosition   :: Position
    , spLengthMinusOne :: Int
    }
    deriving (Eq, Ord, Show, Generic)

-- base position of x is the minimal postion whose corresponding style position is x
basePosition :: StylePosition -> Position
basePosition (StylePosition p _) = p


-- * Range

-- | Invariant: rangeBegin <= rangeEnd
-- the consturctor is called 'Inner' because it should be hidden (module inner use allowed only)
-- FIXME: hide the RangeInner consturctor & Functor instance
data Range a = RangeInner
    { _rangeBegin   :: a
    , _rangeEnd     :: a
    }
    deriving (Eq, Ord, Show, Read, Generic, Functor)

pattern Range :: (Ord t) => t -> t -> Range t
pattern Range a b <- RangeInner a b
  where Range a b = RangeInner (min a b) (max a b)


emptyRange :: a -> Range a
emptyRange a = RangeInner a a

isEmptyRange :: (Eq a, Ord a) => Range a -> Bool
isEmptyRange (Range a b) = a == b

doRangesOverlap :: Ord a => Range a -> Range a -> Bool
doRangesOverlap (Range b1 e1) (Range b2 e2) = not $ e2 <= b1 || e1 <= b2

type EntityRange = Range Int

-- FIXME: eliminate this
pattern EntityRange :: (Num t, Ord t) => t -> t -> Range t
pattern EntityRange a b <- (mkRangeDiff -> (a, b))
  where EntityRange a b = RangeInner a (a + b)

-- FIXME: eliminate this
mkRangeDiff :: (Num t, Ord t) => Range t -> (t, t)
mkRangeDiff (Range a b) = (a, b - a)


-- * Selection

-- | A selection is a directed range
data Selection a = Selection
    { _selectionIsBackward :: Bool
    , _selectionRange      :: Range a
    }
    deriving (Eq, Ord, Show, Generic, Functor)

toSelection :: Range a -> Selection a
toSelection = Selection False

toBackwardSelection :: Range a -> Selection a
toBackwardSelection = Selection True

-- | https://draftjs.org/docs/api-reference-selection-state.html
-- use only when interfacing with Draft
--
-- The JSON representation uses anchor, focus rather than begin, end so the json value can be used
-- directly in 'forceSelection'.
data SelectionState = SelectionState
  { _unSelectionState :: Selection SelectionPoint
  , _selectionStateHasFocus :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

toSelectionState :: Selection Position -> Bool -> SelectionState
toSelectionState = SelectionState . fmap toSelectionPoint

instance ToJSON SelectionState where
  toJSON (SelectionState (Selection back (Range (Position (BlockKey k) c) (Position (BlockKey k') c'))) focus) = object
    [ "anchorKey"         .:= if back then k' else k
    , "anchorOffset"      .:= if back then c' else c
    , "focusKey"          .:= if back then k else k'
    , "focusOffset"       .:= if back then c else c'
    , "hasFocus"          .:= focus
    , "isBackward"        .:= back
    ]

instance FromJSON SelectionState where
  parseJSON = withObject "SelectionState" $ \obj -> do
    back   <- obj .: "isBackward"
    anchor <- Position
              <$> (BlockKey <$> (obj .: "anchorKey"))
              <*> (obj .: "anchorOffset")
    focus  <- Position
              <$> (BlockKey <$> (obj .: "focusKey"))
              <*> (obj .: "focusOffset")
    hasFoc <- obj .: "hasFocus"
    pure $ SelectionState
      (Selection back (if back
                        then Range focus anchor
                        else Range anchor focus))
      hasFoc


-- * Ranges

-- | Invariant: the end points of ranges form an increasing sequence and there are no touching /
-- overlapping ranges.  the constuctor is called 'Inner' because it should be hidden (module inner
-- use allowed only).
--
-- FIXME: do not export the RangesInner constructor.
newtype Ranges a = RangesInner {unRanges :: [Range a]}
    deriving (Eq, Ord, Show, Generic)

rangesFromRange :: Ord a => Bool{-Allow empty ranges-} -> Range a -> Ranges a
rangesFromRange allowEmptyRanges r
    | not allowEmptyRanges && isEmptyRange r = mempty
    | otherwise = RangesInner [r]

-- | Semantics: union of ranges
instance Ord a => Monoid (Ranges a) where

    as_ `mappend` bs_ = RangesInner $ f (unRanges as_) (unRanges bs_)
      where
        f [] bs = bs
        f as [] = as
        f (a@(Range x1 y1): as) (b@(Range x2 y2): bs)
            | y1 < x2   = a: f as (b: bs)
            | y2 < x1   = b: f (a: as) bs
            | y2 < y1   = f (RangeInner (min x1 x2) y1: as) bs
            | otherwise = f as (RangeInner (min x1 x2) y2: bs)

    mempty = RangesInner []

-- | deletes empty ranges
-- FUTUREWORK: parameterize this or make a variant which keeps empty ranges if necessary
intersectionRanges :: Ord a => Ranges a -> Ranges a -> Ranges a
intersectionRanges as_ bs_ = RangesInner $ f (unRanges as_) (unRanges bs_)
  where
    f [] _ = []
    f _ [] = []
    f (a@(Range x1 y1): as) (b@(Range x2 y2): bs)
        | y1 <= x2  = f as (b: bs)
        | y2 <= x1  = f (a: as) bs
        | y1 < y2   = f (RangeInner (max x1 x2) y1: as) bs
        | otherwise = f as (RangeInner (max x1 x2) y2: bs)

-- | The smallest range which contains all ranges in input
rangesClosure :: Ranges a -> Maybe (Range a)
rangesClosure rs = case unRanges rs of
    [] -> Nothing
    xs -> Just $ RangeInner (_rangeBegin $ head xs) (_rangeEnd $ last xs)


-- | Use only when interfacing with Draft
--
-- Javascript: `document.querySelectorAll('article span[data-offset-key="2vutk-0-1"]');`.  The
-- offset-key is constructed from block key and two 'Int's.
--
-- The first 'Int' increments every time a decorator is encountered; the second one increments on
-- every span and resets on every decorator.  'getEntitySelectors', 'getLeafSelectors',
-- 'getSelectors' depend on this behavior.  See the resp. tests in pkg/frontend.
--
-- See also: node_modules/draft-js/lib/DraftOffsetKey.js,
-- node_modules/draft-js/lib/DraftEditorContents.react.js
type LeafSelector = GPosition BlockKey SpanIndex

-- | See 'LeafSelector'.
data SpanIndex = SpanIndex
    { decoratorIndex :: Int
    , leafIndex      :: Int
    }
  deriving (Eq, Ord, Show, Generic)

-- | FUTUREWORK: @article span[...@ didn't work.
renderLeafSelector :: LeafSelector -> ST
renderLeafSelector (Position (BlockKey b) (SpanIndex k i)) =
  "span[data-offset-key=\"" <> b <> "-" <> cs (show k) <> "-" <> cs (show i) <> "\"]"


deriveClasses [ ([''GPosition, ''Range, ''Ranges, ''Selection, ''BlockIndex, ''BlockKey], allClass)
              , ([''SelectionState], [''Lens', ''SOP.Generic])
              ]

-- counts from zero
rowIndex :: Lens' Position Int
rowIndex = blockIndex . blockIndexIndex
