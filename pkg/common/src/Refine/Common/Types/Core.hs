{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}   -- FIXME: elim this
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}   -- pattern completeness checker has problems with pattern synonyms

-- | This module is huge because we have the following cyclic dependency:
--
--         data Edit
--     --> data EditSource
--     --> associated data type OT.EEdit RawContent
--     --> instance OT.Edit RawContent
--     --> data RawConent, and lots of conversion functions
--     --> data ContributionID
--     --> data Edit
module Refine.Common.Types.Core
    ( module Refine.Common.Types.Core
    , NonEmptyST(..)
    ) where

import Refine.Common.Prelude

import           Control.DeepSeq
import           Data.Int
import           Data.Foldable (toList)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap as IntMap
import           Data.Either (isLeft)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import           Data.String.Conversions (ST, cs, (<>))
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)
import           Text.Read (readEither)
import           Web.HttpApiData (toUrlPiece, parseUrlPiece, ToHttpApiData(..), FromHttpApiData(..))

import qualified Refine.Common.OT as OT
import           Refine.Common.OT hiding (Edit)
import           Refine.Common.Types.Chunk
import           Refine.Common.Types.Comment
import           Refine.Common.Types.Prelude
import           Refine.Prelude.TH (makeRefineType)


-- * VDoc

data VDoc = VDoc
  { _vdocMetaID   :: MetaID VDoc
  , _vdocTitle    :: Title
  , _vdocAbstract :: Abstract
  , _vdocHeadEdit :: ID Edit
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | the name clashes in the record selectors are really annoying...
-- makes me understand why people were so fond of OO when they invented it
data CreateVDoc = CreateVDoc
  { _createVDocTitle       :: Title
  , _createVDocAbstract    :: Abstract
  , _createVDocInitVersion :: VDocVersion
  }
  deriving (Eq, Ord, Show, Generic)

newtype Title = Title { _unTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Abstract = Abstract { _unAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype VDocVersion = VDocVersion { _unVDocVersion :: ST }
  deriving (Eq, Ord, Show, Generic, Monoid)

data EditSource a =
    InitialEdit
  | EditOfEdit (OT.Edit RawContent) a
  | MergeOfEdits a a
  deriving (Eq, Show, Generic, Functor)

data Edit = Edit
  { _editMetaID :: MetaID Edit
  , _editDesc   :: ST
  , _editRanges :: NonEmpty SelectionState
  , _editKind   :: EditKind
  , _editSource :: EditSource (ID Edit)
  }
  deriving (Eq, Show, Generic)

data CreateEdit = CreateEdit
  { _createEditDesc  :: ST
  , _createEditVDoc  :: VDocVersion
  , _createEditKind  :: EditKind
  }
  deriving (Eq, Ord, Show, Generic)

data EditKind = Grammar | Phrasing | Meaning | Initial
  deriving (Eq, Ord, Show, Read, Generic)


-- ** create types, instances

type instance Create VDoc  = CreateVDoc
type instance Create Edit = CreateEdit


-- ** composites

-- | Packaged vdoc ready for use by client.
--
-- - morally we have three phases in working on a document: (1) add comments and edits, (2) merge a
--   bunch of edits and (3) create a new version.
--
-- - what follows from this:
--     - there are no edits on edits that we need to display
--     - it's ok to only display edits on head, not on any other version
--     - same for comments: comments collect on head, then then are discarded in (2), (3).
--
-- - if we try to consider comments, edits, ... on other versions than head, we are in trouble.
data CompositeVDoc = CompositeVDoc
  { _compositeVDoc            :: VDoc
  , _compositeVDocEditID      :: ID Edit
  , _compositeVDocVersion     :: VDocVersion
  , _compositeVDocEdits       :: Map (ID Edit) Edit
  , _compositeVDocNotes       :: Map (ID Note) Note
  -- , _compositeVDocQuestions   :: [Question]  -- will be due in #99
  , _compositeVDocDiscussions :: Map (ID Discussion) CompositeDiscussion
  }
  deriving (Eq, Show, Generic)


-- * Contribution

-- | Type to define terminology (it's ok if it is not used anywhere else in the code).  This is just a
-- list of everything that is deemed a 'Contribution'.
--
-- Note: first edit to a document is also a contribution.  As the initial version of the document it
-- may be a special thing in the UI metaphor, but it is not in the backend types: here there is just
-- an edit on the empty document.
data Contribution =
    ContribNote Note
  | ContribQuestion Question
  | ContribDiscussion Discussion
  | ContribEdit Edit
  | ContribHighlightMark ChunkRange
  deriving (Eq, Show, Generic)

-- | For places where we need heterogeneous lists of different 'ID's, we can use this type.
--
-- | FUTUREWORK: It would be nice to just use @ID Contribution@ instead of 'ContributionID', but
-- that changes the case switch implementation, and I'm not sure right now if it'll still be as
-- straight-forward.
data ContributionID =
    ContribIDNote (ID Note)
  | ContribIDQuestion (ID Question)
  | ContribIDDiscussion (ID Discussion)
  | ContribIDEdit (ID Edit)
  | ContribIDHighlightMark  -- ^ current selection (FUTUREWORK: rename to
                            -- 'ContribIDCurrentSelection'; or better yet remove from this type
                            -- altogether).
  deriving (Eq, Ord, Show, Read, Generic)

-- | In the frontend, for replacing the browser selection range with a mark when an editor overlay
-- opens, we need a 'Void'-like contribution kind that cannot have a contribution value.
data HighlightMark


-- * RawContent

-- | Haskell representation of the javascript @RawDraftContentState@.
-- https://draftjs.org/docs/api-reference-data-conversion.html#content
--
-- FIXME: make this type abstract.  if we construct a value here instead of by calling
-- 'mkRawContent', it may be non-canonical, and that may lead to strange artifacts in the diff.
data RawContent = RawContent
  { _rawContentBlocks    :: NonEmpty (Block EntityKey)
  , _rawContentEntityMap :: IntMap Entity  -- ^ for performance, do not use @Map EntityKey Entity@ here.
  }
  deriving (Eq, Show, Generic)

-- | typical rangekey values are 'Int' and 'Entity'
data Block rangeKey = Block
  { _blockText         :: ST
  , _blockEntityRanges :: [(rangeKey, EntityRange)]  -- ^ entity ranges for entities must not overlap!
  , _blockStyles       :: [(EntityRange, Style)]     -- ^ entity ranges for styles are allowed to overlap.
  , _blockType         :: BlockType
  , _blockDepth        :: Int
  , _blockKey          :: Maybe BlockKey  -- ^ FIXME: many function rely on this being defined.
                                          -- make this an index and either store '()' or 'BlockKey'.
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

-- | `key` attribute of the 'Block'.  'SelectionState' uses this to refer to blocks.  If in doubt
-- leave it 'Nothing'.
newtype BlockKey = BlockKey ST
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic)

-- | key into 'rawContentEntityMap'.
newtype EntityKey = EntityKey { _unEntityKey :: Int }
  deriving (Eq, Ord, Show, ToJSON, FromJSON, Generic)

type EntityRange = (Int, Int)

-- | an entity's range may span across multiple blocks
newtype Entity =
    EntityLink ST  -- ^ url
--  | ...
  deriving (Show, Eq, Ord, Generic)

-- | a style's range should fit into a single block.
--
-- NOTE: 'Mark' could be an entity if it weren't for the fact that we need overlaps (see
-- https://github.com/facebook/draft-js/issues/212).
data Style =
    Bold
  | Italic
  | Underline
  | Code
    -- custom styles
  | Mark ContributionID
    -- styles for visual diff
  | StyleAdded
  | StyleDeleted
  | StyleChanged
  deriving (Show, Eq, Ord, Generic)

-- | each block has a unique blocktype
data BlockType =
    NormalText
  | Header1
  | Header2
  | Header3
  | BulletPoint
  | EnumPoint
  deriving (Show, Eq, Generic, Bounded, Enum)


-- | https://draftjs.org/docs/api-reference-selection-state.html
data SelectionState
  = SelectionState
      { _selectionIsBackward :: Bool
      , _selectionStart      :: SelectionPoint
      , _selectionEnd        :: SelectionPoint
      }
  deriving (Eq, Ord, Show, Generic)

data SelectionPoint
  = SelectionPoint
      { _selectionBlock  :: BlockKey
      , _selectionOffset :: Int
      }
  deriving (Eq, Ord, Show, Generic)


-- | Javascript: `document.querySelectorAll('article span[data-offset-key="2vutk-0-1"]');`.  The
-- offset-key is constructed from block key, a '0' literal, and the number of left siblings of the
-- span the selector refers to.
data MarkSelector = MarkSelector MarkSelectorSide BlockKey Int
  deriving (Eq, Show, Generic)

data MarkSelectorSide = MarkSelectorTop | MarkSelectorBottom | MarkSelectorUnknownSide
  deriving (Eq, Show, Generic)


-- * OT.Edit RawContent

type OTDoc = NonEmpty DocBlock

-- | (third constructor arg is depth)
type DocBlock = (((Atom BlockType, Atom BlockDepth), NonEditable (Maybe BlockKey)), LineElems)

pattern DocBlock :: BlockType -> BlockDepth -> Maybe BlockKey -> [LineElem] -> DocBlock
pattern DocBlock t d k ls = (((Atom t, Atom d), NonEditable k), Segments ls)

-- | An integer between 0 and 36
-- (ok, the upper bound of 36 for depth is arbitrarily introduced here.  don't know about draft.)
newtype BlockDepth = BlockDepth {unBlockDepth :: Int}
  deriving (Eq, Ord, Show, Read, ToJSON, FromJSON, Generic, NFData)

type LineElems = Segments EntityStyles NonEmptyST
type LineElem = (EntityStyles, NonEmptyST)

type EntityStyles = (Atom (Maybe Entity), Set (Atom Style))

-- | TUNING: (Set.mapMonotonic unAtom) and (Set.mapMonotonic Atom) should be (coerce)
--   but GHC can't see that (Ord (Atom a)) is the same as (Ord a)
pattern LineElem :: Set (Either Entity Style) -> NonEmptyST -> LineElem
pattern LineElem a b <- (makeEntityStyleSet -> a, b)
  where LineElem a b = ((Atom (mb $ Set.toList s1), Set.mapMonotonic (\(Right sty) -> Atom sty) s2), b)
          where
            (s1, s2) = Set.partition isLeft a
            mb [] = Nothing
            mb [Left x] = Just x
            mb _ = error "overlapping entities"

makeEntityStyleSet :: EntityStyles -> Set (Either Entity Style)
makeEntityStyleSet (Atom e, s) = maybe mempty (Set.singleton . Left) e <> Set.mapMonotonic (Right . unAtom) s

-- * Instances

-- ** Contribution Http instances

instance ToHttpApiData ContributionID where
  toUrlPiece (ContribIDNote (ID i))       = "n" <> cs (show i)
  toUrlPiece (ContribIDQuestion (ID i))   = "q" <> cs (show i)
  toUrlPiece (ContribIDDiscussion (ID i)) = "d" <> cs (show i)
  toUrlPiece (ContribIDEdit (ID i))       = "e" <> cs (show i)
  toUrlPiece ContribIDHighlightMark       = "h"

instance FromHttpApiData ContributionID where
  parseUrlPiece piece = case ST.splitAt 1 piece of
    (ks, is) -> do
      let i = either (Left . cs) Right . (readEither :: String -> Either String Int64) . cs $ is
      case ks of
        "n" -> ContribIDNote . ID <$> i
        "q" -> ContribIDQuestion . ID <$> i
        "d" -> ContribIDDiscussion . ID <$> i
        "e" -> ContribIDEdit . ID <$> i
        "h" -> pure ContribIDHighlightMark
        bad -> Left . cs $ "FromHttpApiData ContributionID: no parse: " <> show bad


-- ** RawContent JSON instances

instance ToJSON RawContent where
  toJSON (RawContent blocks entitymap) = object
    [ "blocks"    .:= blocks
    , "entityMap" .:= renderEntityMap entitymap
    ]
    where
      renderEntityMap m = object [ cs (show a) .:= b | (a, b) <- IntMap.toList m ]

instance FromJSON RawContent where
  parseJSON = withObject "RawContent" $ \obj -> RawContent
    <$> obj .: "blocks"
    <*> (parseEntityMap =<< obj .: "entityMap")
    where
      parseEntityMap = withObject "parseEntityMap" $ foldM f mempty . HashMap.toList
        where
          f :: IntMap Entity -> (ST, Value) -> Parser (IntMap Entity)
          f m (read . cs -> k, v) = (\e -> IntMap.insert k e m) <$> parseJSON v

instance ToJSON (Block EntityKey) where
  toJSON (Block content ranges styles ty depth key) = object $
    [ "text"              .:= content
    , "entityRanges"      .:= (renderRange <$> ranges)
    , "inlineStyleRanges" .:= (renderStyle <$> styles)
    , "depth"             .:= depth  -- ^ (if certain BlockType values force this field to be 0, move this field there.)
    , "type"              .:= ty
    ] <>
    [ "key" .:= k | k <- maybeToList key ]
    where
      renderRange (k, (o, l)) = object ["key"   .:= k, "length" .:= l, "offset" .:= o]
      renderStyle ((o, l), s) = object ["style" .:= s, "length" .:= l, "offset" .:= o]

instance FromJSON (Block EntityKey) where
  parseJSON = withObject "Block EntityKey" $ \obj -> Block
    <$> obj .: "text"
    <*> (mapM parseRange =<< (obj .: "entityRanges"))
    <*> (mapM parseStyle =<< (obj .: "inlineStyleRanges"))
    <*> obj .: "type"
    <*> (round <$> (obj .: "depth" :: Parser Double))
    <*> obj .:? "key"
    where
      parseRange = withObject "Block EntityKey: entityRanges" $ \obj -> do
        k <- obj .: "key"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure (k, (o, l))
      parseStyle = withObject "Block EntityKey: inlineStyleRanges" $ \obj -> do
        s <- obj .: "style"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure ((o, l), s)

instance ToJSON BlockType where
  toJSON NormalText  = "unstyled"
  toJSON Header1     = "header-one"
  toJSON Header2     = "header-two"
  toJSON Header3     = "header-three"
  toJSON BulletPoint = "unordered-list-item"
  toJSON EnumPoint   = "ordered-list-item"

instance FromJSON BlockType where
  parseJSON (String "unstyled")            = pure NormalText
  parseJSON (String "header-one")          = pure Header1
  parseJSON (String "header-two")          = pure Header2
  parseJSON (String "header-three")        = pure Header3
  parseJSON (String "unordered-list-item") = pure BulletPoint
  parseJSON (String "ordered-list-item")   = pure EnumPoint
  parseJSON bad = fail $ "BlockType: no parse for " <> show bad

instance ToJSON Entity where
  toJSON (EntityLink url) = object
    [ "type"            .:= ("LINK" :: ST)
    , "mutability"      .:= ("MUTABLE" :: ST)
    , "data"            .:= object ["url" .:= url]
    ]

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \obj -> do
    ty :: ST <- obj .: "type"
    case ty of
      "LINK" -> let parseData = withObject "LINK data" (.: "url")
                in EntityLink <$> (parseData =<< obj .: "data")
      bad -> fail $ "Entity: no parse for " <> show bad

instance ToJSON Style where
  toJSON Bold         = "BOLD"
  toJSON Italic       = "ITALIC"
  toJSON Underline    = "UNDERLINE"
  toJSON Code         = "CODE"
  toJSON (Mark cid)   = String $ "MARK__" <> toUrlPiece cid
  toJSON StyleAdded   = "ADDED"
  toJSON StyleDeleted = "DELETED"
  toJSON StyleChanged = "CHANGED"

instance FromJSON Style where
  parseJSON (String "BOLD")                 = pure Bold
  parseJSON (String "ITALIC")               = pure Italic
  parseJSON (String "UNDERLINE")            = pure Underline
  parseJSON (String "CODE")                 = pure Code
  parseJSON (String (ST.splitAt 6 -> ("MARK__", parseUrlPiece -> Right cid)))
                                            = pure $ Mark cid
  parseJSON (String "ADDED")                = pure StyleAdded
  parseJSON (String "DELETED")              = pure StyleDeleted
  parseJSON (String "CHANGED")              = pure StyleChanged
  parseJSON bad = fail $ "Style: no parse for " <> show bad


-- ** Editable RawContent instance

-- | assumption: neighbouring elements have different entities or styles.
instance Editable RawContent where
    newtype EEdit RawContent
        {-
        Each elementary edit on RawContent should keep the invariant that neighbouring line elems
        has different styles/entity.
        To achieve this, an elementary edit on RawContent is a list of elementary edits on OTDoc.

        Example:
            Document: "*bold* normal"
            Edit: ERawConent [<change style of line elem #1 to normal>, <join line elems #1 and #2>]
        Without the join, the invariant would not hold.
        -}
        = ERawContent {unERawContent :: OT.Edit OTDoc}
      deriving (Generic, Show, Eq)

    docCost = docCost . rawContentToDoc
    eCost = cost . unERawContent

    -- FUTUREWORK: smarter diff producing smaller elementary patches
    diff a b = eRawContent <$> diff (rawContentToDoc a) (rawContentToDoc b)

    ePatch e = docToRawContent . patch (coerce e) . rawContentToDoc
    patch e = docToRawContent . patch (concat (coerce e :: [OT.Edit OTDoc])) . rawContentToDoc

    {-
            d
        e1 / \ e2
          d1 d2
           \ /
            d'
            | makeJoinEdits
            d''
    -}
    eMerge d a b = coerce ([a' <> e], [b' <> e])
      where
        doc = rawContentToDoc d
        e = makeJoinEdits $ patch (coerce a <> a') doc
        (a', b') = merge doc (coerce a) (coerce b)

    -- TUNING: implement better-than-default merge
    -- merge d a b = coerce $ merge (rawContentToDoc d) (coerce a) (coerce b)

    eInverse d = pure . coerce . inverse (rawContentToDoc d) . coerce
    inverse d es = coerce $ f [] (rawContentToDoc d) (coerce es)
      where
        f acc _ [] = acc
        f acc doc (x: xs) = f (inverse doc x: acc) (patch x doc) xs

eRawContent :: OT.Edit OTDoc -> OT.Edit RawContent
eRawContent [] = []
eRawContent e  = [ERawContent e]

instance ToJSON (EEdit RawContent)
instance FromJSON (EEdit RawContent)
instance SOP.Generic (EEdit RawContent)
instance SOP.HasDatatypeInfo (EEdit RawContent)


-- ** helper functions for Editable RawContent instance

-- | @'docToRawContent' . 'rawContentToDoc' == id@ iff 'RawContent' is canonicalized (see
-- 'canonicalizeRawContent').
rawContentToDoc :: RawContent -> OTDoc
rawContentToDoc (RawContent blocks entities) = mkDocBlock <$> blocks
  where
    mkDocBlock :: Block EntityKey -> DocBlock
    mkDocBlock (Block txt eranges styles ty depth key) = DocBlock ty (BlockDepth depth) key (segment segments txt)
      where
        segment [] "" = []
        segment [] text = [LineElem mempty $ NonEmptyST text]
        segment ((len, s): ss) text
            | len > 0 = LineElem s (NonEmptyST $ ST.take len text) `add` segment ss (ST.drop len text)
            | otherwise = error "segment text length is 0"

        e `add` [] = [e]
        e@(at1, x) `add` ls@((at2, y): ls')
            | at1 == at2 = (at1, x <> y): ls'
            | otherwise  = e: ls

        segments :: [(Int, Set (Either Entity Style))]
        segments = mkSomeSegments fst snd
                 $ (second Right <$> styles)
                <> ((\(e, r) -> (r, Left (entities IntMap.! coerce e))) <$> eranges)

docToRawContent :: OTDoc -> RawContent
docToRawContent blocks = mkRawContentInternal $ mkDocBlock <$> blocks
  where
    getText (LineElem _ (NonEmptyST txt)) = txt

    mkDocBlock :: DocBlock -> Block Entity
    mkDocBlock (DocBlock ty (BlockDepth d) key es) = Block
        (mconcat $ fmap getText es)
        [(e, r) | (r, Left e) <- ranges]
        [(r, s) | (r, Right s) <- ranges]
        ty
        d
        key
      where
        ranges = mkRanges 0 mempty
            $ [(len, s) | LineElem s (NonEmptyST txt) <- es, let len = ST.length txt]
            <> [(0, mempty)]  -- this is to avoid one more case in mkRanges below when we're done.

        mkRanges _ _ [] = []
        mkRanges n acc ((len, s): ss)
            = -- construct all non-empty ranges that are closed in s
              [((offset, l), sty) | (offset, sty) <- acc, sty `Set.notMember` s, let l = n - offset, l > 0]
           <> -- jump to the next segment (aka line element)
              mkRanges (n + len)
                        (  [(offset, sty) | (offset, sty) <- acc, sty `Set.member` s]
                        <> [(n, sty) | sty <- Set.elems s, sty `notElem` fmap snd acc])
                        ss

-- | Block canonicalization: merge neighboring line elems with same attr set.
makeJoinEdits :: OTDoc -> OT.Edit OTDoc
makeJoinEdits blocks = concat . zipWith simplifyBlock [0..] $ NEL.toList blocks
  where
    simplifyBlock :: Int -> DocBlock -> OT.Edit OTDoc
    simplifyBlock i (DocBlock _ _ _ ls) = map ENonEmpty . editItem i . editSecond $ case ls of
        []  -> []
        [_] -> []
        (es, _): xs -> go 0 es xs

    go _ _ [] = []
    go i p ((es, _): ls)
        | p == es = JoinItems i: go i es ls
        | otherwise = go (i+1) es ls


-- | Canonicalize a 'RawContent' value.  A value is canonicalized if no two entities that have the
-- same 'Entity' value touch.
--
-- TUNING: this is the trivial implementation; there is probably something more efficient.
canonicalizeRawContent :: RawContent -> RawContent
canonicalizeRawContent = docToRawContent . rawContentToDoc

-- | Construct a 'RawContent' value and canonicalize it.
mkRawContent :: NonEmpty (Block Entity) -> RawContent
mkRawContent = canonicalizeRawContent . mkRawContentInternal

-- | Construct a 'RawContent' value *without* canonicalizing it.
--
-- Note: empty block list is illegal.  For once it will make draft crash in 'stateFromContent'.
mkRawContentInternal :: NonEmpty (Block Entity) -> RawContent
mkRawContentInternal bsl = RawContent (index <$$> bsl) (IntMap.fromList entities)
  where
    -- FUTUREWORK: it is possible to do just one traversal to collect and index entities
    -- https://www.reddit.com/r/haskell/comments/610sa1/applicative_sorting/
    entities :: [(Int, Entity)]
    entities = zip [0..] . nub $ concatMap toList bsl

    index :: Entity -> EntityKey
    index e = EntityKey . fromMaybe (error "mkRawContentInternal: impossible") $ Map.lookup e em

    em = Map.fromList $ (\(a, b) -> (b, a)) <$> entities

mkBlock :: ST -> Block rangeKey
mkBlock t = Block t [] [] NormalText 0 Nothing

emptyBlock :: Block rangeKey
emptyBlock = mkBlock mempty

-- | Take two accessors and a list of things carrying a range and a payload each.  Ranges can
-- overlap.  Compute a list of non-overlapping segments, starting at @offset == 0@, consisting of
-- length and the set of all payloads active in this segment.
--
-- NOTE: since this function does have access to the block length, the last segment will be
-- contained in the output *iff* the end of some range coincides with the end of the block.  (ranges
-- must not point beyong the end of the block.)
mkSomeSegments :: (Ord payload, Show payload)
               => (el -> EntityRange) -> (el -> payload) -> [el] -> [(Int, Set payload)]
mkSomeSegments frange fpayload els = segments
  where
    segments =
        mkSegments 0 []
      . fmap (\((offset, len), s) -> (offset, (offset + len, s)))
      . sortBy (compare `on` fst)
      $ [(frange el, fpayload el) | el <- els]

    -- FIXME: stack (2nd arg) should be @IntMap (Set style)@ (keyed by @offset@).
    mkSegments _ [] [] = []  -- (stack will be emptied in the third case.)
    mkSegments n stack ((offset, s): ss) | offset == n = mkSegments n (insertBy (compare `on` fst) s stack) ss
    mkSegments n ((offset, _): stack) ss | offset == n = mkSegments n stack ss
    mkSegments n stack ss                | offset > n  = (offset - n, Set.fromList $ snd <$> stack): mkSegments offset stack ss
      where
        offset = case (fst <$> stack, fst <$> ss) of
            (a: _, b: _) -> min a b
            (a: _, [])   -> a
            ([],   b: _) -> b
            ([],   [])   -> error "impossible"
    mkSegments n stack ss = error $ "impossible: " <> show (n, stack, ss)


-- * Derived instances

makeRefineType ''CompositeVDoc
makeRefineType ''ContributionID
makeRefineType ''SelectionState
makeRefineType ''SelectionPoint
makeRefineType ''VDoc
makeRefineType ''CreateVDoc
makeRefineType ''EditSource
makeRefineType ''Edit
makeRefineType ''CreateEdit
makeRefineType ''EditKind
makeRefineType ''Title
makeRefineType ''Abstract
makeRefineType ''VDocVersion

makeLenses ''RawContent
makeLenses ''Block
makeLenses ''BlockKey
makeLenses ''EntityKey
makeLenses ''Entity
makeLenses ''Style
makeLenses ''BlockType

makeSOPGeneric ''RawContent
makeSOPGeneric ''Block
makeSOPGeneric ''BlockKey
makeSOPGeneric ''EntityKey
makeSOPGeneric ''Entity
makeSOPGeneric ''Style
makeSOPGeneric ''BlockType

makeNFData ''RawContent
makeNFData ''Block
makeNFData ''BlockKey
makeNFData ''EntityKey
makeNFData ''Entity
makeNFData ''Style
makeNFData ''BlockType

-- | It cannot moved further up, needs instance NFData BlockType
deriving instance NFData (EEdit RawContent)


-- * helper lenses

vdocID :: Lens' VDoc (ID VDoc)
vdocID = vdocMetaID . miID

editID :: Lens' Edit (ID Edit)
editID = editMetaID . miID
