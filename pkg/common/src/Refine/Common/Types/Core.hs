{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-orphans             #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}  -- pattern completeness checker has problems with pattern synonyms

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
    , module Refine.Common.Types.Position
    , NonEmptyST(..)
    , Segments(..)
    , Atom(..)
    ) where

#include "import_common.hs"

import           Control.DeepSeq
import           Control.Lens (both)
import           Data.Char
import           Data.Either (isLeft)
import           Data.List (group)
import           Data.Tree
import           Text.Read (readEither)
import           Web.HttpApiData (toUrlPiece, parseUrlPiece, ToHttpApiData(..), FromHttpApiData(..))

import           Refine.Common.Types.Position
import           Refine.Common.Types.Vote
import qualified Refine.Common.OT as OT
import           Refine.Common.OT hiding (Edit)
import           Refine.Common.Types.Prelude


-- * VDoc

data VDoc = VDoc
  { _vdocMetaID   :: MetaID VDoc
  , _vdocTitle    :: Title
  , _vdocAbstract :: Abstract
  , _vdocHeadEdit :: ID Edit
  , _vdocGroup    :: ID Group
  , _vdocStats    :: EditStats
  , _vdocImage    :: Maybe ImageInline
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | the name clashes in the record selectors are really annoying...
-- makes me understand why people were so fond of OO when they invented it
data CreateVDoc = CreateVDoc
  { _createVDocTitle       :: Title
  , _createVDocAbstract    :: Abstract
  , _createVDocInitVersion :: RawContent
  , _createVDocGroup       :: ID Group
  , _createVDocImage       :: Maybe ImageInline
  }
  deriving (Eq, Show, Generic)

-- | the name clashes in the record selectors are really annoying...
-- makes me understand why people were so fond of OO when they invented it
data UpdateVDoc = UpdateVDoc
  { _updateVDocTitle       :: Title
  , _updateVDocAbstract    :: Abstract
  }
  deriving (Eq, Ord, Show, Generic)

newtype Title = Title { _unTitle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Abstract = Abstract { _unAbstract :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

-- | List of parents tupled with the diff
--
-- The type parameter is instantiated by 'Edit' or 'ID Edit'
--
-- Invariant: the diffs should produce the same 'editVDocVersion'
-- For example, if
--   editSource e == EditSource [(d1, e1), (d2, e2)]
-- then
--   editVDocVersion e == patch d1 (editVDocVersion e1)
-- end
--   editVDocVersion e == patch d2 (editVDocVersion e2)
--
-- Possible length of source lists:
--  0: the edit is an initial edit
--  1: the edit is a modificiation of another edit
--  2: the edit is a merge of two edits
--
-- It is OK to have longer source lists but we don't use that feature.
--
-- It is OK to have non-empty initial edits.
--
-- It is OK to have multiple initial edits in a VDoc, but we have always exactly one,
-- because two initial edits cannot be merged because they have no common ancestors.
-- So instead of having two initial edits one can have an empty initial edit and base the
-- two edits on that.
newtype EditSource a = EditSource { _unEditSource :: [(OT.Edit RawContent, a)] }
  deriving (Eq, Show, Generic, Functor, Monoid)

data Edit = Edit
  { _editMetaID       :: MetaID Edit
  , _editDesc         :: ST
  , _editKind         :: EditKind
  , _editSource       :: EditSource (ID Edit)
  , _editVDoc         :: ID VDoc
  , _editVDocVersion  :: RawContent     -- FIXME: is it OK to store this in edit (consider serialization)?
  , _editVotes        :: Votes
  , _editChildren     :: Set (ID Edit)
  , _editDiscussions' :: Map (ID Discussion) (Range Position)
  }
  deriving (Eq, Show, Generic)

data EditStats = EditStats
  { _editStatsUsers    :: Int
  , _editStatsEdits    :: Int
  , _editStatsComments :: Int
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance Monoid EditStats where
  mempty = EditStats 0 0 0
  EditStats a b c `mappend` EditStats a' b' c' = EditStats (a + a') (b + b') (c + c')

data CreateEdit = CreateEdit
  { _createEditDesc        :: ST
  , _createEditVDocVersion :: RawContent
  , _createEditKind        :: EditKind
  }
  deriving (Eq, Show, Generic)

data EditKind = Grammar | Phrasing | Meaning | Initial
  deriving (Eq, Ord, Show, Read, Generic, Bounded, Enum)


-- ** composites

-- | Packaged vdoc ready for use by client.  "Applicable
-- contributions" fields only contain contributions made to (or
-- rebased to) "this version".  (More info can easily be added,
-- though.)
data CompositeVDoc = CompositeVDoc
  { _compositeVDoc                      :: VDoc
  , _compositeVDocThisEdit              :: Edit
  , _compositeVDocApplicableEdits       :: Map (ID Edit) Edit
  , _compositeVDocApplicableDiscussions :: Map (ID Discussion) (Range Position, Discussion)
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
    ContribDiscussion Discussion
  | ContribEdit Edit
  | ContribHighlightMark (Range Position)
  deriving (Eq, Show, Generic)

-- | For places where we need heterogeneous lists of different 'ID's, we can use this type.
--
-- | FUTUREWORK: It would be nice to just use @ID Contribution@ instead of 'ContributionID', but
-- that changes the case switch implementation, and I'm not sure right now if it'll still be as
-- straight-forward.
data ContributionID =
    ContribIDDiscussion Bool{-is note-} (ID Discussion)
  | ContribIDEdit (ID Edit)
  deriving (Eq, Ord, Show, Read, Generic)

-- | In the frontend, for replacing the browser selection range with a mark when an editor overlay
-- opens, we need a 'Void'-like contribution kind that cannot have a contribution value.
data HighlightMark


-- | *Groups* are the entities that represent organisations ("zerobuzz.net"),
-- sub-organisations ("zerobuzz developers", "zerobuzz board"), or special interests ("nuke the
-- whales", "equal rights for part-time employees").
--
-- Users can CRUD (Create/Read/Update/Delete) groups.  A user who creates a group has the
-- 'GroupInitiator' 'Role' in that group.
--
-- Groups form a directed acyclic graph (DAG): Every group can have one or more children, but other
-- than in a tree-shaped hierarchy, every child can have multiple parents.
data Group = Group
  { _groupMetaID    :: MetaID Group
  , _groupTitle     :: ST
  , _groupDesc      :: ST
  , _groupParents   :: [ID Group]
  , _groupChildren  :: [ID Group]
  , _groupVDocs     :: [ID VDoc]
  , _groupMembers   :: [ID User]
  , _groupImage     :: Maybe ImageInline
  }
  deriving (Eq, Generic, Show)

type CreateGroup = CreateGroup_ (Map (ID User) Bool)

data CreateGroup_ members = CreateGroup
  { _createGroupTitle :: ST
  , _createGroupDesc  :: ST
  , _createGroupParents  :: [ID Group]
  , _createGroupChildren :: [ID Group]
  , _createGroupMembers  :: members
  , _createGroupImage    :: Maybe ImageInline
  }
  deriving (Eq, Generic, Show)


-- * RawContent

-- | Haskell representation of the javascript @RawDraftContentState@.
-- https://draftjs.org/docs/api-reference-data-conversion.html#content
--
-- FIXME: make this type abstract.  if we construct a value here instead of by calling
-- 'mkRawContent', it may be non-canonical, and that may lead to strange artifacts in the diff.
data RawContent = RawContent
  { _rawContentBlocks    :: NonEmpty (Block EntityKey BlockKey)
  , _rawContentEntityMap :: IntMap Entity  -- ^ for performance, do not use @Map EntityKey Entity@ here.
  }
  deriving (Eq, Show, Generic)

-- | typical rangekey values are 'Int' and 'Entity'
data Block rangeKey blockKey = Block'
  { _blockText'         :: ST
  , _blockEntityRanges' :: Set (rangeKey, EntityRange)  -- ^ entity ranges for entities must not overlap!
  , _blockStyles'       :: Set (EntityRange, Style)     -- ^ entity ranges for styles are allowed to overlap.
  , _blockType'         :: BlockType
  , _blockDepth'        :: Int
  , _blockKey'          :: blockKey  -- ^ 'BlockKey' or, if not available, '()'.
  }
  deriving (Eq, Show, Functor, Generic)

pattern Block
    :: Ord rangeKey => () =>
    ST -> [(rangeKey, EntityRange)] -> [(EntityRange, Style)] -> BlockType -> Int -> blockKey -> Block rangeKey blockKey
pattern Block{_blockText, _blockEntityRanges, _blockStyles, _blockType, _blockDepth, _blockKey}
            <- Block' _blockText (Set.toList -> _blockEntityRanges) (Set.toList -> _blockStyles) _blockType _blockDepth _blockKey
  where Block t r s ty d k =  Block' t (Set.fromList r) (Set.fromList s) ty d k

-- | key into 'rawContentEntityMap'.
newtype EntityKey = EntityKey { _unEntityKey :: Int }
  deriving (Eq, Ord, Show, Read, Generic)

-- | an entity's range may span across multiple blocks
data Entity =
    EntityLink ST  -- ^ url
  | EntityImage ST -- ^ url
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
  | Mark MarkID
    -- styles for visual diff
  | StyleAdded
  | StyleDeleted
  | StyleChanged
  deriving (Show, Eq, Ord, Generic)

-- | identifier for different marks in document
data MarkID
  = MarkCurrentSelection
  | MarkContribution ContributionID Int  -- ^ the Int is used as the Bubble serial number per
                                         -- contribution; note that this is always 0 for notes and
                                         -- discussion (because they have just one range)
  deriving (Show, Read, Eq, Ord, Generic)

-- | each block has a unique blocktype
data BlockType =
    NormalText
  | Header1
  | Header2
  | Header3
  | BulletPoint
  | EnumPoint
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)


-- * OT.Edit RawContent

type OTDoc = NonEmpty DocBlock

-- | (third constructor arg is depth)
type DocBlock = (((Atom BlockType, Atom BlockDepth), NonEditable BlockKey), LineElems)

pattern DocBlock :: BlockType -> BlockDepth -> BlockKey -> [LineElem] -> DocBlock
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
  toUrlPiece = \case
    ContribIDDiscussion True (ID i)  -> "n" <> cs (show i)
    ContribIDDiscussion False (ID i) -> "d" <> cs (show i)
    ContribIDEdit (ID i)             -> "e" <> cs (show i)

instance FromHttpApiData ContributionID where
  parseUrlPiece (ST.splitAt 1 -> (ks, readEither . cs -> Right n))
    | "n" <- ks = f $ ContribIDDiscussion True
    | "d" <- ks = f $ ContribIDDiscussion False
    | "e" <- ks = f ContribIDEdit
    where
      f :: (ID a -> ContributionID) -> Either ST ContributionID
      f c = pure . c $ ID n
  parseUrlPiece bad = Left . cs $ "FromHttpApiData ContributionID: no parse: " <> show bad

instance ToHttpApiData MarkID where
  toUrlPiece = \case
    MarkCurrentSelection -> "h"
    MarkContribution cid n -> "n" <> cs (show n) <> toUrlPiece cid

instance FromHttpApiData MarkID where
  parseUrlPiece "h" = pure MarkCurrentSelection
  parseUrlPiece (cs -> 'n': (span isDigit -> (readEither -> Right n, s))) = do
    cid <- parseUrlPiece $ cs s
    pure $ MarkContribution cid n
  parseUrlPiece bad = Left . cs $ "FromHttpApiData MarkID: no parse: " <> show bad

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

instance (Typeable blockKey, ToJSON blockKey) => ToJSON (Block EntityKey blockKey) where
  toJSON (Block content ranges styles ty depth key) = object $
    [ "text"              .:= content
    , "entityRanges"      .:= (renderRange <$> ranges)
    , "inlineStyleRanges" .:= (renderStyle <$> styles)
    , "depth"             .:= depth  -- ^ (if certain BlockType values force this field to be 0, move this field there.)
    , "type"              .:= ty
    ] <>
    [ "key" .:= key | typeOf key == typeOf (undefined :: BlockKey) ]
    where
      renderRange (k, EntityRange o l) = object ["key"   .:= k, "length" .:= l, "offset" .:= o]
      renderStyle (EntityRange o l, s) = object ["style" .:= s, "length" .:= l, "offset" .:= o]

instance FromJSON (Block EntityKey BlockKey) where
  parseJSON = withObject "Block EntityKey" $ \obj -> Block
    <$> obj .: "text"
    <*> (mapM parseRange =<< (obj .: "entityRanges"))
    <*> (mapM parseStyle =<< (obj .: "inlineStyleRanges"))
    <*> obj .: "type"
    <*> (round <$> (obj .: "depth" :: Parser Double))
    <*> obj .: "key"
    where
      parseRange = withObject "Block EntityKey: entityRanges" $ \obj -> do
        k <- obj .: "key"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure (k, EntityRange o l)
      parseStyle = withObject "Block EntityKey: inlineStyleRanges" $ \obj -> do
        s <- obj .: "style"
        l <- obj .: "length"
        o <- obj .: "offset"
        pure (EntityRange o l, s)

blockTypeToST :: BlockType -> ST
blockTypeToST = \case
  NormalText  -> "unstyled"
  Header1     -> "header-one"
  Header2     -> "header-two"
  Header3     -> "header-three"
  BulletPoint -> "unordered-list-item"
  EnumPoint   -> "ordered-list-item"

blockTypeFromST :: ST -> Maybe BlockType
blockTypeFromST = \case
  "unstyled"            -> Just NormalText
  "header-one"          -> Just Header1
  "header-two"          -> Just Header2
  "header-three"        -> Just Header3
  "unordered-list-item" -> Just BulletPoint
  "ordered-list-item"   -> Just EnumPoint
  _                     -> Nothing

instance ToJSON BlockType where
  toJSON = String . blockTypeToST

instance FromJSON BlockType where
  parseJSON (String (blockTypeFromST -> Just bt)) = pure bt
  parseJSON bad = fail $ "BlockType: no parse for " <> show bad

instance ToJSON Entity where
  toJSON (EntityLink url) = object
    [ "type"            .:= ("LINK" :: ST)
    , "mutability"      .:= ("MUTABLE" :: ST)
    , "data"            .:= object ["url" .:= url]
    ]
  toJSON (EntityImage url) = object
    [ "type"            .:= ("IMAGE" :: ST)
    , "mutability"      .:= ("IMMUTABLE" :: ST)
    , "data"            .:= object ["src" .:= url]
    ]

instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \obj -> do
    ty :: ST <- obj .: "type"
    case ty of
      "LINK" -> let parseData = withObject "LINK data" (.: "url")
                in EntityLink <$> (parseData =<< obj .: "data")
      "IMAGE" -> let parseData = withObject "LINK data" (.: "src")
                 in EntityImage <$> (parseData =<< obj .: "data")
      bad -> fail $ "Entity: no parse for " <> show bad

styleToST :: Style -> ST
styleToST = \case
  Bold         -> "BOLD"
  Italic       -> "ITALIC"
  Underline    -> "UNDERLINE"
  Code         -> "CODE"
  (Mark cid)   -> "MARK__" <> toUrlPiece cid
  StyleAdded   -> "ADDED"
  StyleDeleted -> "DELETED"
  StyleChanged -> "CHANGED"

styleFromST :: ST -> Maybe Style
styleFromST = \case
  "BOLD"                                                   -> Just Bold
  "ITALIC"                                                 -> Just Italic
  "UNDERLINE"                                              -> Just Underline
  "CODE"                                                   -> Just Code
  (ST.splitAt 6 -> ("MARK__", parseUrlPiece -> Right cid)) -> Just $ Mark cid
  "ADDED"                                                  -> Just StyleAdded
  "DELETED"                                                -> Just StyleDeleted
  "CHANGED"                                                -> Just StyleChanged
  _                                                        -> Nothing

instance ToJSON Style where
  toJSON = String . styleToST

instance FromJSON Style where
  parseJSON (String (styleFromST -> Just s)) = pure s
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
    mkDocBlock :: Block EntityKey BlockKey -> DocBlock
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

    mkDocBlock :: DocBlock -> Block Entity BlockKey
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
              [(EntityRange offset l, sty) | (offset, sty) <- acc, sty `Set.notMember` s, let l = n - offset, l > 0]
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
mkRawContent :: NonEmpty (Block Entity ()) -> RawContent
mkRawContent = canonicalizeRawContent . mkRawContentInternal . initBlockKeys

-- | block keys should start with an alpha letter, apparently starting with digits break `document.querySelector`
initBlockKeys :: Ord ek => NonEmpty (Block ek bk) -> NonEmpty (Block ek BlockKey)
initBlockKeys = NEL.zipWith (\k b -> b { _blockKey = BlockKey . cs . ('b':) . show $ k }) (NEL.fromList [(0 :: Int)..])

-- | Construct a 'RawContent' value *without* canonicalizing it.  (The implementation would be much
-- nicer if we had 'Foldable' on 'Block' and lenses, but with the second type parameter, the
-- 'Foldable' instance is a little weird, and TemplateHaskell forces us to define those further down
-- in this module.)
--
-- Note: empty block list is illegal.  For once it will make draft crash in 'stateFromContent'.
mkRawContentInternal :: NonEmpty (Block Entity BlockKey) -> RawContent
mkRawContentInternal bsl = RawContent (index <$> bsl) (IntMap.fromList entities)
  where
    -- FUTUREWORK: it is possible to do just one traversal to collect and index entities
    -- https://www.reddit.com/r/haskell/comments/610sa1/applicative_sorting/
    entities :: [(Int, Entity)]
    entities = zip [0..] . nub $ concatMap (fmap fst . _blockEntityRanges) bsl

    index :: Block Entity BlockKey -> Block EntityKey BlockKey
    index b = b { _blockEntityRanges' = _blockEntityRanges' b & (Set.map . first $
                    \e -> EntityKey . fromMaybe (error "mkRawContentInternal: impossible") $ Map.lookup e em) }

    em = Map.fromList $ (\(a, b) -> (b, a)) <$> entities

mkBlock :: Ord rangeKey => ST -> Block rangeKey ()
mkBlock t = Block t [] [] NormalText 0 ()

emptyBlock :: Ord rangeKey => Block rangeKey ()
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
      . fmap (\(EntityRange offset len, s) -> (offset, (offset + len, s)))
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


-- * comment

type CommentText = ST

data CreateDiscussion range = CreateDiscussion
  { _createDiscussionStatementText :: CommentText
  , _createDiscussionRange         :: range
  , _createDiscussionIsNote        :: Bool
  } -- FIXME: add (createDiscussionEdit :: ID Edit) and simplify SAddDiscussion (fisx does not like
    -- this, but does not want to spend time arguing about it)
  deriving (Eq, Ord, Show, Generic)

data Discussion = Discussion
  { _discussionMetaID :: MetaID Discussion
  , _discussionVDoc   :: ID VDoc
  , _discussionTree   :: Tree Statement
  , _discussionVotes  :: Votes
  , _discussionIsNote :: Bool  -- ^ the discussion is just a note
  }
  deriving (Eq, Show, Generic)

newtype CreateStatement = CreateStatement
  { _createStatementText :: CommentText
  } -- FIXME: add (createStatementParent :: Either (ID Discussion) (ID Statement)) and remove
    -- "onstatementid" from SAddStatement (fisx does not like this, but does not want to spend time
    -- arguing about it)
  deriving (Eq, Ord, Show, Read, Generic)

data Statement = Statement
  { _statementMetaID :: MetaID Statement
  , _statementVDoc   :: ID VDoc
  , _statementText   :: CommentText
  , _statementParent :: Maybe (ID Statement)  -- FIXME: remove this, this is unnecessary if we use the Tree data structure in Discussion
  }
  deriving (Eq, Ord, Show, Read, Generic)

type Comment = Discussion


-- * Derived instances

deriveClasses
  [ ([''RawContent, ''Block, ''Entity, ''Style, ''BlockType]
    , [''NFData, ''SOP.Generic, ''Lens'])
  , ([ ''EntityKey, ''CompositeVDoc, ''ContributionID, ''MarkID
     , ''VDoc, ''CreateVDoc, ''UpdateVDoc, ''EditSource, ''Edit
     , ''EditStats, ''CreateEdit, ''EditKind, ''Title, ''Abstract
     , ''Group, ''CreateGroup_]
    , allClass)
  ]

-- | It cannot moved further up, needs instance NFData BlockType
deriving instance NFData (EEdit RawContent)

makeRefineTypes [''CreateDiscussion, ''Discussion, ''CreateStatement, ''Statement]


-- * helper lenses

vdocID :: Lens' VDoc (ID VDoc)
vdocID = vdocMetaID . miID

editID :: Lens' Edit (ID Edit)
editID = editMetaID . miID

groupID :: Lens' Group (ID Group)
groupID = groupMetaID . miID
{-
contributionID :: Lens' Contribution ContributionID
contributionID k = \case
  ContribDiscussion i -> ContribDiscussion <$> discussionID (fmap (\(ContribIDDiscussion i') -> i') . k . ContribIDDiscussion) i
  ContribEdit i       -> ContribEdit       <$> editID       (fmap (\(ContribIDEdit i')       -> i') . k . ContribIDEdit) i
-}
compositeVDocThisEditID :: Lens' CompositeVDoc (ID Edit)
compositeVDocThisEditID = compositeVDocThisEdit . editID

compositeVDocThisVersion :: Lens' CompositeVDoc RawContent
compositeVDocThisVersion = compositeVDocThisEdit . editVDocVersion

blockText :: Lens' (Block rangeKey blockKey) ST
blockText = blockText'

blockEntityRanges :: Ord rangeKey => Lens' (Block rangeKey blockKey) [(rangeKey, EntityRange)]
blockEntityRanges = blockEntityRanges' . iso Set.toList Set.fromList

blockStyles :: Lens' (Block rangeKey blockKey) [(EntityRange, Style)]
blockStyles = blockStyles' . iso Set.toList Set.fromList

blockType :: Lens' (Block rangeKey blockKey) BlockType
blockType = blockType'

blockDepth :: Lens' (Block rangeKey blockKey) Int
blockDepth = blockDepth'

blockKey :: Lens' (Block rangeKey blockKey) blockKey
blockKey = blockKey'

noteText :: Lens' Discussion CommentText
noteText = discussionTree . treeRoot . statementText

treeRoot :: Lens' (Tree a) a
treeRoot f (Node a t) = f a <&> flip Node t


-- * helper functions

-- TUNING: speed this up by adding an index structure to RawContent
mkBlockIndex :: RawContent -> Int -> BlockIndex
mkBlockIndex rc i = BlockIndex i $ ((rc ^. rawContentBlocks) NEL.!! i) ^. blockKey

-- TUNING: speed this up by adding an index structure to RawContent
fromSelectionPoint :: RawContent -> SelectionPoint -> Position
fromSelectionPoint rc (Position k r) = Position (BlockIndex i k) r
  where
    i = case [i' | (i', b) <- zip [0..] . NEL.toList $ rc ^. rawContentBlocks, b ^. blockKey == k] of
        i': _ -> i'
        _ -> error "impossible"

fromSelectionState :: RawContent -> SelectionState -> Selection Position
fromSelectionState rc (SelectionState sel _) = fromSelectionPoint rc <$> sel

-- previous and next positions, the given one is also in the list
surroundingPositions :: RawContent -> Position -> ([Position]{-previous, reversed-}, [Position]{-next-})
surroundingPositions rc (Position (BlockIndex i _) col)
  = ( [ Position (mkBlockIndex rc r) c
      | (r, l) <- zip [i, i-1..] $ col: prev
      , c <- [l, l-1..0]
      ]
    , [ Position (mkBlockIndex rc r) c
      | (r, cs') <- zip [i..] $ [col..this]: [[0..l] | l <- next]
      , c <- cs'
      ]
    )
  where
    (prev, this: next) = focusList (len <$> NEL.toList (rc ^. rawContentBlocks)) !! i
    len b = ST.length $ b ^. blockText

-- TUNING: speed this up by adding an index structure to RawContent
toStylePosition :: RawContent -> Position -> StylePosition
toStylePosition rc p_@(Position (BlockIndex i_ _) col)
    | col == 0   = g (f (-1) $ b_: bs_) p_ rev
    | col == lb_ = StylePosition p_ $ f 0 bs_
    | otherwise  = StylePosition p_ 0
  where
    (rev, b_: bs_) = focusList (NEL.toList $ rc ^. rawContentBlocks) !! i_
    lb_ = len b_

    f m [] = m
    f m (b: bs)
        | len b == 0 = f (m+1) bs
        | otherwise  = m+1

    g m p [] = StylePosition p m
    g m (Position (BlockIndex i _) _) (b: bs)
        | lb == 0 = g (m+1) p' bs
        | otherwise = StylePosition p' (m+1)
      where
        lb = len b
        p' = Position (BlockIndex (i-1) $ b ^. blockKey) lb

    len b = ST.length $ b ^. blockText

surroundingStylePositions :: RawContent -> StylePosition -> ([StylePosition]{-previous, reversed-}, [StylePosition]{-next-})
surroundingStylePositions rc sp
  = surroundingPositions rc (basePosition sp)
  & both %~ fmap head . group . fmap (toStylePosition rc)

-- TUNING: speed this up
toStyleRanges :: RawContent -> Ranges Position -> Ranges StylePosition
toStyleRanges rc rs = mconcat $ (rangesFromRange False . fmap (toStylePosition rc)) <$> unRanges rs

-- all positions which maps to the same style position
stylePositions :: RawContent -> StylePosition -> [Position]
stylePositions rc (StylePosition p@(Position (BlockIndex i _) _) m)
    = p: [Position (mkBlockIndex rc j) 0 | j <- [i+1..i+m]]

-- this computes the minimal selection range
fromStyleRange :: RawContent -> Range StylePosition -> Range Position
fromStyleRange rc (Range a b)
  | a < b = RangeInner (last $ stylePositions rc a) (basePosition b)
  | a == b = RangeInner (basePosition a) (basePosition a)
  | otherwise = error "range invariant failed"

-- TUNING: speed this up by adding an index structure to RawContent
toLeafSelector :: Bool -> RawContent -> Position -> (LeafSelector, Int)
toLeafSelector top rc (Position (BlockIndex i key) col) = (Position key (SpanIndex dec_ sp_), col - beg)
  where
    DocBlock _ _ _ es_ = rawContentToDoc rc NEL.!! i

    (((beg, _), (dec_, sp_)): _)
        = dropWhile (cmp top . fst) . zip (zip lengths $ tail lengths) $ mkSelectors 0 0 es_

    cmp True  (_, e) = col >= e
    cmp False (_, e) = col >  e

    lengths = scanl (+) 0 $ (ST.length . unNonEmptyST . snd <$> es_) <> [0]

    mkSelectors _ _ [] = []
    mkSelectors dec sp (((Atom me, _), _): es) = (dec, sp):
        if isJust me then mkSelectors (dec+1) 0 es else mkSelectors dec (sp+1) es

-- The range cannot be empty
-- this computes the minimal selection range
styleRangeToLeafSelectors :: RawContent -> Range StylePosition -> Range LeafSelector
styleRangeToLeafSelectors rc (Range a b) | a < b = RangeInner a' b'
  where
    (a', 0) = toLeafSelector True rc . last $ stylePositions rc a
    (b', _) = toLeafSelector False rc $ basePosition b

lineElemLength :: LineElem -> Int
lineElemLength (_, NonEmptyST txt) = ST.length txt


-- * some convenience lenses

discussionID :: Lens' Discussion (ID Discussion)
discussionID = discussionMetaID . miID

statementID :: Lens' Statement (ID Statement)
statementID = statementMetaID . miID
