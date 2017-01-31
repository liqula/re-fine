{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Test.Arbitrary where

import           Control.Lens ((^.))
import           Data.Int (Int64)
import           Data.List ((\\), partition)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.String.Conversions (ST)
import           Data.Tree
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser as HTML
import           Text.HTML.Tree as HTML
import           Web.HttpApiData (toUrlPiece)

import Refine.Common.Types
import Refine.Common.VDoc.HTML
import Refine.Common.VDoc.HTML.Core


instance Arbitrary (ID a) where
  arbitrary = ID <$> arbitrary

instance Arbitrary ChunkPoint where
  arbitrary = ChunkPoint <$> arbitrary <*> arbitrary

instance Arbitrary DataUID where
  arbitrary = DataUID <$> arbitrary


-- * html-parse

instance Arbitrary PreToken where
  arbitrary = oneof [PreToken <$> arbitrary, open, close]
    where
      open  = PreMarkOpen <$> uid <*> owner
      close = PreMarkClose <$> uid
      uid   = toUrlPiece . unDataUID <$> (arbitrary :: Gen DataUID)
      owner = elements ["edit", "node", "question", "discussion"]

  shrink (PreToken t) = PreToken <$> shrink t
  shrink _ = []

instance Arbitrary Token where
  arbitrary = oneof [validOpen, validClose, validFlat]

  shrink (TagOpen n as)      = TagOpen n <$> shrinkAttrs as
  shrink (TagSelfClose n as) = TagSelfClose n <$> shrinkAttrs as
  shrink (TagClose _)        = []
  shrink (ContentText _)     = []
  shrink (ContentChar _)     = []
  shrink (HTML.Comment b)    = HTML.Comment . B.fromText <$> (shrink . LT.toStrict . B.toLazyText $ b)
  shrink (Doctype t)         = Doctype <$> shrink t

-- TODO: This does not catch on when shrinking `Forest PreToken`.  no idea why.
shrinkAttrs :: [Attr] -> [[Attr]]
shrinkAttrs attrs = case partition isDataUID attrs of
  (datauid, other) -> (datauid <>) <$> shrink other
  where
    isDataUID (Attr "data-uid" _) = True
    isDataUID _                   = False

instance Arbitrary Attr where
  arbitrary = Attr <$> validXmlAttrName <*> validXmlAttrValue
  shrink (Attr k v) = Attr <$> shrink k <*> shrink v

validOpen :: Gen Token
validOpen = TagOpen <$> validXmlTagName <*> arbitrary

validClose :: Gen Token
validClose = TagClose <$> validClosingXmlTagName

validFlat :: Gen Token
validFlat = oneof
    [ TagSelfClose <$> validXmlTagName <*> arbitrary
    , ContentChar <$> validXmlChar
    , ContentText <$> validXmlText
    , HTML.Comment . B.fromText <$> validXmlCommentText
    , Doctype <$> validXmlText
    ]


-- | Set this to False for nicer test failure reporting, but possibly less rigorous testing.
useWeirdXmlNames :: Bool
useWeirdXmlNames = False

simpleNames :: [ST]
simpleNames = ["wef", "phoo", "x123"]


-- FIXME: sometimes it is allowed to use '<' as text token, and we don't test that yet.  (whether we
-- like this choice or not, we may want to follow the standard here.)  (same in tag names, attr
-- names.)
validXmlChar :: Gen Char
validXmlChar = elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /<>")

validXmlText :: Gen ST
validXmlText = if useWeirdXmlNames
  then ST.pack <$> sized (`maxListOf` validXmlChar)
  else elements simpleNames

validXmlTagName :: Gen ST
validXmlTagName = if useWeirdXmlNames
  then do
    initchar  <- elements $ ['a'..'z'] <> ['A'..'Z']
    thenchars <- sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /<>"))
    pure . ST.pack $ initchar : thenchars
  else elements simpleNames

validClosingXmlTagName :: Gen ST
validClosingXmlTagName = do
    n <- validXmlTagName
    pure $ if n `elem` nonClosing then n <> "phoo" else n

validXmlAttrName :: Gen ST
validXmlAttrName = if useWeirdXmlNames
  then do
    initchar  <- elements $ ['a'..'z'] <> ['A'..'Z']
    thenchars <- sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /=<>\x00"))
    pure . ST.pack $ initchar : thenchars
  else elements simpleNames

-- FIXME: not sure if @Attr "key" "\""@ should be parseable, but it's not, so we don't test it.
validXmlAttrValue :: Gen ST
validXmlAttrValue = if useWeirdXmlNames
  then do
    ST.pack <$> sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /=<>\x00\""))
  else elements simpleNames

-- FIXME: i think this should be 'validXmlChar', but that will fail the test suite.
validXmlCommentText :: Gen ST
validXmlCommentText = do
    ST.pack <$> sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /=<>\x00\"-"))

maxListOf :: Int -> Gen a -> Gen [a]
maxListOf n g = take n <$> listOf g

arbitraryTokenForest :: Gen (Forest Token)
arbitraryTokenForest = listOf arbitraryTokenTree

arbitraryTokenTree :: Gen (Tree Token)
arbitraryTokenTree = oneof
    [ Node <$> validClosingOpen    <*> scale (`div` 5) arbitraryTokenForest
    , Node <$> validNonClosingOpen <*> pure []
    , Node <$> validFlat           <*> pure []
    ]

validNonClosingOpen :: Gen Token
validNonClosingOpen = TagOpen <$> elements nonClosing <*> arbitrary

validClosingOpen :: Gen Token
validClosingOpen = TagOpen <$> validClosingXmlTagName <*> arbitrary


-- * vdoc versions and valid chunk points

-- | For testing only!  In production code, only use 'createChunkRangeErrors'!
chunkRangeErrors :: ChunkRange a -> VDocVersion 'HTMLCanonical -> [ChunkRangeError]
chunkRangeErrors (ChunkRange _ mp1 mp2) = createChunkRangeErrors $ CreateChunkRange mp1 mp2


instance Arbitrary (VDocVersion 'HTMLRaw) where
  arbitrary = arbitraryRawVDocVersion

instance Arbitrary (VDocVersion 'HTMLCanonical) where
  arbitrary = arbitraryCanonicalNonEmptyVDocVersion
  shrink = shrinkCanonicalNonEmptyVDocVersion

data VersWithRanges = VersWithRanges (VDocVersion 'HTMLCanonical) [ChunkRange Edit]
  deriving (Eq, Show)

instance Arbitrary VersWithRanges where
  arbitrary = arbitraryVersWithRanges
  shrink = shrinkVersWithRanges

data CanonicalVDocVersionPairWithDataUID =
    CanonicalVDocVersionPairWithDataUID (Forest Token, DataUID) (Forest Token)
  deriving (Eq, Show)

instance Arbitrary CanonicalVDocVersionPairWithDataUID where
  arbitrary = do
    VDocVersion forest <- arbitraryCanonicalNonEmptyVDocVersion
    uid <- elements . catMaybes $ dataUidOfToken <$> mconcat (flatten <$> forest)
    VDocVersion forest' <- arbitraryCanonicalNonEmptyVDocVersion
    pure $ CanonicalVDocVersionPairWithDataUID (forest, uid) forest'


arbitraryRawVDocVersion :: Gen (VDocVersion 'HTMLRaw)
arbitraryRawVDocVersion = VDocVersion <$> arbitraryTokenForest

arbitraryCanonicalVDocVersion :: Gen (VDocVersion 'HTMLCanonical)
arbitraryCanonicalVDocVersion = canonicalizeVDocVersion <$> arbitraryRawVDocVersion

arbitraryCanonicalNonEmptyVDocVersion :: Gen (VDocVersion 'HTMLCanonical)
arbitraryCanonicalNonEmptyVDocVersion = do
  VDocVersion v <- arbitraryCanonicalVDocVersion
  let v' = if (sum . fmap tokenTextLength . tokensFromForest $ v) == 0
        then Node (TagOpen "span" []) [Node (ContentText "whee") []] : v
        else v
  pure . canonicalizeVDocVersion . VDocVersion $ v'

shrinkCanonicalNonEmptyVDocVersion :: VDocVersion 'HTMLCanonical -> [VDocVersion 'HTMLCanonical]
shrinkCanonicalNonEmptyVDocVersion (VDocVersion forest) = VDocVersion <$> shrink forest
  -- (the Token arb instance needs to make sure that data-uid attributes are not killed.)


arbitraryVersWithRanges :: Gen VersWithRanges
arbitraryVersWithRanges = do
  v   <- arbitraryCanonicalNonEmptyVDocVersion
  rs_ <- listOf $ arbitraryValidChunkRange v
  let rs = zipWith ($) rs_ [0..]
  case mconcat $ (`chunkRangeErrors` v) <$> rs of
    []        -> pure $ VersWithRanges v rs
    bad@(_:_) -> error $ show bad

arbitraryValidChunkRange :: VDocVersion h -> Gen (Int64 -> ChunkRange a)
arbitraryValidChunkRange (VDocVersion v) = do
  b <- arbitraryValidChunkPointBegin v
  e <- arbitraryValidChunkPointEnd v b
  pure $ \i -> ChunkRange (ID i) b e

-- | pick a uid at random from the forest; look at all direct children of the node with that uid;
-- pick one that is a text node and pick an offset in that node; finally, try again if we failed to
-- leave at least one character to the right.
arbitraryValidChunkPointBegin :: Forest Token -> Gen (Maybe ChunkPoint)
arbitraryValidChunkPointBegin forest = go 300  -- TODO: this is iterating too much, need to find a better implementation
  where
    go :: Int -> Gen (Maybe ChunkPoint)
    go 0 = error "arbitraryValidChunkPointBegin could not find non-empty data-uid."
    go i = case catMaybes $ dataUidOfToken <$> mconcat (flatten <$> forest) of
      [] -> go (i - 1)
      uids -> do
        uid :: DataUID <- elements uids
        let [Node _ sub] = forest ^. atToken uid
        case chunkPointOffsetCandidates 0 sub of
          [] -> go (i - 1)
          cands@(_:_) -> do
            mpoint <- pickChunkPointOffset uid cands
            case hitTheRightWall <$> mpoint of
              Just True -> go (i - 1)
              _ -> pure mpoint

    hitTheRightWall :: ChunkPoint -> Bool
    hitTheRightWall (ChunkPoint node off)
      = consumeOffset off
      . dropWhile ((/= Just node) . dataUidOfToken)
      . tokensFromForest
      $ forest
      where
        -- consumed at least one character than eaten up by offset, then we're good.
        consumeOffset n _ | n < 0 = False
        consumeOffset _ []        = True
        consumeOffset n (t : ts)  = consumeOffset (n - tokenTextLength t) ts

-- | calculate offset candidates of siblings with the lengths of text to their resp. lefts.
chunkPointOffsetCandidates :: Int -> Forest Token -> [(Int, ST)]
chunkPointOffsetCandidates = f
  where
    f total (Node (ContentText t) [] : ss) = (total, t) : f (total + ST.length t)      ss
    f total (s                       : ss) =              f (total + treeTextLength s) ss
    f _     []                             = []

pickChunkPointOffset :: DataUID -> [(Int, ST)] -> Gen (Maybe ChunkPoint)
pickChunkPointOffset _ [] = error "impossible."
pickChunkPointOffset uid cands@(_:_) = do
  (leftLength, thisText) <- elements cands
  off <- choose (leftLength, leftLength + ST.length thisText)
  let point = ChunkPoint uid off
  frequency [(5, pure Nothing), (95, pure (Just point))]

-- | TODO: dfs the forest.  when hitting the left node's uid (or immediately if left node is nothing),
-- start counting characters.  when the number of characters traversed is 1 + offset of left node
-- (or 1 if left node is nothing), you have hit the range of points that you can return.  construct
-- a list of all points between here and the end of file, then pick one from that list at random.
arbitraryValidChunkPointEnd :: Forest Token -> Maybe ChunkPoint -> Gen (Maybe ChunkPoint)
arbitraryValidChunkPointEnd _ _ = pure Nothing


shrinkVersWithRanges :: VersWithRanges -> [VersWithRanges]
shrinkVersWithRanges (VersWithRanges v rs) = do
  v' <- shrink v
  rs' <- shrinkList (\_ -> []) (filter (null . (`chunkRangeErrors` v')) rs)
  pure $ VersWithRanges v' rs'
