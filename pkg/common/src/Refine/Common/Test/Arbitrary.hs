{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Test.Arbitrary where

import           Control.Arrow (second)
import           Control.Monad.State
import           Data.Function (on)
import           Data.Functor.Infix ((<$$>))
import           Data.List ((\\), partition, nub, nubBy)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.String.Conversions (ST)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import           Data.Tree
import qualified Data.Vector as V
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser as HTML
import           Text.HTML.Tree as HTML

import Refine.Common.Types
import Refine.Common.VDoc.HTML
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Splice


instance Arbitrary (ID a) where
  arbitrary = ID <$> arbitrary

instance Arbitrary ChunkPoint where
  arbitrary = ChunkPoint <$> arbitrary <*> arbitrary

instance Arbitrary DataUID where
  arbitrary = DataUID <$> arbitrary


-- * html-parse

instance Arbitrary ContributionID where
  arbitrary = oneof
    [ ContribIDNote <$> arbitrary
    , ContribIDQuestion <$> arbitrary
    , ContribIDDiscussion <$> arbitrary
    , ContribIDEdit <$> arbitrary
    , pure ContribIDHighlightMark
    ]

instance Arbitrary PreToken where
  arbitrary = oneof [PreToken <$> arbitrary, PreMarkOpen <$> arbitrary, PreMarkClose <$> arbitrary]

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
chunkRangeErrors :: ChunkRange -> VDocVersion 'HTMLCanonical -> [ChunkRangeError]
chunkRangeErrors (ChunkRange mp1 mp2) = createChunkRangeErrors $ CreateChunkRange mp1 mp2


instance Arbitrary (VDocVersion 'HTMLRaw) where
  arbitrary = arbitraryRawVDocVersion

instance Arbitrary (VDocVersion 'HTMLCanonical) where
  arbitrary = arbitraryCanonicalNonEmptyVDocVersion
  shrink = shrinkCanonicalNonEmptyVDocVersion  -- TODO: shrinking is slow and probably buggy.

data VersWithRanges = VersWithRanges (VDocVersion 'HTMLCanonical) [SomethingWithChunkRangeAndID]
  deriving (Eq, Show)

data SomethingWithChunkRangeAndID = SomethingWithChunkRangeAndID ChunkRange ContributionID
  deriving (Eq, Show)

instance HasChunkRangeAndID SomethingWithChunkRangeAndID where
  askChunkRange (SomethingWithChunkRangeAndID r _) = r
  askID         (SomethingWithChunkRangeAndID _ i) = i

instance Arbitrary VersWithRanges where
  arbitrary = arbitraryVersWithRanges
  shrink = shrinkVersWithRanges  -- TODO: shrinking is slow and probably buggy.

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

      -- this makes test output much more interesting, and probably not any less representative of
      -- production data.
      muteAttrs_ attrs = case partition isDataUID attrs of (datauid, _) -> datauid
        where
          isDataUID (Attr "data-uid" _) = True
          isDataUID _                   = False

      muteAttrs (TagOpen n attrs) = TagOpen n $ muteAttrs_ attrs
      muteAttrs (TagSelfClose n attrs) = TagSelfClose n $ muteAttrs_ attrs
      muteAttrs t = t

  pure . canonicalizeVDocVersion . VDocVersion . (muteAttrs <$$>) $ v'

shrinkCanonicalNonEmptyVDocVersion :: VDocVersion 'HTMLCanonical -> [VDocVersion 'HTMLCanonical]
shrinkCanonicalNonEmptyVDocVersion (VDocVersion forest) = VDocVersion <$> shrink forest
  -- (the Token arb instance needs to make sure that data-uid attributes are not killed.)


arbitraryVersWithRanges :: Gen VersWithRanges
arbitraryVersWithRanges = do
  v <- arbitraryCanonicalNonEmptyVDocVersion
  let crs = allNonEmptyCreateChunkRanges v
      rs = zipWith (\(CreateChunkRange b e) i -> SomethingWithChunkRangeAndID (ChunkRange b e) (ContribIDNote (ID i))) crs [0..]
  VersWithRanges v . nub <$> vectorOf 11 (elements rs)

shrinkVersWithRanges :: VersWithRanges -> [VersWithRanges]
shrinkVersWithRanges (VersWithRanges v rs) = do
  v' <- shrink v
  rs' <- shrinkList (\_ -> []) (filter (null . (`chunkRangeErrors` v') . askChunkRange) rs)
  [VersWithRanges v' rs' | not $ null rs']


type AllChunkPointsState = (Int, [(Int, DataUID)])

-- | All theoretically possible chunk points, together with the total number of chars to their left in
-- the 'VDocVersion'.
allChunkPoints :: VDocVersion 'HTMLCanonical -> [(Int, ChunkPoint)]
allChunkPoints (VDocVersion forest) = nubBy ((==) `on` snd) $ evalState (dfs forest) (0, [])
  where
    dfs :: Forest Token -> State AllChunkPointsState [(Int, ChunkPoint)]
    dfs (Node (ContentText s) [] : siblings)
      | ST.length s > 0 = do
      let upd :: AllChunkPointsState -> ((Int, (Int, DataUID)), AllChunkPointsState)
          upd (leftcharsForest, top@(leftcharsHere, uid) : stack)
            = ((leftcharsForest, top), (leftcharsForest', top' : stack))
            where
              top'             = (mv leftcharsHere, uid)
              leftcharsForest' = mv leftcharsForest
              mv               = (ST.length s +)
          upd (_, []) = error $ "allChunkPoints: text node without wrapping tag: " <> show (s, forest)

      (leftcharsForest, (leftcharsHere, uid)) <- state upd

      let mkpoint :: Int -> (Int, ChunkPoint)
          mkpoint ((+) -> mv) = (mv leftcharsForest, ChunkPoint uid (mv leftcharsHere))

      ((mkpoint <$> [0 .. ST.length s]) <>) <$> dfs siblings

    dfs (t@(Node (dataUidOfToken -> Just uid) children@(_:_)) : siblings)
      | treeTextLength t > 0 = do
      let pushuid = modify $ second ((0, uid) :)
          popuid  = modify . second $ \case
            (_ : (leftcharsHere, uid') : stack) -> (leftcharsHere + treeTextLength t, uid') : stack
            [_] -> []
            [] -> error "allChunkPoints: impossibly empty stack."

      pushuid
      childrenPoints <- dfs children
      popuid
      siblingsPoints <- dfs siblings

      pure $ childrenPoints <> siblingsPoints

    dfs (_ : siblings) = dfs siblings
    dfs [] = pure []


allNonEmptyCreateChunkRanges :: VDocVersion 'HTMLCanonical -> [CreateChunkRange]
allNonEmptyCreateChunkRanges vers =
  CreateChunkRange Nothing Nothing : cheapnub (snd <$> allNonEmptyCreateChunkRanges_ vers)
  where
    cheapnub (x : y : ys) = [x | x /= y] <> cheapnub (y : ys)
    cheapnub (y : ys)     = y : cheapnub ys
    cheapnub []           = []

-- | 'allNonEmptyCreateChunkRanges', plus absolute offsets.
allNonEmptyCreateChunkRanges_ :: VDocVersion 'HTMLCanonical -> [((Int, Int), CreateChunkRange)]
allNonEmptyCreateChunkRanges_ vers = result
  where
    result = filter nonempty (mconcat rs)

    ps = V.fromList . allChunkPoints $ vers
    ix = [0 .. V.length ps - 1]

    rs = [ let (n, b) = ps V.! i
               (k, e) = ps V.! j
           in [ ((n, maxk), CreateChunkRange (Just b) Nothing)
              , ((0, k),    CreateChunkRange Nothing (Just e))
              ]
              <> replicate 11 ((n, k), CreateChunkRange (Just b) (Just e))
         | i <- ix, j <- ix, i < j
         ]

    nonempty ((n, k), _) = n < k

    maxk = fst $ V.last ps
