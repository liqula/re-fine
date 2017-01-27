{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Test.Arbitrary where

import           Control.Exception (assert)
import           Control.Lens ((^.))
import           Data.List ((\\))
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.String.Conversions (cs)
import           Data.Tree
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Text.HTML.Parser as HTML
import           Text.HTML.Tree as HTML

import Refine.Common.Types
import Refine.Common.VDoc.HTML
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Splice (chunkRangeCanBeApplied)

instance Arbitrary ChunkPoint where
  arbitrary               = ChunkPoint <$> arbitrary <*> arbitrary
  shrink (ChunkPoint n o) = ChunkPoint <$> shrink n <*> shrink o

instance Arbitrary DataUID where
  arbitrary          = DataUID <$> arbitrary
  shrink (DataUID i) = DataUID <$> shrink i

instance Arbitrary (ID a) where
  arbitrary     = ID <$> arbitrary
  shrink (ID i) = ID <$> shrink i

instance Arbitrary Token where
  arbitrary = oneof [validOpen, validClose, validFlat]

  shrink (TagOpen n as)      = TagOpen n <$> shrink as
  shrink (TagSelfClose n as) = TagSelfClose n <$> shrink as
  shrink (TagClose _)        = []
  shrink (ContentText _)     = []
  shrink (ContentChar _)     = []
  shrink (HTML.Comment b)    = HTML.Comment . B.fromText <$> (shrink . TL.toStrict . B.toLazyText $ b)
  shrink (Doctype t)         = Doctype <$> shrink t

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

-- FIXME: sometimes it is allowed to use '<' as text token, and we don't test that yet.  (whether we
-- like this choice or not, we may want to follow the standard here.)  (same in tag names, attr
-- names.)
validXmlChar :: Gen Char
validXmlChar = elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /<>")

validXmlText :: Gen T.Text
validXmlText = T.pack <$> sized (`maxListOf` validXmlChar)

validXmlTagName :: Gen T.Text
validXmlTagName = do
    initchar  <- elements $ ['a'..'z'] <> ['A'..'Z']
    thenchars <- sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /<>"))
    pure . T.pack $ initchar : thenchars

validClosingXmlTagName :: Gen T.Text
validClosingXmlTagName = do
    n <- validXmlTagName
    pure $ if n `elem` nonClosing then n <> "phoo" else n

validXmlAttrName :: Gen T.Text
validXmlAttrName = do
    initchar  <- elements $ ['a'..'z'] <> ['A'..'Z']
    thenchars <- sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /=<>\x00"))
    pure . T.pack $ initchar : thenchars

-- FIXME: not sure if @Attr "key" "\""@ should be parseable, but it's not, so we don't test it.
validXmlAttrValue :: Gen T.Text
validXmlAttrValue = do
    T.pack <$> sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /=<>\x00\""))

-- FIXME: i think this should be 'validXmlChar', but that will fail the test suite.
validXmlCommentText :: Gen T.Text
validXmlCommentText = do
    T.pack <$> sized (`maxListOf` elements (['\x20'..'\x7E'] \\ "\x09\x0a\x0c /=<>\x00\"-"))

maxListOf :: Int -> Gen a -> Gen [a]
maxListOf n g = take n <$> listOf g

instance Arbitrary (VDocVersion 'HTMLCanonical) where
  arbitrary = arbitraryCanonicalVDocVersion

arbitraryCanonicalNonEmptyVDocVersion :: Gen (VDocVersion 'HTMLCanonical)
arbitraryCanonicalNonEmptyVDocVersion = do
  VDocVersion v <- arbitraryCanonicalVDocVersion
  let v' = if (sum . fmap tokenTextLength . parseTokens $ v) == 0
        then "<span>whee</span>" <> v
        else v
  pure . (\(Right v'') -> v'') . canonicalizeVDocVersion $ VDocVersion v'

arbitraryCanonicalVDocVersion :: Gen (VDocVersion 'HTMLCanonical)
arbitraryCanonicalVDocVersion =
  (\(Right v) -> v) .
  canonicalizeVDocVersion .
  VDocVersion . cs . renderTokens . tokensFromForest <$> arbitraryTokenForest

arbitraryCanonicalTokenForest :: Gen (Forest Token)
arbitraryCanonicalTokenForest =
  (\(Right v) -> v) . tokensToForest <$> arbitraryCanonicalTokenStream

arbitraryCanonicalTokenStream :: Gen [Token]
arbitraryCanonicalTokenStream =
  parseTokens . _unVDocVersion <$> arbitraryCanonicalVDocVersion

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

arbitraryValidChunkPoint :: VDocVersion h -> Maybe ChunkPoint -> Gen (Maybe ChunkPoint)
arbitraryValidChunkPoint vers mLeftBound = do
  let forest :: Forest Token
      Right forest = tokensToForest . parseTokens . _unVDocVersion $ vers

      uids :: [DataUID]
      uids = catMaybes $ dataUidOfToken <$> mconcat (flatten <$> forest)

      validUids :: [DataUID]
      validUids = case mLeftBound of
        Nothing                    -> uids
        Just (ChunkPoint uid _off) -> tail $ dropWhile (/= uid) uids

  cp <- case validUids of
    [] -> pure Nothing
    _ -> do
      uid <- elements validUids
      off <- choose (0, forestTextLength $ forest ^. atNode ((== Just uid) . dataUidOfToken))
      pure . Just $ ChunkPoint uid off

  frequency [(5, pure Nothing), (95, pure cp)]

arbitraryValidChunkRange :: VDocVersion h -> Gen (ChunkRange a)
arbitraryValidChunkRange vers = do
  b <- arbitraryValidChunkPoint vers Nothing
  e <- arbitraryValidChunkPoint vers b
  pure $ ChunkRange (ID 3) b e

arbitraryChunkRangesWithVersion :: Gen (VDocVersion 'HTMLCanonical, [ChunkRange a])
arbitraryChunkRangesWithVersion = do
  v  <- arbitraryCanonicalNonEmptyVDocVersion
  rs <- listOf $ arbitraryValidChunkRange v
  assert (all (`chunkRangeCanBeApplied` v) rs) $ pure (v, rs)
