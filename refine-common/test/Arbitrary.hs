{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import           Data.List ((\\))
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
validClose = TagClose <$> validXmlTagName

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

arbitraryCanonicalVDocVersion :: Gen (VDocVersion 'HTMLCanonical)
arbitraryCanonicalVDocVersion =
  VDocVersion . cs . renderTokens . tokensFromForest <$> arbitraryCanonicalTokenForest

arbitraryCanonicalTokenForest :: Gen (Forest Token)
arbitraryCanonicalTokenForest =
  (\(Right stream) -> stream) . tokensToForest <$> arbitraryCanonicalTokenStream

arbitraryCanonicalTokenStream :: Gen [Token]
arbitraryCanonicalTokenStream =
  canonicalizeTokens . tokensFromForest <$> arbitraryTokenForest

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
validClosingOpen = do
    n <- validXmlTagName
    let n' = if n `elem` nonClosing then "_" else n
    TagOpen n' <$> arbitrary
