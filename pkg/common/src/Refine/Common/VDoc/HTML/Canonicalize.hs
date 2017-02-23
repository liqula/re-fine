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

module Refine.Common.VDoc.HTML.Canonicalize
  ( canonicalizeVDocVersion, reCanonicalizeVDocVersion, downgradeRawVDocVersion
  , canonicalizeWhitespace
  , setElemUIDs, nextFreeElemUID
  , wrapInTopLevelTags
  , canonicalizeAttrsForest, canonicalizeAttrsTree, canonicalizeAttrsStream, canonicalizeAttrsToken, canonicalizeAttrs
  ) where

import           Data.Char (isSpace)
import           Data.Function (on)
import           Data.Functor.Infix ((<$$>))
import           Data.List as List (foldl', sort, nubBy)
import           Data.Maybe (catMaybes)
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..), canonicalizeTokens)
import           Text.HTML.Tree (tokensFromForest, tokensToForest, nonClosing)

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core (dataUidOfToken)


-- | Does several things.
--
-- 1. Canonicalize whitespace and merge neighboring text tokens.  (See test suite for details.)
-- 2. Refresh @data-uid@ attributes in all tags.  Keep in mind that @data-uid@ are only meaningful
--    in the scope of their (immutable) @VDocVersion@.  Values may change for unchanging nodes
--    between versions.
-- 3. wrap all top-level text nodes into @<span>@ tags (so 'ChunkPoint' always has a @data-uid@
--    value to point to.
canonicalizeVDocVersion :: VDocVersion 'HTMLRaw -> VDocVersion 'HTMLCanonical
canonicalizeVDocVersion (VDocVersion vers) = VDocVersion (canonicalizeTokenForest vers)

canonicalizeTokenForest :: Forest Token -> Forest Token
canonicalizeTokenForest
      = canonicalizeAttrsForest
      . onStream setElemUIDs
      . wrapInTopLevelTags
      . (onText canonicalizeWhitespace . fixSelfClosing <$$>)
      . onStream (canonicalizeTokens . dropCommentsAndDoctypes)
  where
    -- | FUTUREWORK: it would be nice to avoid the detour over a list, and run all transformations
    -- on the tree.  in particular, it would be nice if we didn't have to do the detour twice
    -- (because transformations have to be called in this order).
    onStream :: ([Token] -> [Token]) -> Forest Token -> Forest Token
    onStream go forest = case tokensToForest . go . tokensFromForest $ forest of
      Right forest' -> forest'
      Left err      -> error $ "canonicalizeVDocVersion: impossible: " <> show err


-- | De-canonicalizing is always safe, since every canonicalized 'VDocVersion' is also a raw one.
downgradeRawVDocVersion :: VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLRaw
downgradeRawVDocVersion (VDocVersion s) = VDocVersion s

-- | Canonicalization is always safe (see 'downgradeRawVDocVersion') and also idempotent.
reCanonicalizeVDocVersion :: VDocVersion a -> VDocVersion a
reCanonicalizeVDocVersion (VDocVersion vers) = VDocVersion (canonicalizeTokenForest vers)


dropCommentsAndDoctypes :: [Token] -> [Token]
dropCommentsAndDoctypes = Prelude.filter $ \case
  (Text.HTML.Parser.Comment _) -> False
  (Text.HTML.Parser.Doctype _) -> False
  _                            -> True

-- 'nonClosing' tags are parsed as 'TagOpen' if they occur in the (unfortunately legal) form @<br>@.
-- this function transforms them here into the equally legal form @<br/>@.
fixSelfClosing :: Token -> Token
fixSelfClosing (TagOpen n attrs) | n `elem` nonClosing = TagSelfClose n attrs
fixSelfClosing t = t

onText :: (ST -> ST) -> Token -> Token
onText f (ContentText s) = ContentText $ f s
onText _ t = t

canonicalizeWhitespace :: ST -> ST
canonicalizeWhitespace t = leading t <> ST.intercalate "\n" (ST.words t) <> trailing t
  where
    leading s = if ST.all isSpace (ST.take 1 s) then "\n" else ""
    trailing = leading . ST.reverse


-- | Remove all existing @data-uid@ attributes and add new attributes with fresh unique values to
-- all tags.
setElemUIDs :: [Token] ->  [Token]
setElemUIDs ts = reverse . snd $ foldl' fill (nextFreeElemUID ts, []) ts
  where
    fill :: (DataUID, [Token]) -> Token -> (DataUID, [Token])
    fill (next, acc) = \case
      (TagOpen      n attrs) | n /= "mark" -> (next + 1, TagOpen      n (addNext next attrs) : acc)
      (TagSelfClose n attrs)               -> (next + 1, TagSelfClose n (addNext next attrs) : acc)
      t                                    -> (next,     t                                   : acc)

    addNext :: DataUID -> [Attr] -> [Attr]
    addNext next attrs = canonicalizeAttrs $ attrs <> [Attr "data-uid" (cs $ show next)]

nextFreeElemUID :: [Token] -> DataUID
nextFreeElemUID = (+1) . maximum . (0:) . catMaybes . fmap dataUidOfToken

wrapInTopLevelTags :: Forest Token -> Forest Token
wrapInTopLevelTags = fmap fill
  where
    defaultTag :: Token
    defaultTag = TagOpen "span" []

    fill :: Tree Token -> Tree Token
    fill n@(Node (ContentText _) _) = Node defaultTag [n]
    fill n                          = n


-- | Call 'canonicalizeAttrs' on a token 'Forest'.
canonicalizeAttrsForest :: Forest Token -> Forest Token
canonicalizeAttrsForest = fmap canonicalizeAttrsTree

-- | Call 'canonicalizeAttrs' on a token 'Tree'.
canonicalizeAttrsTree :: Tree Token -> Tree Token
canonicalizeAttrsTree (Node x xs) = Node (canonicalizeAttrsToken x) (canonicalizeAttrsForest xs)

-- | Call 'canonicalizeAttrsToken' on a token list.
canonicalizeAttrsStream :: [Token] -> [Token]
canonicalizeAttrsStream = fmap canonicalizeAttrsToken

-- | Call 'canonicalizeAttrs' on attribute list.
canonicalizeAttrsToken :: Token -> Token
canonicalizeAttrsToken (TagOpen n xs) = TagOpen n $ canonicalizeAttrs xs
canonicalizeAttrsToken (TagSelfClose n xs) = TagSelfClose n $ canonicalizeAttrs xs
canonicalizeAttrsToken t = t

-- | If one attribute key occurs several times, use the first occurrance and drop the rest.  Sort
-- the resulting list.
canonicalizeAttrs :: [Attr] -> [Attr]
canonicalizeAttrs = sort . nubBy ((==) `on` (\(Attr k _) -> k))
