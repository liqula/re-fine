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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.VDoc.HTML.Canonicalize
  ( canonicalizeVDocVersion
  , decanonicalizeVDocVersion
  , canonicalizeWhitespace
  , setElemUIDs
  , wrapInTopLevelTags
  , canonicalizeAttrsForest, canonicalizeAttrsTree, canonicalizeAttrsStream, canonicalizeAttrs
  ) where

import           Control.Exception (assert)
import           Control.Monad.Error.Class (MonadError)
import           Data.Char (isSpace)
import           Data.Function (on)
import           Data.List as List (foldl', sort, nubBy)
import           Data.Set as Set hiding (foldl')
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..), parseTokens, canonicalizeTokens, renderTokens)
import           Text.HTML.Tree (tokensFromForest, nonClosing)

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core


-- | Does several things.
--
-- 1. Canonicalize whitespace and merge neighboring text tokens.  (See test suite for details.)
-- 2. Refresh @data-uid@ attributes in all tags.  Keep in mind that @data-uid@ are only meaningful
--    in the scope of their (immutable) @VDocVersion@.  Values may change for unchanging nodes
--    between versions.
-- 3. wrap all top-level text nodes into @<span>@ tags (so 'ChunkPoint' always has a @data-uid@
--    value to point to.
canonicalizeVDocVersion :: MonadError VDocHTMLError m
                        => VDocVersion 'HTMLRaw -> m (VDocVersion 'HTMLCanonical)
canonicalizeVDocVersion = fmap (VDocVersion . cs . renderTokens) . canonicalize . parseTokens . _unVDocVersion
  where
    canonicalize
      = fmap setElemUIDs
      . wrapInTopLevelTags
      . fmap (onText canonicalizeWhitespace)
      . fmap fixSelfClosing
      . canonicalizeTokens
      . dropCommentsAndDoctypes

-- | De-canonicalizing is always safe, since every canonicalized 'VDocVersion' is also a raw one.
decanonicalizeVDocVersion :: VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLRaw
decanonicalizeVDocVersion (VDocVersion s) = VDocVersion s


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
setElemUIDs = fill mempty . fmap clear
  where
    clear :: Token -> Token
    clear (TagOpen      n attrs) = TagOpen      n (Prelude.filter stale attrs)
    clear (TagSelfClose n attrs) = TagSelfClose n (Prelude.filter stale attrs)
    clear t                      = t

    stale :: Attr -> Bool
    stale (Attr "data-uid" _) = False
    stale _                   = True

    fill :: Set DataUID -> [Token] -> [Token]
    fill already = reverse . snd . foldl' f (totalMaximum already, [])
      where
        f :: (DataUID, [Token]) -> Token -> (DataUID, [Token])
        f (next, acc) = \case
          (TagOpen      n attrs) -> (nextDataUID next, TagOpen      n (Attr "data-uid" (cs $ show next) : attrs) : acc)
          (TagSelfClose n attrs) -> (nextDataUID next, TagSelfClose n (Attr "data-uid" (cs $ show next) : attrs) : acc)
          t                      -> (next,             t : acc)

        totalMaximum s = if Set.null s then DataUID 1 else nextDataUID (Set.findMax s)
        nextDataUID (DataUID n) = DataUID (n + 1)


wrapInTopLevelTags :: MonadError VDocHTMLError m => [Token] -> m [Token]
wrapInTopLevelTags ts = assert (ts == canonicalizeTokens ts)
                      $ tokensFromForest . fmap fill <$> tokensToForest' ts
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
canonicalizeAttrsTree (Node x xs) = Node (canonicalizeAttrs x) (canonicalizeAttrsForest xs)

-- | Call 'canonicalizeAttrs' on a token list.
canonicalizeAttrsStream :: [Token] -> [Token]
canonicalizeAttrsStream = fmap canonicalizeAttrs

-- | If one attribute key occurs several times, use the first occurrance and drop the rest.  Sort
-- the resulting list.
canonicalizeAttrs :: Token -> Token
canonicalizeAttrs (TagOpen n xs) = TagOpen n (sort $ nubBy ((==) `on` (\(Attr k _) -> k)) xs)
canonicalizeAttrs t = t
