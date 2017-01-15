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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Code for handling `VDocVersions` with HTML content.
--
-- TODO: think of a better module name / place?
module Refine.Common.VDoc.HTML
  ( VDocHTMLError(..)
  , canonicalizeVDocVersion
  , decanonicalizeVDocVersion
  , canonicalizeWhitespace
  , setElemUIDs
  , wrapInTopLevelTags
  , insertMarks
  ) where

import           Control.Exception (assert)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Data.Char (isSpace)
import           Data.List as List (foldl')
import           Data.Set as Set hiding (foldl')
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..), parseTokens, canonicalizeTokens, renderTokens)
import           Text.HTML.Tree (ParseTokenForestError, tokensToForest, tokensFromForest)

import Refine.Common.Types


newtype VDocHTMLError =
    VDocHTMLErrorBadTree ParseTokenForestError
  deriving (Eq, Show)


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
      = wrapInTopLevelTags
      . setElemUIDs
      . fmap (onText canonicalizeWhitespace) . canonicalizeTokens

-- | De-canonicalizing is always safe, since every canonicalized 'VDocVersion' is also a raw one.
decanonicalizeVDocVersion :: VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLRaw
decanonicalizeVDocVersion (VDocVersion s) = VDocVersion s

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
    clear (TagOpen n xs) = TagOpen n (Prelude.filter stale xs)
    clear t              = t

    stale :: Attr -> Bool
    stale (Attr "data-uid" _) = False
    stale _                   = True

    fill :: Set DataUID -> [Token] -> [Token]
    fill already = reverse . snd . foldl' f (totalMaximum already, [])
      where
        f :: (DataUID, [Token]) -> Token -> (DataUID, [Token])
        f (next, acc) = \case
          (TagOpen n attrs) -> (nextDataUID next, TagOpen n (Attr "data-uid" (cs $ show next) : attrs) : acc)
          t                 -> (next,             t : acc)

        totalMaximum s = if Set.null s then DataUID 1 else nextDataUID (Set.findMax s)
        nextDataUID (DataUID n) = DataUID (n + 1)

wrapInTopLevelTags :: MonadError VDocHTMLError m => [Token] -> m [Token]
wrapInTopLevelTags ts = assert (ts == canonicalizeTokens ts)
                      $ case tokensToForest ts of
    Right forest -> pure . tokensFromForest $ fill <$> forest
    Left err     -> throwError . VDocHTMLErrorBadTree $ err
  where
    defaultTag :: Token
    defaultTag = TagOpen "span" []

    fill :: Tree Token -> Tree Token
    fill n@(Node (ContentText _) _) = Node defaultTag [n]
    fill n                          = n


-- | Render 'VDocVersion' as needed in the browser.  More specifically: Insert @mark@ html elements
-- for all chunks of all patches, comments, notes, etc.
insertMarks :: MonadError VDocHTMLError m
            => [ChunkRange a] -> VDocVersion 'HTMLCanonical -> m (VDocVersion 'HTMLWithMarks)
insertMarks crs (VDocVersion (parseTokens -> ts))
  = assert (ts == canonicalizeTokens ts)
  . fmap (VDocVersion . cs . renderTokens)  -- TODO: do we still want '\n' between tokens for darcs?
  . insertMarksTs crs
  $ ts

insertMarksTs :: MonadError VDocHTMLError m
            => [ChunkRange a] -> [Token] -> m [Token]
insertMarksTs = undefined
