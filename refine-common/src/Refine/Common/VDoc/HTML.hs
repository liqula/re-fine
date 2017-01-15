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

  -- * internal (exported for testing)
  , PreToken(..), enablePreTokens, resolvePreTokens, runPreToken
  ) where

import           Control.Exception (assert)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Data.Char (isSpace)
import           Data.Functor.Infix ((<$$>))
import           Data.List as List (foldl')
import           Data.Set as Set hiding (foldl')
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..), parseTokens, canonicalizeTokens, renderTokens)
import           Text.HTML.Tree (ParseTokenForestError, tokensToForest, tokensFromForest)

import Refine.Common.Types
import Refine.Prelude


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


-- | this type is introduced because we keep open and close marks separately in the tree at some
-- point.  in order to keep track of which close mark belongs to which open mark, we cannot rely on
-- their order (which still needs to be de-overlapped), so need close marks of the form
-- @PreMarkClose "data-chunk-id-value"@.
data PreToken = PreToken Token | PreMarkOpen ST | PreMarkClose ST
  deriving (Eq, Show)

runPreToken :: PreToken -> Token
runPreToken (PreToken t)     = t
runPreToken (PreMarkOpen l)  = TagOpen "mark" [Attr "data-chunk-id" l]
runPreToken (PreMarkClose _) = TagClose "mark"

isOpen :: PreToken -> Bool
isOpen (PreMarkOpen _)          = True
isOpen (PreToken (TagOpen _ _)) = True
isOpen _                        = False

isClose :: PreToken -> Bool
isClose (PreMarkClose _)        = True
isClose (PreToken (TagClose _)) = True
isClose _                       = False

isMatchingOpenClose :: PreToken -> PreToken -> Bool
isMatchingOpenClose (PreMarkOpen l)          (PreMarkClose l')        | l == l' = True
isMatchingOpenClose (PreToken (TagOpen n _)) (PreToken (TagClose n')) | n == n' = True
isMatchingOpenClose _                        _                                  = False

closeFromOpen :: PreToken -> PreToken
closeFromOpen (PreMarkOpen l)           = PreMarkClose l
closeFromOpen (PreToken (TagOpen el _)) = PreToken (TagClose el)
closeFromOpen _                         = error "closeFromOpen: internal error."


enablePreTokens :: Forest Token -> Forest PreToken
enablePreTokens ys = f <$> ys
  where f (Node x xs) = Node (PreToken x) (f <$> xs)


data ResolvePreTokensStack =
    ResolvePreTokensStack
      { _rptsOpen    :: [PreToken]
      , _rptsReopen  :: [PreToken]
      , _rptsWritten :: [PreToken]
      , _rptsReading :: [PreToken]
      }
  deriving (Eq, Show)

-- | If there are any pre-marks left, open and close them as often as needed to resolve overlaps.
-- Crashes if the opening and closing premarks do not match up.
resolvePreTokens :: forall m . MonadError String m => [PreToken] -> m [Token]
resolvePreTokens ts_ = runPreToken <$$> (filterEmptyChunks <$> go)
  where
    go :: m [PreToken]
    go = either throwError pure
       . recursion f
       $ ResolvePreTokensStack [] [] [] ts_

    f :: ResolvePreTokensStack -> Recursion ResolvePreTokensStack String [PreToken]
    f (ResolvePreTokensStack [] [] written []) = Halt $ reverse written

    -- (a) handle closing tags
    f (ResolvePreTokensStack (opening : openings') reopenings written ts@(t : ts'))
        | isClose t && isMatchingOpenClose opening t
            = Run $ ResolvePreTokensStack openings' reopenings (t : written) ts'
        | isClose t
            = Run $ ResolvePreTokensStack openings' (opening : reopenings) (closeFromOpen opening : written) ts

    f (ResolvePreTokensStack [] _ _ ts@(t : _))
        | isClose t
            = Fail $ "resolvePreTokens: close without open: " <> show (ts, ts_)

    -- (b) handle opening tags
    f (ResolvePreTokensStack openings reopenings written (t : ts'))
        | isOpen t
            = Run $ ResolvePreTokensStack (t : openings) reopenings (t : written) ts'

    -- (c) *after* having handled closing tags, re-open tags artificially closed in (a)
    f (ResolvePreTokensStack openings reopenings@(_:_) written ts)
            = Run $ ResolvePreTokensStack openings [] written (reverse reopenings <> ts)

    -- (d) simply run and output any other tags
    f (ResolvePreTokensStack openings [] written (t : ts'))
            = Run $ ResolvePreTokensStack openings [] (t : written) ts'

    f (ResolvePreTokensStack openings@(_:_) [] _ [])
            = Fail $ "resolvePreTokens: open without close: " <> show (openings, ts_)

filterEmptyChunks :: [PreToken] -> [PreToken]
filterEmptyChunks (PreMarkOpen _ : PreMarkClose _ : xs) = filterEmptyChunks xs
filterEmptyChunks (x : xs)                              = x : filterEmptyChunks xs
filterEmptyChunks []                                    = []


insertMarksTs :: MonadError VDocHTMLError m
            => [ChunkRange a] -> [Token] -> m [Token]
insertMarksTs = undefined
