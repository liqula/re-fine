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

module Refine.Common.VDoc.HTML.Splice
  ( insertMarks, insertMoreMarks
  , chunkRangeCanBeApplied
  , PreToken(..)
  , enablePreTokens
  , resolvePreTokens
  , runPreToken
  , splitAtOffset
  , preTokensFromForest
  , preTokensToForest
  ) where

import           Control.Exception (assert)
import           Control.Lens (Traversal', (&), (^.), (%~))
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad (foldM)
import           Data.Functor.Infix ((<$$>))
import           Data.Maybe (catMaybes)
import           Data.String.Conversions ((<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Data.Typeable (Typeable, typeOf)
import           Text.HTML.Parser (Token(..), parseTokens, canonicalizeTokens, renderTokens)
import           Text.HTML.Tree (tokensToForest, nonClosing)
import           Web.HttpApiData (toUrlPiece)

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core
import Refine.Prelude


-- * module interface

-- | Render 'VDocVersion' as needed in the browser.  More specifically: Insert @mark@ html elements
-- for all chunks of all edits, comments, notes, etc.
insertMarks :: (Typeable a, MonadError VDocHTMLError m)
            => [ChunkRange a] -> VDocVersion 'HTMLCanonical -> m (VDocVersion 'HTMLWithMarks)
insertMarks crs (VDocVersion (parseTokens -> (ts :: [Token])))
  = assert (ts == canonicalizeTokens ts)
  . assert (all (`chunkRangeCanBeAppliedTs` ts) crs)
  . fmap (VDocVersion . cs . renderTokens)  -- TODO: do we still want '\n' between tokens for darcs?
  . insertMarksTs crs
  $ ts

-- Calls 'insertMarks', but expects the input 'VDocVersion' to have passed through before.
insertMoreMarks :: (Typeable a, MonadError VDocHTMLError m)
                => [ChunkRange a] -> VDocVersion 'HTMLWithMarks -> m (VDocVersion 'HTMLWithMarks)
insertMoreMarks crs (VDocVersion vers) = insertMarks crs (VDocVersion vers)


-- * sanity check

chunkRangeCanBeApplied :: ChunkRange a -> VDocVersion b -> Bool
chunkRangeCanBeApplied crs (VDocVersion (parseTokens -> (ts :: [Token]))) = chunkRangeCanBeAppliedTs crs ts

chunkRangeCanBeAppliedTs :: ChunkRange a -> [Token] -> Bool
chunkRangeCanBeAppliedTs (ChunkRange _ mp1 mp2) ts = all (`chunkPointCanBeAppliedTs` ts) $ catMaybes [mp1, mp2]

chunkPointCanBeAppliedTs :: ChunkPoint -> [Token] -> Bool
chunkPointCanBeAppliedTs (ChunkPoint uid off) ts = case tokensToForest ts of
  Right forest ->
    let sub = forest ^. atNode (\p -> dataUidOfToken p == Just uid)
    in not (null sub) && forestTextLength sub >= off
  Left _ -> False


-- * inserting marks

insertMarksTs :: (Typeable a, MonadError VDocHTMLError m)
              => [ChunkRange a] -> [Token] -> m [Token]
insertMarksTs crs ts = do
  pf <- monadError VDocHTMLErrorBadTree (tokensToForest ts)
  pfm <- insertMarksF crs (enablePreTokens pf)
  monadError VDocHTMLErrorInternal (resolvePreTokens $ preTokensFromForest pfm)


insertMarksF :: forall m a . Typeable a => MonadError VDocHTMLError m
             => [ChunkRange a] -> Forest PreToken -> m (Forest PreToken)
insertMarksF crs = (`woodZip` splitup crs)
  where
    -- turn chunk ranges into chunk points.
    splitup :: [ChunkRange a] -> [(ChunkPoint, PreToken)]
    splitup = mconcat . fmap f
      where
        t = cs . show . typeOf $ (undefined :: a)
        f (ChunkRange (toUrlPiece -> l) mb me) =
            catMaybes [(, PreMarkOpen l t) <$> mb, (, PreMarkClose l)  <$> me]

    -- FUTUREWORK: there is a faster implementation for @woodZip@: if chunks come in the correct
    -- order, we only need to traverse the forest once.  that's why it's called "zip".
    --
    -- FUTUREWORK: it's weird that we have to 'view' into the forest and then 'over' into it *after*
    -- the 'view' has proven there is sufficient text.  with more lens-foo, i bet we could write
    -- something that looks like the 'Right' branch here, but lets 'insertPreToken' return an
    -- 'Either' that is somehow propagated through '(%~)' and out of 'woodZip'.
    woodZip :: Forest PreToken -> [(ChunkPoint, PreToken)] -> m (Forest PreToken)
    woodZip = foldM switch

    switch :: Forest PreToken -> (ChunkPoint, PreToken) -> m (Forest PreToken)
    switch f (cp@(ChunkPoint nod off), mark)
      = if preForestTextLength (f ^. atDataUID nod) >= off
          then pure $ f & atDataUID nod %~ insertPreToken (show cp) off mark
          else throwError $ VDocHTMLErrorBadChunkPoint f cp
      where
        atDataUID :: DataUID -> Traversal' (Forest PreToken) (Forest PreToken)
        atDataUID node = atNode (\p -> dataUidOfPreToken p == Just node)


-- | In a list of text or premark tokens, add another premark token at a given offset, splitting up
-- an existing text if necessary.
--
-- FUTUREWORK: we could make more assumptions here on the structure of the tree: input is
-- canonicalized and all tags have data-uid, so the forest can only contain a single ContentText
-- node and no deep nodes.
insertPreToken :: String -> Int -> PreToken -> Forest PreToken -> Forest PreToken
insertPreToken errinfo offset mark forest = assert (isPreMark mark) $ either err id go
  where
    go :: MonadError VDocHTMLError m => m (Forest PreToken)
    go = do
      (ts, ts') <- splitAtOffset offset $ preTokensFromForest forest
      preTokensToForest $ ts <> [mark] <> ts'

    -- If 'ChunkRange' does not apply to tree (e.g. because of too large offset), this throws an
    -- internal error.  This is impossible as long as 'insertMarks' checks 'chunkCanBeApplied' on
    -- all chunks.
    err e = error $ "internal error: insertPreToken: " <> show (e, errinfo, mark, forest)


-- | Split a token stream into one that has a given number of characters @n@ in its text nodes, and
-- the rest.  Splits up a text node into two text nodes if necessary.  Tags that have no length
-- (e.g. comments) go to the left.  Stops scanning on any non-flat token (definition of non-flat is
-- in the source), and either returns the resulting split or an error.
splitAtOffset :: MonadError VDocHTMLError m => Int -> [PreToken] -> m ([PreToken], [PreToken])
splitAtOffset offset ts_ = assert (offset >= 0)
                         . assert ((runPreToken <$> ts_) == canonicalizeTokens (runPreToken <$> ts_))
                         . either throwError pure
                         $ recursion consumeToken (offset, [], ts_)
  where
    consumeToken :: (Int, [PreToken], [PreToken])
                 -> Recursion (Int, [PreToken], [PreToken])
                              VDocHTMLError
                              ([PreToken], [PreToken])
    consumeToken (n, ps, ts)
        = if n > 0
            then hungry n ps ts
            else nothungry ps ts

    nothungry ps []
        = Halt (reverse ps, [])

    nothungry ps ts@(t : ts')
        = if preTokenTextLength t == 0 && isFlatToken t
            then Run (0, t : ps, ts')
            else Halt (reverse ps, ts)

    hungry _ _ []
        = Fail $ VDocHTMLErrorNotEnouchCharsToSplit offset ts_

    hungry n ps (t : ts')
        | isFlatToken t
            = let n' = n - preTokenTextLength t
              in if n' >= 0
                  then Run (n', t : ps, ts')
                  else case splitTokenText n t of
                      (p, s) -> Run (0, p : ps, s : ts')
        | otherwise
            = Fail $ VDocHTMLErrorNotEnouchCharsToSplit offset ts_

    splitTokenText :: Int -> PreToken -> (PreToken, PreToken)
    splitTokenText n = \case
      (PreToken (ContentText (ST.splitAt n -> (s, s'))))
        -> (PreToken $ ContentText s, PreToken $ ContentText s')
      bad
        -> error $ "splitTokenText: bad input " <> show (n, bad)

    isFlatToken :: PreToken -> Bool
    isFlatToken = \case
      (PreToken (TagOpen n _)) -> n `notElem` nonClosing
      (PreToken (TagClose _))  -> False
      (PreMarkOpen _ _)        -> True  -- TODO: why?
      (PreMarkClose _)         -> True  -- TODO: why?
      _                        -> True


-- * pretokens

isOpen :: PreToken -> Bool
isOpen (PreMarkOpen _ _)        = True
isOpen (PreToken (TagOpen n _)) = n `notElem` nonClosing
isOpen _                        = False

isClose :: PreToken -> Bool
isClose (PreMarkClose _)        = True
isClose (PreToken (TagClose _)) = True
isClose _                       = False

isPreMark :: PreToken -> Bool
isPreMark (PreMarkOpen _ _) = True
isPreMark (PreMarkClose _)  = True
isPreMark (PreToken _)      = False

isMatchingOpenClose :: PreToken -> PreToken -> Bool
isMatchingOpenClose (PreMarkOpen l _)        (PreMarkClose l')        | l == l' = True
isMatchingOpenClose (PreToken (TagOpen n _)) (PreToken (TagClose n')) | n == n' = True
isMatchingOpenClose _                        _                                  = False

closeFromOpen :: PreToken -> PreToken
closeFromOpen (PreMarkOpen l _)         = PreMarkClose l
closeFromOpen (PreToken (TagOpen el _)) = PreToken (TagClose el)
closeFromOpen _                         = error "closeFromOpen: internal error."


-- * translating between pretokens and tokens

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
    filterEmptyChunks (PreMarkOpen _ _ : PreMarkClose _ : xs) = filterEmptyChunks xs
    filterEmptyChunks (x : xs)                                = x : filterEmptyChunks xs
    filterEmptyChunks []                                      = []
