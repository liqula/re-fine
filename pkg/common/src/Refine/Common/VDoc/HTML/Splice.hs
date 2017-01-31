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
  , ChunkRangeError(..), createChunkRangeErrors
  , enablePreTokens
  , resolvePreTokens
  , splitAtOffset
  , preTokensFromForest
  , preTokensToForest
  ) where

import           Control.Exception (assert)
import           Control.Lens ((&), (^.), (%~))
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad (unless, foldM)
import           Data.Functor.Infix ((<$$>))
import           Data.List (find)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Data.Typeable (Typeable, typeOf)
import           Text.HTML.Parser (Token(..), canonicalizeTokens)
import           Text.HTML.Tree (tokensFromForest, tokensToForest)
import           Web.HttpApiData (toUrlPiece)

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Canonicalize
import Refine.Prelude


-- * module interface

-- | Render 'VDocVersion' as needed in the browser.  More specifically: Insert @mark@ html elements
-- for all chunks of all edits, comments, notes, etc.
--
-- TODO: do we still want '\n' between tokens for darcs?
insertMarks :: (Typeable a, MonadError VDocHTMLError m)
            => [ChunkRange a] -> VDocVersion 'HTMLCanonical -> m (VDocVersion 'HTMLWithMarks)
insertMarks crs vers@(VDocVersion forest) = do
    invariants (tokensFromForest forest)
    withPreTokens <- insertMarksForest crs $ enablePreTokens forest
    afterRunPreTokens <- monadError VDocHTMLErrorInternal . resolvePreTokens . preTokensFromForest $ withPreTokens
    forest' <- monadError (VDocHTMLErrorInternal . show) $ tokensToForest afterRunPreTokens
    let VDocVersion forest'' = canonicalizeVDocVersion $ VDocVersion forest'
    pure $ VDocVersion forest''
  where
    -- FIXME: these invariants should all be caught earlier than here.  remove the checks once we've
    -- established they are.
    invariants ts = canonicalized ts >> validchunks

    canonicalized ts = unless (ts == canonicalizeTokens ts) . throwError . VDocHTMLErrorInternal $
        "insertMarks: non-canonical input: " <> show ts

    validchunks = unless (null errs) . throwError . VDocHTMLErrorInternal $
        "insertMarks: invalid chunk ranges: " <> show errs
      where
        errs = mconcat $ (`chunkRangeErrors` vers) <$> crs
        chunkRangeErrors (ChunkRange _ mp1 mp2) = createChunkRangeErrors $ CreateChunkRange mp1 mp2


-- Calls 'insertMarks', but expects the input 'VDocVersion' to have passed through before.
insertMoreMarks :: (Typeable a, MonadError VDocHTMLError m)
                => [ChunkRange a] -> VDocVersion 'HTMLWithMarks -> m (VDocVersion 'HTMLWithMarks)
insertMoreMarks crs (VDocVersion vers) = insertMarks crs (VDocVersion vers)


-- * sanity check

-- | Returns 'True' iff (a) both chunk points are either Nothing or hit into an existing point in
-- the tree (i.e. have existing @data-uid@ attributes and offsets no larger than the text length);
-- (b) the begin chunk point is left of the end, and the text between them is non-empty.
createChunkRangeErrors :: CreateChunkRange -> VDocVersion b -> [ChunkRangeError]
createChunkRangeErrors (CreateChunkRange mp1 mp2) vers@(VDocVersion forest) = rangeNonEmpty <> pointsHit
  where
    ts = tokensFromForest forest

    pointsHit :: [ChunkRangeError]
    pointsHit = mconcat $ (`chunkPointErrors` vers) <$> catMaybes [mp1, mp2]

    rangeNonEmpty :: [ChunkRangeError]
    rangeNonEmpty = case (mp1, mp2) of
      (Nothing, _) -> []
      (_, Nothing) -> []
      (Just (ChunkPoint b boff), Just (ChunkPoint e eoff))
        -> let -- start keeping tokens from the open tag with the opening data-uid
               ts' = dropWhile ((/= Just b) . dataUidOfToken) ts

               -- stop keeping tokens after the closing tag at the end of the sub-tree with the closing data-uid.
               mts'' = case find ((== Just e) . dataUidOfToken . snd) (zip [0..] ts') of
                   Nothing -> Nothing
                   Just (treeStartIx, TagOpen n _) ->
                     let adhocParser :: [ST] -> [Token] -> [Token]
                         adhocParser (n_:ns') (t@(TagClose n')   : ts_) | n' == n_ = t : adhocParser ns'       ts_
                         adhocParser ns       (t@(TagOpen  n' _) : ts_)            = t : adhocParser (n' : ns) ts_
                         adhocParser ns       (t                 : ts_)            = t : adhocParser ns        ts_
                         adhocParser []       []                                   = []
                         adhocParser ns@(_:_) [] = error $ "chunkRangeErrorTs: bad tree: " <> show ns

                         chunkLength = treeStartIx + length (adhocParser [n] (drop (treeStartIx + 1) ts'))
                     in Just $ take chunkLength ts'

                   bad -> error $ "chunkRangeErrorTs: non-tag end node: " <> show bad

           in case mts'' of
               Just ts'' -> if sum (tokenTextLength <$> ts'') - boff > 0 && (b /= e || boff <= eoff)
                 then []
                 else [ChunkRangeEmpty ts mp1 mp2]
               Nothing -> [ChunkRangeBadEndNode ts mp1 mp2]

chunkPointErrors :: ChunkPoint -> VDocVersion b -> [ChunkRangeError]
chunkPointErrors cp@(ChunkPoint uid off) (VDocVersion forest) =
    if forestTextLength sub >= off
      then []
      else [ChunkRangeOffsetTooLarge sub cp]
  where
    sub = forest ^. atNode (\p -> dataUidOfToken p == Just uid)


-- * inserting marks

insertMarksForest :: forall m a . Typeable a => MonadError VDocHTMLError m
             => [ChunkRange a] -> Forest PreToken -> m (Forest PreToken)
insertMarksForest crs = (`woodZip` splitup crs)
  where
    -- turn chunk ranges into chunk points.
    splitup :: [ChunkRange a] -> [(Maybe ChunkPoint, PreToken)]
    splitup = mconcat . fmap f
      where
        t = cs . show . typeOf $ (undefined :: a)
        f (ChunkRange (toUrlPiece -> l) mb me) = [(mb, PreMarkOpen l t), (me, PreMarkClose l)]

    -- FUTUREWORK: there is a faster implementation for @woodZip@: if chunks come in the correct
    -- order, we only need to traverse the forest once.  that's why it's called "zip".
    --
    -- FUTUREWORK: it's weird that we have to 'view' into the forest and then 'over' into it *after*
    -- the 'view' has proven there is sufficient text.  with more lens-foo, i bet we could write
    -- something that looks like the 'Right' branch here, but lets 'insertPreToken' return an
    -- 'Either' that is somehow propagated through '(%~)' and out of 'woodZip'.
    woodZip :: Forest PreToken -> [(Maybe ChunkPoint, PreToken)] -> m (Forest PreToken)
    woodZip = foldM switch

    switch :: Forest PreToken -> (Maybe ChunkPoint, PreToken) -> m (Forest PreToken)
    switch f (Nothing, mark@(PreMarkOpen _ _)) = pure $ Node mark [] : f
    switch f (Nothing, mark@(PreMarkClose _))  = pure $ f <> [Node mark []]
    switch f (Just cp@(ChunkPoint nod off), mark)
      = pure $ f & atPreToken nod %~ insertPreToken (show cp) off mark
    switch f bad = error $ "insertMarksF: " <> show (f, bad)

-- | In a list of text or premark tokens, add another premark token at a given offset, splitting up
-- an existing text if necessary. The output is not canonicalized (there may be empty text nodes).
insertPreToken :: String -> Int -> PreToken -> Forest PreToken -> Forest PreToken
insertPreToken errinfo offset mark forest = assert (isPreMark mark) $ either err id go
  where
    go :: MonadError VDocHTMLError m => m (Forest PreToken)
    go = do
      (ts, ts') <- splitAtOffset offset forest
      pure $ ts <> [Node mark []] <> ts'

    -- If 'ChunkRange' does not apply to tree (e.g. because of too large offset), this throws an
    -- internal error.  This is impossible as long as 'insertMarks' only ever sees chunk ranges that
    -- have been validated by 'createChunkRangeErrors'.
    err e = error $ "internal error: insertPreToken: " <> show (e, errinfo, mark, forest)

    isPreMark :: PreToken -> Bool
    isPreMark (PreMarkOpen _ _) = True
    isPreMark (PreMarkClose _)  = True
    isPreMark (PreToken _)      = False


-- | Split a token stream into one that has a given number of characters @n@ in its text nodes, and
-- the rest.  Splits up a text node into two text nodes if necessary.
splitAtOffset :: MonadError VDocHTMLError m => Int -> Forest PreToken -> m (Forest PreToken, Forest PreToken)
splitAtOffset offset ts_
    = assert (offset >= 0)
    $ recursion consumeToken (offset, [], ts_)
  where
    consumeToken :: (Int, Forest PreToken, Forest PreToken)
                 -> Recursion (Int, Forest PreToken, Forest PreToken)
                              VDocHTMLError
                              (Forest PreToken, Forest PreToken)

    consumeToken (n, ps, t@(Node (PreToken (ContentText s)) []) : ts')
        = let n' = n - ST.length s
          in if n' >= 0
              then Run (n', t : ps, ts')
              else case ST.splitAt n s of
                (s', s'') ->
                  let pack = (`Node` []) . PreToken . ContentText
                  in Halt ( reverse $ pack s' : ps
                          ,           pack s'' : ts'
                          )

    consumeToken (n, ps, t@(Node _ _) : ts')
        = let n' = n - preForestTextLength [t]
          in if n' >= 0
              then Run (n', t : ps, ts')
              else Fail $ VDocHTMLErrorSplitPointsToSubtree offset ts_

    consumeToken (n, ps, [])
        = if n == 0
            then Halt (reverse ps, [])
            else Fail $ VDocHTMLErrorNotEnoughCharsToSplit offset ts_


-- * translating between pretokens and tokens

enablePreTokens :: Forest Token -> Forest PreToken
enablePreTokens ys = f <$> ys
  where f (Node x xs) = Node (PreToken x) (f <$> xs)


data ResolvePreTokensStack =
    ResolvePreTokensStack
      { _rptsOpen    :: Set PreToken'
      , _rptsWritten :: [PreToken]
      , _rptsReading :: [PreToken]
      }
  deriving (Eq, Show)

-- | This is really just PreMarkOpen, but we want the helper functions to be total, so we scrap the
-- constructor.
type PreToken' = (ST, ST)


-- | Traverse a 'PreToken' and wrap all 'PreMarkOpen' and 'PreMarkClose' directly around the
-- affected text nodes.  Fails if the opening and closing premarks do not match up.
resolvePreTokens :: forall m . MonadError String m => [PreToken] -> m [Token]
resolvePreTokens ts_ = runPreToken <$$> go
  where
    go :: m [PreToken]
    go = recursion f $ ResolvePreTokensStack mempty [] ts_

    f :: ResolvePreTokensStack -> Recursion ResolvePreTokensStack String [PreToken]
    f (ResolvePreTokensStack opens written (t@(PreToken (ContentText _)) : ts')) =
        Run $ ResolvePreTokensStack opens (wrap opens t <> written) ts'

    f (ResolvePreTokensStack opens written (PreMarkOpen name owner : ts')) =
        Run $ ResolvePreTokensStack (Set.insert (name, owner) opens) written ts'

    f stack@(ResolvePreTokensStack opens written (PreMarkClose name : ts')) =
        case Set.partition ((== name) . fst) opens of
          (closing, opens') -> if Set.null closing
            then Fail $ "resolvePreTokens: close without open: " <> show (ts_, stack)
            else Run $ ResolvePreTokensStack opens' written ts'

    f (ResolvePreTokensStack opens written (t : ts')) =
        Run $ ResolvePreTokensStack opens (t : written) ts'

    f stack@(ResolvePreTokensStack opens written []) =
        if Set.null opens
          then Halt $ reverse written
          else Fail $ "resolvePreTokens: open without close: " <> show (ts_, stack)

    wrap :: Set PreToken' -> PreToken -> [PreToken]
    wrap (Set.toList -> opens) t = reverse (mkclose <$> opens) <> [t] <> (mkopen <$> opens)
      where
        mkopen  (name, owner) = PreMarkOpen  name owner
        mkclose (name, _)     = PreMarkClose name
