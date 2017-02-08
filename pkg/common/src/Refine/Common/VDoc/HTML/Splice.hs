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
  ) where

import           Control.Arrow (second)
import           Control.Exception (assert)
import           Control.Lens ((&), (^.), (%~))
import           Control.Monad.State
import           Data.Functor.Infix ((<$$>))
import           Data.List (foldl')
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Conversions ((<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Data.Typeable (Typeable, typeOf)
import           Data.Void (Void, absurd)
import           Text.HTML.Parser (Token(..), canonicalizeTokens)
import           Text.HTML.Tree (tokensFromForest, tokensToForest)
import           Web.HttpApiData (toUrlPiece)

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Canonicalize (reCanonicalizeVDocVersion)
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Enhance (addUIInfoToVDocVersion)
import Refine.Prelude


-- * module interface

-- | Render 'VDocVersion' as needed in the browser.  More specifically: Insert @mark@ html elements
-- for all chunks of all edits, comments, notes, etc.
--
-- TODO: do we still want '\n' between tokens for darcs?
insertMarks :: Typeable a => [ChunkRange a] -> VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLWithMarks
insertMarks crs vers@(VDocVersion forest) = invariants (tokensFromForest forest) `seq` vers'
  where
    withPreTokens        = insertMarksForest crs $ enablePreTokens forest
    afterRunPreTokens    = resolvePreTokens . preTokensFromForest $ withPreTokens
    forest'              = either (error . show) id $ tokensToForest afterRunPreTokens
    vers'                = addUIInfoToVDocVersion . reCanonicalizeVDocVersion . VDocVersion $ forest'

    -- FIXME: these invariants should all be caught earlier than here.  remove the checks once we've
    -- established they are.
    invariants ts = canonicalized ts `seq` validchunks

    canonicalized ts = if ts /= canonicalizeTokens ts
      then error $ "insertMarks: non-canonical input: " <> show ts
      else ()

    validchunks = if not $ null errs
      then error $ "insertMarks: invalid chunk ranges: " <> show errs
      else ()
      where
        errs = mconcat $ (`chunkRangeErrors` vers) <$> crs
        chunkRangeErrors (ChunkRange _ mp1 mp2) = createChunkRangeErrors $ CreateChunkRange mp1 mp2


-- Calls 'insertMarks', but expects the input 'VDocVersion' to have passed through before.
insertMoreMarks :: Typeable a => [ChunkRange a] -> VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLWithMarks
insertMoreMarks crs (VDocVersion vers) = insertMarks crs (VDocVersion vers)


-- * sanity check

-- | Returns 'True' iff (a) both chunk points are either Nothing or hit into an existing point in
-- the tree (i.e. have existing @data-uid@ attributes and offsets no larger than the text length);
-- (b) the begin chunk point is left of the end; (c) the text between them is non-empty; and (d) for
-- each chunk point, the text node targeted by the offset is a direct child of the node targeted by
-- the corresponding @data-uid@.
--
-- FIXME: [performance] we are checking a number of rules individually, and some of those rules may
-- follow from each other, or checking several rules with one sweep would be cheaper.
createChunkRangeErrors :: CreateChunkRange -> VDocVersion 'HTMLCanonical -> [ChunkRangeError]
createChunkRangeErrors cr@(CreateChunkRange mp1 mp2) (VDocVersion forest) =
    badDataUID <> offsetTooLarge <> isEmpty
  where
    checkChunkPoint :: (ChunkPoint -> [ChunkRangeError]) -> [ChunkRangeError]
    checkChunkPoint go = mconcat $ go <$> catMaybes [mp1, mp2]

    badDataUID = checkChunkPoint $ \cp@(ChunkPoint uid _) ->
      [ChunkRangeBadDataUID cp forest | null $ forest ^. atToken uid]

    offsetTooLarge = checkChunkPoint $ \cp@(ChunkPoint uid off) ->
      [ChunkRangeOffsetOutOfBounds cp forest | off > forestTextLength (forest ^. atToken uid) || off < 0]

    isEmpty = [ChunkRangeEmpty mp1 mp2 forest | not $ isNonEmptyChunkRange cr forest]

isNonEmptyChunkRange :: CreateChunkRange -> Forest Token -> Bool
isNonEmptyChunkRange cr forest = case cr of
  (CreateChunkRange Nothing       Nothing)   -> 0               < forestTextLength forest
  (CreateChunkRange (Just p1)     Nothing)   -> numLeftChars p1 < forestTextLength forest
  (CreateChunkRange Nothing       (Just p2)) -> 0               < numLeftChars p2
  (CreateChunkRange (Just p1)     (Just p2)) -> numLeftChars p1 < numLeftChars p2
  where
    numLeftChars :: ChunkPoint -> Int
    numLeftChars (ChunkPoint uid off) = evalState (dfs forest) 0
      where
        dfs :: Forest Token -> State Int Int
        dfs (Node t@(TagOpen _ _) forest' : forest'') = do
          if dataUidOfToken t == Just uid
            then (+ off) <$> get
            else dfs (forest' <> forest'')
        dfs (Node (ContentText txt) forest' : forest'') = do
          modify (ST.length txt +)
          dfs (forest' <> forest'')
        dfs (Node _ forest' : forest'') = do
          dfs (forest' <> forest'')
        dfs [] = pure 0  -- (happens iff chunk point has 'ChunkRangeBadDataUID'.)


-- * inserting marks

insertMarksForest :: forall a . Typeable a
                  => [ChunkRange a] -> Forest PreToken -> Forest PreToken
insertMarksForest crs = (`woodZip` splitup crs)
  where
    -- turn chunk ranges into chunk points.
    splitup :: [ChunkRange a] -> [(Maybe ChunkPoint, PreToken'')]
    splitup = mconcat . fmap f
      where
        t = cs . show . typeOf $ (undefined :: a)
        f (ChunkRange (toUrlPiece -> l) mb me) = [(mb, PreMarkOpen'' l t), (me, PreMarkClose'' l)]

    -- FUTUREWORK: there is a faster implementation for @woodZip@: if chunks come in the correct
    -- order, we only need to traverse the forest once.  that's why it's called "zip".
    --
    -- FUTUREWORK: it's weird that we have to 'view' into the forest and then 'over' into it *after*
    -- the 'view' has proven there is sufficient text.  with more lens-foo, i bet we could write
    -- something that looks like the 'Right' branch here, but lets 'insertPreToken' return an
    -- 'Either' that is somehow propagated through '(%~)' and out of 'woodZip'.
    woodZip :: Forest PreToken -> [(Maybe ChunkPoint, PreToken'')] -> Forest PreToken
    woodZip = foldl' switch

    switch :: Forest PreToken -> (Maybe ChunkPoint, PreToken'') -> Forest PreToken
    switch f (Nothing, mark@(PreMarkOpen'' _ _)) = [Node (unPreToken'' mark) []] <> f
    switch f (Nothing, mark@(PreMarkClose'' _))  =                                  f <> [Node (unPreToken'' mark) []]
    switch f (Just (ChunkPoint node off), mark)  = f & atPreToken node %~ splitAtOffset off [Node (unPreToken'' mark) []]


-- | Split a token stream into one that has a given number of characters @n@ in its text nodes, and
-- the rest.  Splits up a text node into two text nodes if necessary.
--
-- failures in this function are internal errors.  should have been caught by
-- 'createChunkRangeErrors' earlier.
splitAtOffset :: Int -> Forest PreToken -> Forest PreToken -> Forest PreToken
splitAtOffset offset insertion ts_ = assert (offset >= 0)
                                   . repack . resolve $ dfs (Just offset) unpack
  where
    unpack :: Forest PreToken
    repack :: Forest PreToken -> Forest PreToken
    (unpack, repack) = case ts_ of
      [Node n ts'] -> (ts', \ts'' -> [Node n ts''])
      _ -> error $ "impossible splitAtOffset: " <> show (offset, ts_)

    resolve :: (Maybe Int, Forest PreToken) -> Forest PreToken
    resolve (Nothing, forest)
        = forest

    resolve (Just 0, forest)
        = insertion <> forest

    resolve (Just n, forest)
        = assert (n > 0) . error $ "splitAtOffset: out of characters: " <> show (offset, n, forest, ts_)

    dfs :: Maybe Int -> Forest PreToken -> (Maybe Int, Forest PreToken)
    -- already done splitting.
    dfs Nothing forest
        = (Nothing, forest)

    -- not inserted yet, but out of children on this level.  ascend if applicable.
    dfs (Just n) []
        = (Just n, [])

    -- text nodes: update length, insert here if we have reached the offset.
    dfs (Just n) (t@(Node (PreToken (ContentText s)) []) : siblings)
        = let n' = n - ST.length s
          in if n' > 0
              then second (t :) $ dfs (Just n') siblings
              else case ST.splitAt n s of
                (s', s'') ->
                  let pack "" = []
                      pack s_ = [Node (PreToken (ContentText s_)) []]
                  in (Nothing, pack s' <> insertion <> pack s'' <> siblings)

    -- tag nodes: descend.
    dfs (Just n) (Node t@(PreToken (TagOpen _ _)) children : siblings)
        = case dfs (Just n) children of
            (mo, children') -> case dfs mo siblings of
              (mo', siblings') -> (mo', Node t children' : siblings')

    -- pre-marks (also allows any other child-less nodes).
    dfs (Just n) (t@(Node _ []) : siblings)
        = second (t :) $ dfs (Just n) siblings

    -- disallow any nodes with children that are not open tags.
    dfs (Just n) (t : _)
        = error $ "splitAtOffset: weirdly shaped node: " <> show (t, offset, n, ts_)


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


-- | Traverse a 'PreToken' and wrap all 'PreMarkOpen' and 'PreMarkClose' directly around the
-- affected text nodes.  Fails if the opening and closing premarks do not match up.
--
-- failures in this function are internal errors.  should have been caught by
-- 'createChunkRangeErrors' earlier.
resolvePreTokens :: [PreToken] -> [Token]
resolvePreTokens ts_ = either absurd id $ runPreToken <$$> go
  where
    go :: Either Void [PreToken]
    go = recursion f $ ResolvePreTokensStack mempty [] ts_

    f :: ResolvePreTokensStack -> Recursion ResolvePreTokensStack Void [PreToken]
    f (ResolvePreTokensStack opens written (t@(PreToken (ContentText _)) : ts')) =
        Run $ ResolvePreTokensStack opens (wrap opens t <> written) ts'

    f (ResolvePreTokensStack opens written (PreMarkOpen name owner : ts')) =
        Run $ ResolvePreTokensStack (Set.insert (name, owner) opens) written ts'

    f stack@(ResolvePreTokensStack opens written (PreMarkClose name : ts')) =
        case Set.partition ((== name) . fst) opens of
          (closing, opens') -> if Set.null closing
            then error $ "resolvePreTokens: close without open: " <> show (ts_, stack)
            else Run $ ResolvePreTokensStack opens' written ts'

    f (ResolvePreTokensStack opens written (t : ts')) =
        Run $ ResolvePreTokensStack opens (t : written) ts'

    f stack@(ResolvePreTokensStack opens written []) =
        if Set.null opens
          then Halt $ reverse written
          else error $ "resolvePreTokens: open without close: " <> show (ts_, stack)

    wrap :: Set PreToken' -> PreToken -> [PreToken]
    wrap (Set.toList -> opens) t = reverse (mkclose <$> opens) <> [t] <> (mkopen <$> opens)
      where
        mkopen  (name, owner) = PreMarkOpen  name owner
        mkclose (name, _)     = PreMarkClose name
