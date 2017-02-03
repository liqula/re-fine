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
  ( insertMarks
  , ChunkRangeError(..), createChunkRangeErrors
  , enablePreTokens
  , resolvePreTokens
  , splitAtOffset
  ) where

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
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Canonicalize
import Refine.Prelude


-- * module interface

-- | Render 'VDocVersion' as needed in the browser.  More specifically: Insert @mark@ html elements
-- for all chunks of all edits, comments, notes, etc.
--
-- FUTUREWORK: it's not so nice that we have to pass four lists of chunk ranges, but that's the
-- easiest way to work around list homogeneity.
--
-- TODO: do we still want '\n' between tokens for darcs?
insertMarks ::
     [ChunkRange Edit] -> [ChunkRange Note] -> [ChunkRange Question] -> [ChunkRange Discussion]
  -> VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLWithMarks
insertMarks crse crsn crsq crsd vers@(VDocVersion forest) = invariants (tokensFromForest forest) `seq`
                                            VDocVersion forest''
  where
    withPreTokens        = insertMarksForest crse
                         . insertMarksForest crsn
                         . insertMarksForest crsq
                         . insertMarksForest crsd
                         $ enablePreTokens forest
    afterRunPreTokens    = resolvePreTokens . preTokensFromForest $ withPreTokens
    forest'              = either (error . show) id $ tokensToForest afterRunPreTokens
    VDocVersion forest'' = canonicalizeVDocVersion $ VDocVersion forest'

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
        errs = mconcat $ ((`chunkRangeErrors` vers) <$> crse)
                      <> ((`chunkRangeErrors` vers) <$> crsn)
                      <> ((`chunkRangeErrors` vers) <$> crsq)
                      <> ((`chunkRangeErrors` vers) <$> crsd)
        chunkRangeErrors (ChunkRange _ mp1 mp2) = createChunkRangeErrors $ CreateChunkRange mp1 mp2


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
    badDataUID <> offsetTooLarge <> nodeMustBeDirectParent <> isEmpty
  where
    checkChunkPoint :: (ChunkPoint -> [ChunkRangeError]) -> [ChunkRangeError]
    checkChunkPoint go = mconcat $ go <$> catMaybes [mp1, mp2]

    badDataUID = checkChunkPoint $ \cp@(ChunkPoint uid _) ->
      [ChunkRangeBadDataUID cp forest | null $ forest ^. atToken uid]

    offsetTooLarge = checkChunkPoint $ \cp@(ChunkPoint uid off) ->
      [ChunkRangeOffsetTooLarge cp forest | off > forestTextLength (forest ^. atToken uid)]

    nodeMustBeDirectParent = checkChunkPoint $ \cp@(ChunkPoint uid off) ->
      case forest ^. atToken uid of
        [Node _ xs] ->
          let ok :: Int -> [Tree Token] -> Bool
              ok seen = \case
                -- give up if we're past the offset or out of nodes.
                _ | seen > off -> False
                [] -> False

                -- check text nodes for hits.
                (Node (ContentText s) _ : ts)
                  | seen <= off && (seen + ST.length s) >= off -> True
                  | otherwise                                  -> ok (seen + ST.length s) ts
                (t : ts) -> ok (seen + treeTextLength t) ts

          in [ChunkRangeNodeMustBeDirectParent cp forest | not $ off == 0 || ok 0 xs]
        _ -> []  -- handled by @badDataUID@ above.

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
                                   . either absurd repack
                                   $ recursion consumeToken (offset, [], unpack)
  where
    unpack :: Forest PreToken
    repack :: (Forest PreToken, Forest PreToken) -> Forest PreToken
    (unpack, repack) = case ts_ of
      [Node n ts'] -> (ts', \(tsleft, tsright) -> [Node n $ tsleft <> insertion <> tsright])
      _ -> error $ "impossible splitAtOffset: " <> show (offset, ts_)

    consumeToken :: (Int, Forest PreToken, Forest PreToken)
                 -> Recursion (Int, Forest PreToken, Forest PreToken)
                              Void
                              (Forest PreToken, Forest PreToken)

    consumeToken (0, ps, ts)
        = Halt (reverse ps, ts)

    consumeToken (n, ps, t@(Node (PreToken (ContentText s)) []) : ts')
        = let n' = n - ST.length s
          in if n' > 0
              then Run (n', t : ps, ts')
              else case ST.splitAt n s of
                (s', s'') ->
                  let pack = (`Node` []) . PreToken . ContentText
                  in Halt ( reverse $ pack s'  : ps
                          ,           pack s'' : ts'
                          )

    consumeToken (n, ps, t@(Node _ _) : ts')
        = let n' = n - preTreeTextLength t
          in if n' >= 0
              then Run (n', t : ps, ts')
              else error $ "impossible ChunkRangeNodeMustBeDirectParent: "
                        <> show (offset, insertion, ts_, n, t, ts', ps)

    consumeToken (n, ps, [])
        = if n == 0
            then Halt (reverse ps, [])
            else error $ "impossible ChunkRangeOffsetTooLarge " <> show (offset, n, ts_)
                      <> show (offset, insertion, ts_, n, ps)


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
