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

import           Control.Exception (assert)
import           Control.Lens ((&), (^.), (%~))
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
-- TODO: do we still want '\n' between tokens for darcs?
insertMarks :: Typeable a => [ChunkRange a] -> VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLWithMarks
insertMarks crs vers@(VDocVersion forest) = invariants (tokensFromForest forest) `seq`
                                            VDocVersion forest''
  where
    withPreTokens        = insertMarksForest crs $ enablePreTokens forest
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
                (Node (ContentText s) _ : ts)
                  | seen <= off && (seen + ST.length s) >= off -> True
                  | seen >= off                                -> False
                  | otherwise                                  -> ok (seen + ST.length s) ts
                (t : ts) -> ok (seen + treeTextLength t) ts
                [] -> False
          in [ChunkRangeNodeMustBeDirectParent cp forest | not $ off == 0 || ok 0 xs]
        _ -> []  -- handled by @badDataUID@ above.

    isEmpty = [ChunkRangeEmpty mp1 mp2 forest | not $ isNonEmptyChunkRange cr forest]

-- | Run through the token tree once.  Start counting characters when (1) data-uid has matched, and
-- (2) start offset has been consumed in characters; stop counting accordingly.  Count must be > 0.
--
-- The state machine below only works if the two chunk points do not have the same data-uid, so we
-- catch that case separately.  May falsely return True if there are 'ChunkRangeOffsetTooLarge'
-- errors (which is ok, because in this error is caught in another rule in
-- 'createChuknRangeErrors').
isNonEmptyChunkRange :: CreateChunkRange -> Forest Token -> Bool
isNonEmptyChunkRange (CreateChunkRange mp1 mp2) forest = onSameNode || onDifferentNodes
  where
    onSameNode = case (mp1, mp2) of
      (Just (ChunkPoint uid off), Just (ChunkPoint uid' off')) | uid == uid' -> off' > off
      _ -> False

    onDifferentNodes = findBeginUID (tokensFromForest forest) mp1

    findBeginUID :: [Token] -> Maybe ChunkPoint -> Bool
    findBeginUID ts Nothing   = findEndUID False ts mp2
    findBeginUID ts (Just p1) = findBeginUIDJust ts p1

    findBeginUIDJust :: [Token] -> ChunkPoint -> Bool
    findBeginUIDJust []           _                       = False
    findBeginUIDJust ts@(t : ts') p1@(ChunkPoint uid off) = if dataUidOfToken t == Just uid
      then consumeBeginOffset ts off
      else findBeginUIDJust ts' p1

    consumeBeginOffset :: [Token] -> Int -> Bool
    consumeBeginOffset []        _   = False
    consumeBeginOffset (t : ts') off = assert (off >= 0) $ if n <= off
      then consumeBeginOffset ts' (off - n)
      else findEndUID (n > off) ts' mp2  -- (this is where we assume begin-uid /= end-uid)
      where n = tokenTextLength t

    -- keep track of whether we have seen any characters inside the range in a boolean.
    findEndUID :: Bool -> [Token] -> Maybe ChunkPoint -> Bool
    findEndUID False     (t : ts') Nothing   = findEndUID (tokenTextLength t > 0) ts' Nothing
    findEndUID False     []        Nothing   = False
    findEndUID True      _         Nothing   = True
    findEndUID haveEaten ts        (Just p2) = findEndUIDJust haveEaten ts p2

    findEndUIDJust :: Bool -> [Token] -> ChunkPoint -> Bool
    findEndUIDJust _         []        _                       = False  -- (p2 is missing)
    findEndUIDJust haveEaten (t : ts') p2@(ChunkPoint uid off) = if dataUidOfToken t == Just uid
      then haveEaten || (off > 0 && consumeEndOffset (tokensFromForest $ forest ^. atToken uid))
                           -- (must not read beyond the sub-tree under p2's data-uid!)
      else findEndUIDJust (haveEaten || tokenTextLength t > 0) ts' p2

    consumeEndOffset :: [Token] -> Bool
    consumeEndOffset = any ((> 0) . tokenTextLength)


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
