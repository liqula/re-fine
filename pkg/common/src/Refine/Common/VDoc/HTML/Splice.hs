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

module Refine.Common.VDoc.HTML.Splice
  ( HasChunkRangeAndID(..), insertMarks, insertMoreMarks, downgradeVDocVersionWC, downgradeVDocVersionWR
  , ChunkRangeError(..), chunkRangeErrors
  , enablePreTokens
  , resolvePreTokens
  , splitAtOffset
  , highlightRange, removeHighlights, isHighlightingMark
  ) where

import           Control.Arrow (second)
import           Control.Exception (assert)
import           Control.Lens ((^.))
import           Control.Monad.State
import           Data.Functor.Infix ((<$$>))
import           Data.List (foldl')
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String.Conversions ((<>))
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Data.Void (Void, absurd)
import           Text.HTML.Parser (Token(..), Attr(..), canonicalizeTokens)
import           Text.HTML.Tree (tokensFromForest, tokensToForest)
import           Web.HttpApiData (toUrlPiece)

import Refine.Common.Types
import Refine.Common.VDoc.HTML.Canonicalize (reCanonicalizeVDocVersion, downgradeVDocVersionCR)
import Refine.Common.VDoc.HTML.Core
import Refine.Common.VDoc.HTML.Enhance (addUIInfoToVDocVersion)
import Refine.Prelude


-- * module interface

class HasChunkRangeAndID a where
  askChunkRange :: a -> ChunkRange
  askID         :: a -> ContributionID

instance HasChunkRangeAndID Contribution where
  askChunkRange (ContribNote note) = note ^. noteRange
  askChunkRange (ContribQuestion question) = question ^. questionRange
  askChunkRange (ContribDiscussion discussion) = discussion ^. discussionRange
  askChunkRange (ContribEdit edit) = edit ^. editRange
  askChunkRange (ContribHighlightMark cr) = cr

  askID (ContribNote note) = ContribIDNote $ note ^. noteID
  askID (ContribQuestion question) = ContribIDQuestion $ question ^. questionID
  askID (ContribDiscussion discussion) = ContribIDDiscussion $ discussion ^. discussionID
  askID (ContribEdit edit) = ContribIDEdit $ edit ^. editID
  askID (ContribHighlightMark _) = ContribIDHighlightMark


-- | Render 'VDocVersion' as needed in the browser.  More specifically: Insert @mark@ html elements
-- for all chunks of all edits, comments, notes, etc.
--
-- TODO: do we still want '\n' between tokens for darcs?
insertMarks :: HasChunkRangeAndID x => [x] -> VDocVersion 'HTMLCanonical -> VDocVersion 'HTMLWithMarks
insertMarks xs vers@(VDocVersion forest) = invariants (tokensFromForest forest) `seq` vers'
  where
    withPreTokens        = insertMarksForest xs $ enablePreTokens forest
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
        errs = mconcat $ (`chunkRangeErrors` vers) . askChunkRange <$> xs


-- Calls 'insertMarks', but expects the input 'VDocVersion' to have passed through before.
insertMoreMarks :: HasChunkRangeAndID x => [x] -> VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLWithMarks
insertMoreMarks xs (VDocVersion vers) = insertMarks xs (VDocVersion vers)

-- | See also: 'removeHighlights'.
downgradeVDocVersionWC :: VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLCanonical
downgradeVDocVersionWC (VDocVersion forest) = reCanonicalizeVDocVersion . VDocVersion $ dfs forest
  where
    dfs [] = []
    dfs (Node element children : siblings) = case element of
      TagOpen "mark" _ ->               dfs children <> dfs siblings
      _                -> Node element (dfs children) : dfs siblings

downgradeVDocVersionWR :: VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLRaw
downgradeVDocVersionWR = downgradeVDocVersionCR . downgradeVDocVersionWC


-- * sanity check

-- | Returns 'True' iff (a) both chunk points are either Nothing or hit into an existing point in
-- the tree (i.e. have existing @data-uid@ attributes and offsets no larger than the text length);
-- (b) the begin chunk point is left of the end; (c) the text between them is non-empty; and (d) for
-- each chunk point, the text node targeted by the offset is a direct child of the node targeted by
-- the corresponding @data-uid@.
--
-- FIXME: [performance] we are checking a number of rules individually, and some of those rules may
-- follow from each other, or checking several rules with one sweep would be cheaper.
chunkRangeErrors :: ChunkRange -> VDocVersion 'HTMLCanonical -> [ChunkRangeError]
chunkRangeErrors cr@(ChunkRange mp1 mp2) (VDocVersion forest) =
    badDataUID <> offsetTooLarge <> isEmpty
  where
    checkChunkPoint :: (ChunkPoint -> [ChunkRangeError]) -> [ChunkRangeError]
    checkChunkPoint go = mconcat $ go <$> catMaybes [mp1, mp2]

    badDataUID = checkChunkPoint $ \cp@(ChunkPoint uid _) ->
      [ChunkRangeBadDataUID cp forest | null $ forest ^. atToken uid]

    offsetTooLarge = checkChunkPoint $ \cp@(ChunkPoint uid off) ->
      [ChunkRangeOffsetOutOfBounds cp forest | off > forestTextLength (forest ^. atToken uid) || off < 0]

    isEmpty = [ChunkRangeEmpty mp1 mp2 forest | not $ isNonEmptyChunkRange cr forest]

isNonEmptyChunkRange :: ChunkRange -> Forest Token -> Bool
isNonEmptyChunkRange cr forest = case cr of
  (ChunkRange Nothing       Nothing)   -> 0               < forestTextLength forest
  (ChunkRange (Just p1)     Nothing)   -> numLeftChars p1 < forestTextLength forest
  (ChunkRange Nothing       (Just p2)) -> 0               < numLeftChars p2
  (ChunkRange (Just p1)     (Just p2)) -> numLeftChars p1 < numLeftChars p2
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

-- | Insert Mark Forest Stack (local type for 'insertMarkForest').
data IMFStack = IMFStack (Forest PreToken) (Forest PreToken) (Map DataUID [(Int, Forest PreToken)])
  deriving (Show)

insertMarksForest :: HasChunkRangeAndID x => [x] -> Forest PreToken -> Forest PreToken
insertMarksForest xs = (prefix <>) . (<> suffix) . dfs
  where
    IMFStack prefix suffix infixmap = foldl' unmaybe (IMFStack mempty mempty mempty) (mconcat $ f <$> xs)
      where
        f x = [(mb, PreMarkOpen'' cnid), (me, PreMarkClose'' cnid)]
          where
            ChunkRange mb me = askChunkRange x
            cnid = askID x

    pushList :: PreToken'' -> Forest PreToken -> Forest PreToken
    pushList mark = (Node (runPreToken'' mark) [] :)

    pushMap :: ChunkPoint -> PreToken'' -> Map DataUID [(Int, Forest PreToken)] -> Map DataUID [(Int, Forest PreToken)]
    pushMap (ChunkPoint duid off) mark = Map.alter upd duid
      where
        new = (off, [Node (runPreToken'' mark) []])
        upd Nothing   = Just [new]
        upd (Just ys) = Just $ new : ys

    unmaybe :: IMFStack -> (Maybe ChunkPoint, PreToken'') -> IMFStack
    unmaybe (IMFStack p s i) (Nothing, mark@(PreMarkOpen'' _))  = IMFStack (pushList mark p) s i
    unmaybe (IMFStack p s i) (Nothing, mark@(PreMarkClose'' _)) = IMFStack p (pushList mark s) i
    unmaybe (IMFStack p s i) (Just point, mark)                 = IMFStack p s (pushMap point mark i)

    dfs :: Forest PreToken -> Forest PreToken
    dfs [] = []
    dfs (Node tok@(PreToken (TagOpen "mark" _)) children : siblings)
      = Node tok (dfs children) : dfs siblings  -- chunk points never refer to mark nodes.
    dfs (Node tok children : siblings)
      = Node tok (dfs children') : dfs siblings
      where
        points :: [(Int, Forest PreToken)]
        points = fromMaybe [] $ (`Map.lookup` infixmap) =<< dataUidOfPreToken tok

        children' = go points children
          where
            go ((offset, insertion) : points') = go points' . splitAtOffset offset insertion
            go []                              = id


-- | Split a token stream into one that has a given number of characters @n@ in its text nodes, and
-- the rest.  Splits up a text node into two text nodes if necessary.
--
-- failures in this function are internal errors.  should have been caught by
-- 'chunkRangeErrors' earlier.
splitAtOffset :: Int -> Forest PreToken -> Forest PreToken -> Forest PreToken
splitAtOffset offset insertion ts_ = assert (offset >= 0)
                                   . resolve $ dfs (Just offset) ts_
  where
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
      { _rptsOpen    :: Set ContributionID
      , _rptsWritten :: [PreToken]
      , _rptsReading :: [PreToken]
      }
  deriving (Eq, Show)


-- | Traverse a 'PreToken' and wrap all 'PreMarkOpen' and 'PreMarkClose' directly around the
-- affected text nodes.  Fails if the opening and closing premarks do not match up.
--
-- failures in this function are internal errors.  should have been caught by
-- 'chunkRangeErrors' earlier.
resolvePreTokens :: [PreToken] -> [Token]
resolvePreTokens ts_ = either absurd id $ runPreToken <$$> go
  where
    go :: Either Void [PreToken]
    go = recursion f $ ResolvePreTokensStack mempty [] ts_

    f :: ResolvePreTokensStack -> Recursion ResolvePreTokensStack Void [PreToken]
    f (ResolvePreTokensStack opens written (t@(PreToken (ContentText _)) : ts')) =
        Run $ ResolvePreTokensStack opens (wrap opens t <> written) ts'

    f (ResolvePreTokensStack opens written (PreMarkOpen name : ts')) =
        Run $ ResolvePreTokensStack (Set.insert name opens) written ts'

    f stack@(ResolvePreTokensStack opens written (PreMarkClose name : ts')) =
        case Set.partition (== name) opens of
          (closing, opens') -> if Set.null closing
            then error $ "resolvePreTokens: close without open: " <> show (ts_, stack)
            else Run $ ResolvePreTokensStack opens' written ts'

    f (ResolvePreTokensStack opens written (t : ts')) =
        Run $ ResolvePreTokensStack opens (t : written) ts'

    f stack@(ResolvePreTokensStack opens written []) =
        if Set.null opens
          then Halt $ reverse written
          else error $ "resolvePreTokens: open without close: " <> show (ts_, stack)

    wrap :: Set ContributionID -> PreToken -> [PreToken]
    wrap (Set.toList -> opens) t = reverse (PreMarkClose <$> opens) <> [t] <> (PreMarkOpen <$> opens)


-- * highlight marks

-- | Insert a @'ChunkRange' ''HighlightMark'@.
--
-- FUTUREWORK: it may be better to not have this special case and rather store a 'Map' of
-- 'ChunkRange's in the frontend that we can add/remove the 'HighlightMark' to/from.  (For now we
-- don't have that 'Map', we only have the 'Map' that stores the pixel positions relative to the
-- document, but it wouldn't be hard to add.)
highlightRange :: Maybe ChunkPoint -> Maybe ChunkPoint
               -> VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLWithMarks
highlightRange mp1 mp2 = reCanonicalizeVDocVersion . insertMoreMarks [ContribHighlightMark $ ChunkRange mp1 mp2]

-- | Remove all @'ChunkRange' ''HighlightMark'@s.
removeHighlights :: VDocVersion 'HTMLWithMarks -> VDocVersion 'HTMLWithMarks
removeHighlights (VDocVersion forest) = reCanonicalizeVDocVersion . VDocVersion $ dfs forest
  where
    dfs [] = []
    dfs (Node element children : siblings) = if isHighlightingMark element
      then dfs children <> dfs siblings
      else Node element (dfs children) : dfs siblings

isHighlightingMark :: Token -> Bool
isHighlightingMark (TagOpen "mark" attrs) =
    Attr "data-contribution-id" (toUrlPiece ContribIDHighlightMark) `elem` attrs
isHighlightingMark _ = False
