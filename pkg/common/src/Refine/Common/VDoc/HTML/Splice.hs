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
  , chunkRangeCanBeApplied, chunkRangeMismatch
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
import           Data.List (find)
import           Data.Maybe (catMaybes)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Conversions (ST, (<>), cs)
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

data ChunkRangeMismatch =
    ChunkRangeMismatchEmpty [Token] (Maybe ChunkPoint) (Maybe ChunkPoint)
  | ChunkRangeMismatchNoEndNode [Token] (Maybe ChunkPoint) (Maybe ChunkPoint)
  | ChunkRangeOffsetTooLarge [Token] ChunkPoint
  | ChunkRangeNotATree [Token]
  deriving (Eq, Show)

chunkRangeCanBeApplied :: ChunkRange a -> VDocVersion b -> Bool
chunkRangeCanBeApplied crs = null . chunkRangeMismatch crs

chunkRangeCanBeAppliedTs :: ChunkRange a -> [Token] -> Bool
chunkRangeCanBeAppliedTs crs = null . chunkRangeMismatchTs crs

chunkRangeMismatch :: ChunkRange a -> VDocVersion b -> [ChunkRangeMismatch]
chunkRangeMismatch crs (VDocVersion (parseTokens -> (ts :: [Token]))) = chunkRangeMismatchTs crs ts

-- | Returns 'True' iff (a) both chunk points are either Nothing or hit into an existing point in
-- the tree (i.e. have existing @data-uid@ attributes and offsets no larger than the text length);
-- (b) the begin chunk point is left of the end, and the text between them is non-empty.
chunkRangeMismatchTs :: ChunkRange a -> [Token] -> [ChunkRangeMismatch]
chunkRangeMismatchTs (ChunkRange _ mp1 mp2) ts = rangeNonEmpty <> pointsHit
  where
    pointsHit :: [ChunkRangeMismatch]
    pointsHit = mconcat $ (`chunkPointMismatchTs` ts) <$> catMaybes [mp1, mp2]

    rangeNonEmpty :: [ChunkRangeMismatch]
    rangeNonEmpty = case (mp1, mp2) of
      (Nothing, _) -> []
      (_, Nothing) -> []
      (Just (ChunkPoint b boff), Just (ChunkPoint e eoff))
        -> let -- start keeping tokens from the open tag with the opening data-uid
               ts' = dropWhile ((/= Just b) . dataUidOfToken) ts

               -- stop keeping tokens after the closing tag at the end of the sub-tree with the closing data-uid.
               mts'' = case find ((== Just e) . dataUidOfToken . snd) (zip [0..] ts') of
                   Nothing -> Nothing
                   Just (treeStartIx, (TagOpen n _)) ->
                     let adhocParser :: [ST] -> [Token] -> [Token]
                         adhocParser (n_:ns') (t@(TagClose n')   : ts_) | n' == n_ = t : adhocParser ns'       ts_
                         adhocParser ns       (t@(TagOpen  n' _) : ts_)            = t : adhocParser (n' : ns) ts_
                         adhocParser ns       (t                 : ts_)            = t : adhocParser ns        ts_
                         adhocParser []       []                                   = []
                         adhocParser ns@(_:_) [] = error $ "chunkRangeMismatchTs: bad tree: " <> show ns

                         chunkLength = treeStartIx + length (adhocParser [n] (drop (treeStartIx + 1) ts'))
                     in Just $ take chunkLength ts'

                   bad -> error $ "chunkRangeMismatchTs: non-tag end node: " <> show bad

           in case mts'' of
               Just ts'' -> if sum (tokenTextLength <$> ts'') > 0 || boff >= eoff
                 then []
                 else [ChunkRangeMismatchEmpty ts mp1 mp2]
               Nothing -> [ChunkRangeMismatchNoEndNode ts mp1 mp2]

chunkPointMismatchTs :: ChunkPoint -> [Token] -> [ChunkRangeMismatch]
chunkPointMismatchTs cp@(ChunkPoint uid off) ts = case tokensToForest ts of
  Right forest ->
    let sub = forest ^. atNode (\p -> dataUidOfToken p == Just uid)
    in if forestTextLength sub >= off
      then []
      else [ChunkRangeOffsetTooLarge ts cp]
  Left _ -> [ChunkRangeNotATree ts]


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
      = if preForestTextLength (f ^. atDataUID nod) >= off
          then pure $ f & atDataUID nod %~ insertPreToken (show cp) off mark
          else throwError $ VDocHTMLErrorBadChunkPoint f cp
      where
        atDataUID :: DataUID -> Traversal' (Forest PreToken) (Forest PreToken)
        atDataUID node = atNode (\p -> dataUidOfPreToken p == Just node)
    switch f bad = error $ "insertMarksF: " <> show (f, bad)


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

    isPreMark :: PreToken -> Bool
    isPreMark (PreMarkOpen _ _) = True
    isPreMark (PreMarkClose _)  = True
    isPreMark (PreToken _)      = False


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

    -- which tokens do not want a closing counter-part?  premarks are considered flat here; matching
    -- of open and close marks happens in 'resolvePreTokens'.
    isFlatToken :: PreToken -> Bool
    isFlatToken = \case
      (PreToken (TagOpen n _)) -> n `elem` nonClosing
      (PreToken (TagClose _))  -> False
      (PreMarkOpen _ _)        -> True
      (PreMarkClose _)         -> True
      _                        -> True


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
    go = either throwError pure
       . recursion f
       $ ResolvePreTokensStack mempty [] ts_

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
