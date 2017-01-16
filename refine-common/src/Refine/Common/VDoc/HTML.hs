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

-- | Code for handling `VDocVersions` with HTML content.
--
-- TODO: think of a better module name / place?
module Refine.Common.VDoc.HTML
  ( VDocHTMLError(..)
  , canonicalizeVDocVersion
  , decanonicalizeVDocVersion
  , canonicalizeWhitespace
  , canonicalizeAttrsForest, canonicalizeAttrsTree, canonicalizeAttrsStream, canonicalizeAttrs
  , setElemUIDs
  , wrapInTopLevelTags
  , trickledownUIInfo
  , insertMarks

  -- * internal (exported for testing)
  , PreToken(..), enablePreTokens, resolvePreTokens, runPreToken, splitAtOffset
  ) where

import           Control.Exception (assert)
import           Control.Lens (Traversal', (&), (^.), (%~), _1)
import           Control.Monad.Error.Class (MonadError, throwError)
import           Control.Monad (foldM)
import           Control.Monad.State
import           Data.Char (isSpace)
import           Data.Function (on)
import           Data.Functor.Infix ((<$$>))
import           Data.List as List (foldl', sort, nubBy)
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Set as Set hiding (foldl')
import           Data.String.Conversions (ST, (<>), cs)
import qualified Data.Text as ST
import           Data.Tree (Forest, Tree(..))
import           Text.HTML.Parser (Token(..), Attr(..), parseTokens, canonicalizeTokens, renderTokens)
import           Text.HTML.Tree (ParseTokenForestError, tokensToForest, tokensFromForest, nonClosing)
import           Text.Read (readMaybe)
import           Web.HttpApiData (toUrlPiece)

import Refine.Common.Types
import Refine.Prelude


data VDocHTMLError =
    VDocHTMLErrorBadTree ParseTokenForestError
  | VDocHTMLErrorBadChunkPoint (Forest PreToken) ChunkPoint
  | VDocHTMLErrorNotEnouchCharsToSplit Int [PreToken]
  | VDocHTMLErrorInternal String
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
                      $ tokensFromForest . fmap fill <$> tokensToForest' ts
  where
    defaultTag :: Token
    defaultTag = TagOpen "span" []

    fill :: Tree Token -> Tree Token
    fill n@(Node (ContentText _) _) = Node defaultTag [n]
    fill n                          = n


-- | Make sure all open tags have a @data-uid@ attribute (if missing: inherit from first suitable
-- ancestor) and a @data-offset@ attribute that contains the text offset relative to that ancestor.
-- (i.e., we traverse the left siblings and their descendents and compute the sum of the text
-- lenghts.)
--
-- This function should probably be called on (the forest contained in) @VDocVersion
-- 'HTMLWithMarks@.
trickledownUIInfo :: Forest Token -> Forest Token
trickledownUIInfo forest = assert (tokensFromForest forest == canonicalizeTokens (tokensFromForest forest))
                         . assert (Right (tokensFromForest forest) == wrapInTopLevelTags (tokensFromForest forest))
                         . fst . fst . (`runState` [])
                         $ doforest 0 forest
  where
    overwriteAttrT :: Attr -> Token -> Token
    overwriteAttrT attr (TagOpen n attrs) = canonicalizeAttrs $ TagOpen n (attr : attrs)
    overwriteAttrT _    t                 = t

    overwriteAttrN :: Attr -> Tree Token -> Tree Token
    overwriteAttrN attr (Node t xs) = Node (overwriteAttrT attr t) xs

    doforest :: Int -> [Tree Token] -> State [DataUID] ([Tree Token], Int)
    doforest acc [] = pure ([], acc)
    doforest acc (t : ts) = do
      (t', l) <- dotree t
      let t'' = overwriteAttrN (Attr "data-offset" (cs $ show acc)) t'
      (_1 %~ (t'' :)) <$> doforest (acc + l) ts

    dotree :: Tree Token -> State [DataUID] (Tree Token, Int)
    dotree t@(Node (ContentText s) []) = do
      pure (t, ST.length s)

    dotree (Node x@(TagOpen _ _) xs) = do
      let push :: [DataUID] -> [DataUID]
          push stack = new : stack
            where
              new = case (dataUidOfToken x, stack) of
                      (Just uid, _)       -> uid
                      (Nothing,  uid : _) -> uid
                      (Nothing,  [])      -> error "trickledownUIInfo: call wrapInTopLevelTags first!"

          pop :: [DataUID] -> [DataUID]
          pop (_ : stack) = stack
          pop [] = error "impossible."

      modify push

      (xs', l) <- doforest 0 xs
      (new : _) <- get

      modify pop

      let x' = overwriteAttrT (Attr "data-uid" (cs $ show new)) x
          n' = Node x' xs'
      pure (n', l)

    dotree n = pure (n, 0)


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
isOpen (PreToken (TagOpen n _)) = n `notElem` nonClosing
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


-- | This is a bit of a hack.  We want to use tokensFromForest, so we 'fmap' 'runPreToken' on the
-- input forest, then render that, and recover the pretokens from the tokens.
--
-- This assumes there weren't any nodes of the form @PreToken (TagOpen "mark" _)@ in the forest, but
-- just @PreMarkOpen _@.
preTokensFromForest :: Forest PreToken -> [PreToken]
preTokensFromForest = fmap popPreToken . tokensFromForest . fmap (fmap stashPreToken)

-- | Inverse of 'preTokensFromForest' (equally hacky).
preTokensToForest :: MonadError VDocHTMLError m => [PreToken] -> m (Forest PreToken)
preTokensToForest = fmap (fmap (fmap popPreToken)) . tokensToForest' . fmap stashPreToken

stashPreToken :: PreToken -> Token
stashPreToken (PreMarkOpen l)  = TagOpen "mark" [Attr "data-chunk-id" l]
stashPreToken (PreMarkClose l) = TagClose ("mark_" <> l)
stashPreToken (PreToken t)     = t

popPreToken :: Token -> PreToken
popPreToken (TagOpen "mark" [Attr "data-chunk-id" l]) = PreMarkOpen l
popPreToken (TagClose m) | "mark_" `ST.isPrefixOf` m  = PreMarkClose $ ST.drop 5 m
popPreToken t                                         = PreToken t

tokensToForest' :: MonadError VDocHTMLError m => [Token] -> m (Forest Token)
tokensToForest' ts = case tokensToForest ts of
  Right f  -> pure f
  Left msg -> throwError $ VDocHTMLErrorBadTree msg


preTokenTextLength :: PreToken -> Int
preTokenTextLength = \case
  (PreToken (ContentText t)) -> ST.length t
  (PreToken (ContentChar _)) -> 1
  _                          -> 0

preForestTextLength :: Forest PreToken -> Int
preForestTextLength = sum . fmap preTokenTextLength . preTokensFromForest


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
insertMarksTs crs ts = do
  let catch cns = either (throwError . cns) pure
  pf <- catch VDocHTMLErrorBadTree (tokensToForest ts)
  pfm <- insertMarksF crs (enablePreTokens pf)
  catch VDocHTMLErrorInternal (resolvePreTokens $ preTokensFromForest pfm)


insertMarksF :: forall m a . MonadError VDocHTMLError m
             => [ChunkRange a] -> Forest PreToken -> m (Forest PreToken)
insertMarksF crs = (`woodZip` splitup crs)
  where
    -- turn chunk ranges into chunk points.
    splitup :: [ChunkRange a] -> [(ChunkPoint, PreToken)]
    splitup = mconcat . fmap f
      where
        f (ChunkRange (toUrlPiece -> l) mb me) =
            catMaybes [(, PreMarkOpen l) <$> mb, (, PreMarkClose l) <$> me]

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

-- | 'Traversal' optics to find all sub-trees with a given `data-uid` html attribute.  Takes time
-- linear in size of document for find the sub-tree.
--
-- NOTE: I think if a sub-tree of a matching sub-tree matches again, that will go unnoticed.  In
-- 'atDataUID' (where always only one node must satisfy the predicate) this isn't an issue.
atNode :: (a -> Bool) -> Traversal' (Forest a) (Forest a)
atNode prop focus = dive
  where
    dive [] = pure []
    dive (Node n chs : ts)
      = if prop n
          then (:) <$> (Node n <$> focus chs) <*> dive ts
          else (:) <$> (Node n <$> dive  chs) <*> dive ts

atDataUID :: DataUID -> Traversal' (Forest PreToken) (Forest PreToken)
atDataUID node = atNode (\p -> dataUidOfPreToken p == Just node)

dataUidOfPreToken :: PreToken -> Maybe DataUID
dataUidOfPreToken (PreToken t)     = dataUidOfToken t
dataUidOfPreToken (PreMarkOpen _)  = Nothing
dataUidOfPreToken (PreMarkClose _) = Nothing

dataUidOfToken :: Token -> Maybe DataUID
dataUidOfToken (TagOpen _ attrs) =
  readMaybe . cs =<< listToMaybe (mconcat $ (\(Attr k v) -> [v | k == "data-uid"]) <$> attrs)
dataUidOfToken _ = Nothing

-- | In a list of text or premark tokens, add another premark token at a given offset, splitting up
-- an existing text if necessary.
--
-- FUTUREWORK: we could make more assumptions here on the structure of the tree: input is
-- canonicalized and all tags have data-uid, so the forest can only contain a single ContentText
-- node and no deep nodes.
insertPreToken :: String -> Int -> PreToken -> Forest PreToken -> Forest PreToken
insertPreToken errinfo offset mark forest = either err id go
  where
    go :: MonadError VDocHTMLError m => m (Forest PreToken)
    go = do
      (ts, ts') <- splitAtOffset offset $ preTokensFromForest forest
      preTokensToForest $ ts <> [mark] <> ts'

    -- TODO: If 'ChunkRange' does not apply to tree (e.g. because of too large offset), this throws an
    -- internal error.  We need to catch that more gracefully.
    err e = error $ "internal error: insertPreToken: " <> show (e, errinfo, mark, forest)


-- | Split a token stream into one that has a given number of characters @n@ in its text nodes, and
-- the rest.  Splits up a text node into two text nodes if necessary.  Tags that have no length
-- (e.g. comments) go to the left.  Stops scanning on any non-flat token (see 'isFlatToken'), and
-- either returns the resulting split or an error.
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
      (PreMarkOpen _)          -> True  -- (this *has* to be "flat" for insertPreToken -> work.)
      (PreMarkClose _)         -> True  -- (this *has* to be "flat" for insertPreToken -> work.)
      _                        -> True
