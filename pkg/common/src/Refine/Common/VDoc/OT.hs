{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Refine.Common.VDoc.OT where

import           Data.List (groupBy)
import qualified Data.Set as Set

import           Refine.Common.Prelude
import           Refine.Common.OT
import           Refine.Common.Types.Core hiding (Edit)

pattern DocBlock' :: Atom BlockType -> Atom Int -> [LineElem] -> DocBlock
pattern DocBlock' a b c = ((a, b), Segments c)

showEditAsRawContent :: Edit RawContent -> RawContent -> RawContent
showEditAsRawContent (fmap unERawContent -> edits) (rawContentToDoc -> doc) = docToRawContent $ patchBlocks doc edits
  where
    -- TUNING: compress/rearrange the elementary edits first or use a vector instead of lists?
    patchBlocks :: [DocBlock] -> Edit [DocBlock] -> [DocBlock]
    patchBlocks bs [] = bs
    patchBlocks bs (e:es) = case e of
        DeleteItem i    -> patchBlocks (take i bs <> (markBlock StyleDeleted (bs !! i): drop (i+1) bs))  $ incIdx i 1 <$> es
        InsertItem i x  -> patchBlocks (take i bs <> (markBlock StyleAdded x: drop i bs)) es
        EditItem i edit -> patchBlocks (take i bs <> (patchBlock (bs !! i) edit: drop (i+1) bs)) es

    patchBlock :: DocBlock -> Edit DocBlock -> DocBlock
    patchBlock (DocBlock' btype depth elems) es = (if null bpatch && null dpatch then id else markBlock StyleChanged)
        $ DocBlock'
            (patch bpatch btype)
            (patch dpatch depth)
            (patchLineElems [e | EditSecond es' <- es, e <- es'] elems)
      where
        bpatch = [e | EditFirst es' <- es, EditFirst  es'' <- es', e <- es'']
        dpatch = [e | EditFirst es' <- es, EditSecond es'' <- es', e <- es'']

    patchLineElems :: Edit LineElems -> [LineElem] -> [LineElem]
    patchLineElems [] bs = bs
    patchLineElems (SegmentListEdit (DeleteItem i): es) bs
        = patchLineElems (incIdx' i 1 <$> es) $ take i bs <> (markElem StyleDeleted (bs !! i): drop (i+1) bs)
    patchLineElems (SegmentListEdit (InsertItem i x): es) bs
        = patchLineElems es $ take i bs <> (markElem StyleAdded x: drop i bs)
    patchLineElems (SegmentListEdit (EditItem i edit): es) bs
        = patchLineElems (incIdx' i (length ls - 1) <$> es) $ take i bs <> (ls <> drop (i+1) bs)
      where ls = patchLineElem (bs !! i) edit
    patchLineElems (JoinItems i: es) (splitAt i -> (as, (a1, b1): (a2, b2): bs))
        | a1 /= a2 = error "impossible: items are not joinable"
        | otherwise = patchLineElems es $ as <> (markElem StyleChanged (a1, joinItems b1 b2): bs)
    patchLineElems (SplitItem i j: es) (splitAt i -> (as, (a, splitItem j -> (b1, b2)): bs))
        = patchLineElems es $ as <> (markElem StyleChanged (a, b1): markElem StyleChanged (a, b2): bs)

    patchLineElem :: LineElem -> Edit LineElem -> [LineElem]
    patchLineElem le es
        = patchLineElemText (patchLineElemStyle le [e | EditFirst es' <- es, e <- es']) [e | EditSecond es' <- es, e <- es']

    patchLineElemStyle :: LineElem -> Edit (Set (Atom EntityStyle)) -> LineElem
    patchLineElemStyle le [] = le
    patchLineElemStyle (as, text) e = markElem StyleChanged (patch e as, text)

    patchLineElemText :: LineElem -> Edit ST -> [LineElem]
    patchLineElemText (as, text) es
        = [ (as <> marks, text')
          | (marks, text') <- compress $ patchText ((,) mempty <$> cs text) [unEText e | e <- es]
          ]

    patchText :: [(Set (Atom EntityStyle), Char)] -> Edit String -> [(Set (Atom EntityStyle), Char)]
    patchText bs [] = bs
    patchText bs (e:es) = case e of
        DeleteItem i    -> patchText (take i bs <> (markElem StyleDeleted (bs !! i): drop (i+1) bs)) $ incIdx i 1 <$> es
        InsertItem i x  -> patchText (take i bs <> (markElem StyleAdded (mempty, x): drop i bs)) es
        EditItem i edit -> patchText (take i bs <> (foldl patchChar (bs !! i) edit: drop (i+1) bs)) es

    patchChar :: (Set (Atom EntityStyle), Char) -> EEdit Char -> (Set (Atom EntityStyle), Char)
    patchChar a _ = markElem StyleChanged a

    compress :: [(Set (Atom EntityStyle), Char)] -> [LineElem]
    compress = fmap (\xs -> (fst (head xs), cs (snd <$> xs))) . groupBy ((==) `on` fst)

    incIdx i k = \case
        DeleteItem j   | j >= i    -> DeleteItem $ j + k
                       | otherwise -> DeleteItem j
        InsertItem j x | j >= i    -> InsertItem (j + k) x
                       | otherwise -> InsertItem j x
        EditItem j x   | j >= i    -> EditItem (j + k) x
                       | otherwise -> EditItem j x

    incIdx' i k = \case
        SegmentListEdit e -> SegmentListEdit $ incIdx i k e
        JoinItems j   | j >= i    -> JoinItems $ j + k
                      | otherwise -> JoinItems j
        SplitItem j x | j >= i    -> SplitItem (j + k) x
                      | otherwise -> SplitItem j x

    markBlock sty (DocBlock btype depth elems) = DocBlock btype depth (markElem sty <$> elems)

    markElem :: Style -> (Set (Atom EntityStyle), a) -> (Set (Atom EntityStyle), a)
    markElem sty = first (Set.singleton (Atom (Right sty)) <>)
