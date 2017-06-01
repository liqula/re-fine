{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Refine.Common.VDoc.OT where

import           Data.List (groupBy)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NEL

import           Refine.Common.Prelude
import           Refine.Common.OT
import           Refine.Common.Types.Core hiding (Edit)

pattern DocBlock' :: Atom BlockType -> Atom BlockDepth -> [LineElem] -> DocBlock
pattern DocBlock' a b c = ((a, b), Segments c)

type Styles = Set (Atom Style)

showEditAsRawContent :: Edit RawContent -> RawContent -> RawContent
showEditAsRawContent edits
    = docToRawContent . NEL.fromList . patchBlocks (concat (coerce edits :: [Edit [DocBlock]])) . NEL.toList . rawContentToDoc
  where
    -- TUNING: compress/rearrange the elementary edits first or use a vector instead of lists?
    patchBlocks :: Edit [DocBlock] -> [DocBlock] -> [DocBlock]
    patchBlocks [] bs = bs
    patchBlocks (e:es) bs = case e of
        DeleteItem i    -> patchBlocks (incIdx i 1 <$> es) $ take i bs <> (markBlock StyleDeleted (bs !! i): drop (i+1) bs)
        InsertItem i x  -> patchBlocks es $ take i bs <> (markBlock StyleAdded x: drop i bs)
        EditItem i edit -> patchBlocks es $ take i bs <> (patchBlock (bs !! i) edit: drop (i+1) bs)

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
      where ls = patchLineElem edit (bs !! i)
    patchLineElems (JoinItems i: es) (splitAt i -> (as, (a1, b1): (a2, b2): bs))
        | a1 /= a2 = error "impossible: items are not joinable"
        | otherwise = patchLineElems es $ as <> (markElem StyleChanged (a1, joinItems b1 b2): bs)
    patchLineElems (SplitItem i j: es) (splitAt i -> (as, (a, splitItem j -> (b1, b2)): bs))
        = patchLineElems es $ as <> (markElem StyleChanged (a, b1): markElem StyleChanged (a, b2): bs)

    patchLineElem :: Edit LineElem -> LineElem -> [LineElem]
    patchLineElem es
        = patchLineElemText [e | EditSecond es' <- es, e <- es']
        . patchLineElemEntityStyle [e | EditFirst es' <- es, e <- es']

    patchLineElemEntityStyle :: Edit EntityStyles -> LineElem -> LineElem
    patchLineElemEntityStyle es
        = patchLineElemEntity [e | EditFirst  es' <- es, e <- es']
        . patchLineElemStyle  [e | EditSecond es' <- es, e <- es']

    patchLineElemEntity :: Edit (Atom (Maybe Entity)) -> LineElem -> LineElem
    patchLineElemEntity [] le = le
    patchLineElemEntity e ((ent, ss), text) = markElem StyleChanged ((patch e ent, ss), text)

    patchLineElemStyle :: Edit (Set (Atom Style)) -> LineElem -> LineElem
    patchLineElemStyle [] le = le
    patchLineElemStyle e ((ent, ss), text) = markElem StyleChanged ((ent, patch e ss), text)

    patchLineElemText :: Edit NonEmptyST -> LineElem -> [LineElem]
    patchLineElemText es ((ent, as), text)
        = [ ((ent, as <> marks), text')
          | (marks, text') <- compress $ patchText ((,) noStyle <$> cs text) [unNEText e | e <- es]
          ]

    patchText :: [(Styles, Char)] -> Edit String -> [(Styles, Char)]
    patchText bs [] = bs
    patchText bs (e:es) = case e of
        DeleteItem i    -> patchText (take i bs <> (markStyle StyleDeleted (bs !! i): drop (i+1) bs)) $ incIdx i 1 <$> es
        InsertItem i x  -> patchText (take i bs <> (markStyle StyleAdded (noStyle, x): drop i bs)) es
        EditItem i edit -> patchText (take i bs <> (foldl patchChar (bs !! i) edit: drop (i+1) bs)) es

    patchChar :: (Styles, Char) -> EEdit Char -> (Styles, Char)
    patchChar a _ = markStyle StyleChanged a

    compress :: [(Styles, Char)] -> [(Styles, NonEmptyST)]
    compress = fmap (\xs -> (fst (head xs), NonEmptyST $ cs (snd <$> xs))) . groupBy ((==) `on` fst)

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

    markElem :: Style -> (EntityStyles, a) -> (EntityStyles, a)
    markElem = first . second . addStyle

    markStyle :: Style -> (Styles, a) -> (Styles, a)
    markStyle = first . addStyle

    addStyle sty = (Set.singleton (Atom sty) <>)

    noStyle :: Styles
    noStyle = mempty
