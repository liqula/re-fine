{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
module Refine.Common.VDoc.OT where

import           Data.List (groupBy)
import qualified Data.Set as Set

import           Refine.Common.Prelude
import           Refine.Common.OT
import           Refine.Common.Types.Core hiding (Edit)

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

    incIdx i k = \case
        DeleteItem j   | j >= i    -> DeleteItem $ j + k
                       | otherwise -> DeleteItem j
        InsertItem j x | j >= i    -> InsertItem (j + k) x
                       | otherwise -> InsertItem j x
        EditItem j x   | j >= i    -> EditItem (j + k) x
                       | otherwise -> EditItem j x

    -- TODO: show blocktype and depth changes
    patchBlock :: DocBlock -> Edit DocBlock -> DocBlock
    patchBlock ((btype, elems), depth) es = (if null bpatch && null dpatch then id else markBlock StyleChanged)
          (( patch bpatch btype
           , patchLineElems elems [e | EditFirst es' <- es, EditSecond es'' <- es', e <- es''])
          , patch dpatch depth
          )
      where
        bpatch = [e | EditFirst es' <- es, EditFirst es'' <- es', e <- es'']
        dpatch = [e | EditSecond es' <- es, e <- es']

    patchLineElems :: [LineElem] -> Edit [LineElem] -> [LineElem]
    patchLineElems bs [] = bs
    patchLineElems bs (e:es) = case e of
        DeleteItem i    -> patchLineElems (take i bs <> (markElem StyleDeleted (bs !! i): drop (i+1) bs))  $ incIdx i 1 <$> es
        InsertItem i x  -> patchLineElems (take i bs <> (markElem StyleAdded x: drop i bs)) es
        EditItem i edit -> patchLineElems (take i bs <> (ls <> drop (i+1) bs)) $ incIdx i (length ls - 1) <$> es
          where ls = patchLineElem (bs !! i) edit

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
        DeleteItem i    -> patchText (take i bs <> (markElem StyleDeleted (bs !! i): drop (i+1) bs))  $ incIdx i 1 <$> es
        InsertItem i x  -> patchText (take i bs <> (markElem StyleAdded (mempty, x): drop i bs)) es
        EditItem i edit -> patchText (take i bs <> (foldl patchChar (bs !! i) edit: drop (i+1) bs)) es

    patchChar :: (Set (Atom EntityStyle), Char) -> EEdit Char -> (Set (Atom EntityStyle), Char)
    patchChar a _ = markElem StyleChanged a

    compress :: [(Set (Atom EntityStyle), Char)] -> [LineElem]
    compress = fmap (\xs -> (fst (head xs), cs (snd <$> xs))) . groupBy ((==) `on` fst)

    markBlock sty ((btype, elems), depth) = ((btype, markElem sty <$> elems), depth)

    markElem :: Style -> (Set (Atom EntityStyle), a) -> (Set (Atom EntityStyle), a)
    markElem sty = first (Set.singleton (Atom (Right sty)) <>)
