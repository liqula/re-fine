{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Types.VDocSpec where
#include "import_common.hs"

import           Test.Hspec
import           Test.QuickCheck
import "quickcheck-instances" Test.QuickCheck.Instances ()

import Refine.Common.Test.Arbitrary
import Refine.Common.Test.Samples
import Refine.Common.Types
import Refine.Common.VDoc.Draft

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

un :: HasCallStack => a
un = undefined

rawContentToCompositeVDoc :: RawContentWithSelections -> CompositeVDoc
rawContentToCompositeVDoc (RawContentWithSelections rawContent selections)
    = assert (length selections == length es + length ds)
    $ CompositeVDoc un
                    (Edit sampleMetaID un un un (sampleMetaID ^. miID) rawContent un
                          (Set.fromList $ fst <$> es) (Map.fromList [(i, r) | (i, (r, _)) <- ds]))
                    (Map.fromList es) (Map.fromList ds)
  where
    (es, ds) = rotate ([], []) 0 selections

    rotate :: ([(ID Edit, Edit)], [(ID Discussion, (Range Position, Discussion))])
           -> Int
           -> [Selection Position]
           -> ([(ID Edit, Edit)], [(ID Discussion, (Range Position, Discussion))])
    rotate contribs _ []           = contribs
    rotate contribs i (sel : sels) = rotate (upd contribs) (i + 1) sels
      where
        upd = case i `mod` 2 of
          0 -> _1 %~ (build (Proxy :: Proxy Edit)       i (\_ -> Edit un un un mempty un un un un un) sel :)
          1 -> _2 %~ (build (Proxy :: Proxy Discussion) i (\r -> (r, Discussion un un un un False)) sel :)
          _ -> error "rawContentToCompositeVDoc: impossible."

    build :: Proxy a -> Int -> (Range Position -> b) -> Selection Position -> (ID a, b)
    build Proxy i cons sel = (ID $ fromIntegral i, cons (sel ^. selectionRange))

mark1 :: Style
mark1 = Mark $ MarkContribution (ContribIDEdit (ID 0)) 0


spec :: Spec
spec = do
  describe "rawContentFromCompositeVDoc" $ do
    it "does not crash" . property $ \rawContentWithSelections -> do
      let rawContent' = rawContentFromCompositeVDoc (rawContentToCompositeVDoc rawContentWithSelections)
      (length . show) rawContent' `shouldNotBe` 0

  describe "isEmptyRange" $ do
    it "works" $ do
      let rawContent = mkRawContent $ NEL.fromList
            [mkBlock "wef", mkBlock "", mkBlock "...", mkBlock "*", mkBlock "1234", mkBlock "????"]
          mksel :: Int -> Int -> Int -> Int -> Range StylePosition
          mksel sk so ek eo = toStylePosition rawContent . fromSelectionPoint rawContent <$> Range
            (Position (BlockKey . cs . ('b':) . show $ sk) so)
            (Position (BlockKey . cs . ('b':) . show $ ek) eo)

      mksel 0 0 0 0 `shouldSatisfy`    isEmptyRange
      mksel 0 2 0 2 `shouldSatisfy`    isEmptyRange
      mksel 0 3 0 3 `shouldSatisfy`    isEmptyRange
      mksel 0 3 1 0 `shouldSatisfy`    isEmptyRange
      mksel 1 0 1 0 `shouldSatisfy`    isEmptyRange
      mksel 1 0 2 0 `shouldSatisfy`    isEmptyRange
      mksel 0 3 2 0 `shouldSatisfy`    isEmptyRange
      mksel 0 0 0 1 `shouldNotSatisfy` isEmptyRange
      mksel 0 2 2 0 `shouldNotSatisfy` isEmptyRange
      mksel 0 3 2 1 `shouldNotSatisfy` isEmptyRange
      mksel 2 3 5 0 `shouldNotSatisfy` isEmptyRange
