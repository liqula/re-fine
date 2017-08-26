{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Contribution.BubbleSpec where

import Refine.Frontend.Prelude hiding (property)

import           Control.Lens ((^.), (&), (.~))
import           Data.Int (Int64)
import qualified Data.List.NonEmpty as NEL
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Language.Css.Syntax
import           Test.Hspec
import           Test.QuickCheck

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Test.Store
import           Refine.Frontend.Types
import           Refine.Frontend.Util


cnid :: Int64 -> ContributionID
cnid = ContribIDDiscussion True . ID


spec :: Spec
spec = do
  let contributionId = cnid 99
      iconSide = BubbleLeft
      offset = Just (OffsetFromDocumentTop (140 + 180))
      highlight = False
      screenState = ScreenState 95 0 Desktop
      bubbleProps = BubbleProps (NoStack (contributionId, 0)) iconSide offset highlight screenState

  describe "The bubble_ component" $ do
    it "renders the o-snippet class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StringSelector ".o-snippet") `shouldReturn` True

    it "renders the top style with the correct value" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StyleSelector [decl "top" (Px 145)]) `shouldReturn` True

    it "has a child with the icon-bg class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      lengthOfIO (find wrapper (StringSelector ".o-snippet__icon-bg")) `shouldReturn` 1

    it "has a child with the icon-bg + side class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      lengthOfIO (find wrapper (StringSelector ".o-snippet__icon-bg--left")) `shouldReturn` 1

    it "has a child with the content class" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      lengthOfIO (find wrapper (StringSelector ".o-snippet__content")) `shouldReturn` 1

    it "does not render the hover class when there is no highlighted bubble" $ do
      wrapper <- shallow $ bubble_ bubbleProps mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` False

    it "does not render the hover class when the highlight flag is off" $ do
      wrapper <- shallow $ bubble_ (bubbleProps & bubblePropsHighlight .~ False) mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` False

    it "renders the hover class when the highlight flag is on" $ do
      wrapper <- shallow $ bubble_ (bubbleProps & bubblePropsHighlight .~ True) mempty
      is wrapper (StringSelector ".o-snippet--hover") `shouldReturn` True

    it "inserts the id of the current bubble into the state on mouseEnter and removes it again on mouseLeave" $ do
      pendingWith "race condition, this only sporadically fails!"

      wrapper <- mount $ bubble_ bubbleProps mempty
      storeShouldEventuallyBe ((^. gsContributionState . csHighlightedMarkAndBubble) :: GlobalState -> [MarkID]) []

      _ <- simulate wrapper MouseEnter
      storeShouldEventuallyBe ((^. gsContributionState . csHighlightedMarkAndBubble) :: GlobalState -> [MarkID]) [MarkContribution (cnid 99) 0]

      _ <- simulate wrapper MouseLeave
      storeShouldEventuallyBe ((^. gsContributionState . csHighlightedMarkAndBubble) :: GlobalState -> [MarkID]) []

  describe "tablet and mobile" $ do
    it "works" $ do
      pending

  describe "stackComponents" $ do
    describe "examples" $ do
      it "sorts its input on absolute position" $ do
        stackComponents fst snd [(27, 1), (10, 1), (16, 1), (12, 1)]
          `shouldBe`
            (NoStack <$> [(10, 1), (12, 1), (16, 1), (27, 1)])

      it "works on non-overlapping components" $ do
        stackComponents fst snd [(10, 5), (16, 3), (27, 8)]
          `shouldBe`
            (NoStack <$> [(10, 5), (16, 3), (27, 8)])

      it "works if two components overlap" $ do
        stackComponents fst snd [(10, 8), (16, 3), (27, 8)]
          `shouldBe`
            [Stack ((10, 8) :| [(16, 3)]), NoStack (27, 8)]

      it "works if one component is completely covered by another" $ do
        stackComponents fst snd [(1, 1), (16, 20), (10, 80)]
          `shouldBe`
            [NoStack (1, 1), Stack ((10, 80) :| [(16, 20)])]

      it "works if two components overlap perfectly" $ do
        stackComponents fst snd [(0, 1), (0, 1)]
          `shouldBe`
            [Stack ((0, 1) :| [(0, 1)])]

      it "works if two components overlap and both end in the same point" $ do
        stackComponents fst snd [(1, 1), (3, 1), (0, 2)]
          `shouldBe`
            [Stack ((0, 2) :| [(1, 1)]), NoStack (3, 1)]

      it "works if overlap is between components with non-neighboring absolute positions" $ do
        -- this was found by quickcheck, and it identified a bug in the property.  what fun!  :-)
        stackComponents fst snd [(1, 1), (0, 3), (3, 1)]
          `shouldBe`
            [Stack ((0, 3) :| [(1, 1), (3, 1)])]

    describe "properties" $ do
      let sanitize = fmap (\(i, j) -> (abs i, 1 + abs j))

      it "sorts its input on absolute position; does not lose or add components" . property $ \cmps_ -> do
        let cmps = sanitize cmps_
            stacked = stackComponents fst snd cmps
            unstack [] = []
            unstack (NoStack a : xs) = a : unstack xs
            unstack (Stack (a :| as) : xs) = a : as <> unstack xs
        unstack stacked `shouldBe` sortBy (compare `on` fst) cmps

      it "only stacks overlappers (and stacks are never empty)" . property $ \cmps_ -> do
        let cmps = sanitize cmps_
            stacked = stackComponents fst snd cmps
        forM_ stacked $ \case
          NoStack _       -> True `shouldBe` True
          Stack (x :| xs) -> forM_ (zip (x : xs) xs) $ \((p, h), (p', h')) -> do
            True `shouldBe` True
            [p, p'] `shouldSatisfy` all (>= 0)
            [h, h'] `shouldSatisfy` all (> 0)
            p' `shouldSatisfy` (>= p)

            -- @p' `shouldSatisfy` (<= p + h)@ is not necessary: the previous stack item may overlap
            -- with the next one even if this one does not.

      it "keeps non-overlappers single" . property $ \(cmp_, cmps_) -> do
        let cmps = sanitize (cmp_ : cmps_)
            stacked = stackComponents fst snd cmps
            minPos (NoStack c) = fst c
            minPos (Stack s)   = fst (NEL.head s)
            maxPos (NoStack c) = uncurry (+) c
            maxPos (Stack s)   = uncurry (+) (NEL.last s)
        forM_ (zip stacked (tail stacked)) $ \(this, next) -> maxPos this `shouldSatisfy` (< minPos next)
