{-# LANGUAGE CPP #-}
#include "language_common.hs"
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Test (module P, passes, failsOn, thisException, traceBeforeAfterM, traceBeforeAfter) where
#include "import_common.hs"

import Test.Hspec as P
import Test.QuickCheck as P
import "quickcheck-instances" Test.QuickCheck.Instances as P ()
import Test.QuickCheck.Monadic as P

import Refine.Common.Test.Arbitrary as P
import Refine.Common.Test.HttpApiData as P
import Refine.Common.Test.Samples as P


passes :: HasCallStack => Expectation
passes = True `shouldBe` True

failsOn :: HasCallStack => Show a => a -> Expectation
failsOn a = show a `shouldBe` "something else"

thisException :: Show e => e -> SomeException -> Bool
thisException e (SomeException e') = es == es'
  where
    es = show e
    es' = take (length es) $ show e'


{-# ANN traceBeforeAfterM ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
traceBeforeAfterM :: (Show a, Monad m) => String -> m a -> m a
traceBeforeAfterM msg act = do
  () <- trace (">>> before: " <> msg) $ pure ()
  result <- act
  () <- trace (">>> after" <> (show . length . show $ result) <> ": " <> msg) $ pure ()
  pure result

{-# ANN traceBeforeAfter ("HLint: ignore Use errorDoNotUseTrace" :: String) #-}
traceBeforeAfter :: (Show a) => String -> a -> a
traceBeforeAfter msg val =
  trace (">>> before: " <> msg) .
  trace (">>> after(" <> (show . length . show $ val) <> "): " <> msg) $
  val
