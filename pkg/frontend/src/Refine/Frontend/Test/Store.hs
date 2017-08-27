{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Test.Store where
#include "import_frontend.hs"

import           Control.Concurrent
import           Data.List (isInfixOf)
import           Test.Hspec

import           Refine.Frontend.Store ()
import           Refine.Frontend.Store.Types


tryRounds :: HasCallStack => Int
tryRounds = 10

tryDelay :: HasCallStack => Int
tryDelay = 500000

-- | Morally, @storeShouldEventuallySatisfy f (== a) === storeShouldEventuallyBe f a@, but the error
-- message here shows the desired value.
storeShouldEventuallyBe :: HasCallStack => forall s a. (HasCallStack, StoreData s, Eq a, Show a) => (s -> a) -> a -> Expectation
storeShouldEventuallyBe fun a = go tryRounds
  where
    go r = do
      a' <- fun <$> readStoreData
      if a' == a || r <= 0
        then a' `shouldBe` a
        else threadDelay tryDelay >> go (r - 1)

-- | Morally, @storeShouldEventuallySatisfy f (isInfixOf as) === storeShouldEventuallyContain f as@,
-- but the error message here shows the desired value.
storeShouldEventuallyContain :: HasCallStack => forall s a. (HasCallStack, StoreData s, Eq a, Show a) => (s -> [a]) -> [a] -> Expectation
storeShouldEventuallyContain fun sublist = go tryRounds
  where
    go r = do
      a' <- fun <$> readStoreData
      if sublist `isInfixOf` a' || r <= 0
        then a' `shouldContain` sublist
        else threadDelay tryDelay >> go (r - 1)

storeShouldEventuallySatisfy :: HasCallStack => forall s a. (HasCallStack, StoreData s, Show a) => (s -> a) -> (a -> Bool) -> Expectation
storeShouldEventuallySatisfy fun predicate = go tryRounds
  where
    go r = do
      a' <- fun <$> readStoreData
      if predicate a' || r <= 0
        then a' `shouldSatisfy` predicate
        else threadDelay tryDelay >> go (r - 1)


-- | Send a 'ResetState' action and wait for it to materialize.
resetState :: HasCallStack => GlobalState -> Expectation
resetState s = do
  dispatchAndExec $ ResetState s
  storeShouldEventuallyBe id s
