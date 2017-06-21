{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Refine.Frontend.UtilSpec where

import Refine.Frontend.Prelude

import Test.Hspec

import Refine.Prelude ()
import Refine.Frontend.Util (toClasses)


spec :: Spec
spec = do
  describe "toClasses" $ do
    it "turns one class name into a string containing that class name" $ do
      toClasses @ST ["single-class-name"] `shouldBe` "single-class-name"
    it "intersperses multiple class names with blanks" $ do
      toClasses @JSString ["class-name-1", "class-name-2"] `shouldBe` "class-name-1 class-name-2"
    it "ignores empty strings" $ do
      toClasses @String ["", "class-name-1", "", "class-name-2", ""] `shouldBe` "class-name-1 class-name-2"

  describe "HasCallStack (not really about this application, but interesting.)" $ do
    it "works" $ do
      crash `shouldThrow` anyException  -- look at the thrown error to see why this is cool.


crash :: HasCallStack => IO Int
crash = crash'

crash' :: HasCallStack => IO Int
crash' = crash''

crash'' :: HasCallStack => IO Int
crash'' = error "wef"
