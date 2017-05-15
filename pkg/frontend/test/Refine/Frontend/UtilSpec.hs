{-# LANGUAGE NoImplicitPrelude          #-}
module Refine.Frontend.UtilSpec where

import Refine.Frontend.Prelude

import Test.Hspec

import Refine.Prelude ()
import Refine.Frontend.Util (toClasses)


spec :: Spec
spec = do
  describe "toClasses" $ do
    it "turns one class name into a string containing that class name" $ do
      toClasses ["single-class-name"] `shouldBe` "single-class-name"
    it "intersperses multiple class names with blanks" $ do
      toClasses ["class-name-1", "class-name-2"] `shouldBe` "class-name-1 class-name-2"
    it "ignores empty strings" $ do
      toClasses ["", "class-name-1", "", "class-name-2", ""] `shouldBe` "class-name-1 class-name-2"
