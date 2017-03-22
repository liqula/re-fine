module Refine.Frontend.UtilSpec where

import Test.Hspec
import Data.JSString (pack)

import Refine.Prelude ()
import Refine.Frontend.Util (toClasses)


spec :: Spec
spec = do
  describe "toClasses" $ do
    it "turns one class name into a string containing that class name" $ do
      toClasses [pack "single-class-name"] `shouldBe` pack "single-class-name"
    it "intersperses multiple class names with blanks" $ do
      toClasses (pack <$> ["class-name-1", "class-name-2"]) `shouldBe` pack "class-name-1 class-name-2"
    it "ignores empty strings" $ do
      toClasses (pack <$> ["", "class-name-1", "", "class-name-2", ""]) `shouldBe` pack "class-name-1 class-name-2"
