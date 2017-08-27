{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.UtilSpec where
#include "import_frontend.hs"

import Test.Hspec
import Refine.Frontend.Util
import Language.Css.Syntax


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

  describe "instance Css [Decl]" $ do
    it "works" $ do
      css [ "margin" ||= Px 10
          ]
        `shouldBe` [Decl Nothing (Ident "margin") (EVal (VPx (Px 10)))]
      css [ "margin" ||= Px 10
          , "margin" ||= Ident "dashed"
          ]
        `shouldBe` [Decl Nothing (Ident "margin") (SpaceSep (EVal (VPx (Px 10))) (EVal (VIdent (Ident "dashed"))))]
      css [ "margin" ||= Px 10
          , "border" ||= [Ident "dashed", Ident "black"]
          , "margin" ||= Ident "dashed"
          , "border" ||= Px 2
          ]
        `shouldBe` [ Decl Nothing (Ident "border") (SpaceSep
                                                     (SpaceSep
                                                       (EVal (VIdent (Ident "dashed")))
                                                       (EVal (VIdent (Ident "black"))))
                                                     (EVal (VPx (Px 2))))
                   , Decl Nothing (Ident "margin") (SpaceSep
                                                     (EVal (VPx (Px 10)))
                                                     (EVal (VIdent (Ident "dashed"))))]

crash :: HasCallStack => IO Int
crash = crash'

crash' :: HasCallStack => IO Int
crash' = crash''

crash'' :: HasCallStack => IO Int
crash'' = error "wef"
