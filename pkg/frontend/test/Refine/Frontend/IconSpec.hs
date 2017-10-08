{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.IconSpec where
#include "import_frontend.hs"

import Test.Hspec
import Language.Css.Syntax hiding (S)

import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.Util


spec :: Spec
spec = do
  describe "render" $ do
    let t scm st ro icn = do
          let msg = (scm, st, ro, icn)
          r <- html =<< shallow (render scm (ButtonState st ro) icn)
          (msg, r) `shouldNotBe` (msg, mempty)
    it "works" $ do
      sequence_ $ t <$> [minBound ..] <*> [minBound ..] <*> [minBound ..] <*> [minBound ..]

  describe "ibutton" $ do
    it "renders ok" $ do  -- FUTUREWORK: make this a generic test that is run on all properties implicitly.
      wrapper <- mount $ ibutton_ IbuttonProps
        { _ibListKey          = "key"
        , _ibOnClick          = [] :: [GlobalAction]
        , _ibOnClickMods      = []
        , _ibPressed          = Nothing
        , _ibImage            = ButtonImageIcon Svg.Close ColorSchemaDark
        , _ibIndexNum         = Just 3
        , _ibEnabled          = True
        , _ibNotGrayedOut     = True
        , _ibSize             = Medium
        }
      contents :: String <- cs <$> html wrapper
      contents `shouldContain` "<div "

    it "click triggers action" pending
    it "click affects state" pending
    it "rollover affects state" pending
