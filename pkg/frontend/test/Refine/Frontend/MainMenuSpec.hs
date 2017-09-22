{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.MainMenuSpec where
#include "import_frontend.hs"

import           Test.Hspec

import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Component
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Test.Enzyme


spec :: Spec
spec = do
  describe "mainMenu_" $ do
    it "renders" $ do
      wrapper <- shallow $ view_ mainMenu "mainMenu" (MainMenuProps MainMenuHelp defaultMainMenuErrors UserLoggedOut)
      lengthOfIO (find wrapper (StringSelector "TopMenuBarInMainMenu")) `shouldReturn` (1 :: Int)
