{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.MainMenuSpec where

import Refine.Frontend.Prelude

import           Test.Hspec

import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Component
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Test.Enzyme


spec :: Spec
spec = do
  describe "mainMenu_" $ do
    it "renders" $ do
      wrapper <- shallow $ mainMenu_ (MainMenuProps MainMenuHelp defaultMainMenuErrors UserLoggedOut)
      lengthOfIO (find wrapper (StringSelector "TopMenuBarInMainMenu")) `shouldReturn` (1 :: Int)
