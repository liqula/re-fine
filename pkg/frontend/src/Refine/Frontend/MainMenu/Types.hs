{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Frontend.MainMenu.Types where

import GHC.Generics (Generic)

import Refine.Prelude.TH (makeRefineType)


data MainMenuState
  = MainMenuClosed
  | MainMenuLogin
  | MainMenuRegistration
  deriving (Eq, Show, Generic)

makeRefineType ''MainMenuState
