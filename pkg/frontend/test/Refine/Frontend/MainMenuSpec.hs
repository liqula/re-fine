{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.MainMenuSpec where

import           Test.Hspec

import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Component
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Test.Enzyme


spec :: Spec
spec = do
  describe "mainMenu_" $ do
    it "renders" $ do
      wrapper <- shallow $ mainMenu_ defaultMainMenuTab defaultMainMenuErrors UserLoggedOut
      lengthOfIO (find wrapper (StringSelector ".c-mainmenu-content")) `shouldReturn` (1 :: Int)
