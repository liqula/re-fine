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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -w #-}

module Refine.Frontend.MainMenuSpec where

import Control.Lens ((^.))
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances ()

import           Refine.Frontend.Style
import           Refine.Frontend.Test.Enzyme
import           Refine.Frontend.UtilityWidgets

import Refine.Common.Types
import Refine.Frontend.Types
import Refine.Frontend.Store ()
import Refine.Frontend.MainMenu


spec :: Spec
spec = do
  describe "mainMenu_" $ do
    it "renders" $ do
      wrapper <- shallow mainMenu_
      lengthOfIO (find wrapper (StringSelector ".c-mainmenu-content")) `shouldReturn` (1 :: Int)
