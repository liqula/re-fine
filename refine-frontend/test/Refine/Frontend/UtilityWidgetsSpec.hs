{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.UtilityWidgetsSpec where

import Test.Hspec

-- import Refine.Frontend.Test.Console
import Refine.Frontend.Test.Enzyme
import Refine.Frontend.UtilityWidgets


spec :: Spec
spec =
  describe "The icon_ component" $ do
    it "annotates the block together with the icon module" $ do
      wrapper <- shallow $ icon_ (IconProps "bla" True ("Icon", "striped") XXL)

      wrapper1 <- find wrapper ".bla__icon"
      -- wrapper1@(ShallowWrapper jsval') <- find wrapper ".bla__icon"
      -- consoleLogJSVal "..." jsval'
      getWrapperAttr wrapper1 "length" `shouldReturn` (1 :: Int)

