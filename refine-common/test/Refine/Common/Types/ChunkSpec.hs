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

module Refine.Common.Types.ChunkSpec where

import qualified Data.Aeson as Aeson
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Refine.Common.Test.Arbitrary ()
import           Refine.Common.Types.Chunk

spec :: Spec
spec = do

  describe "ChunkPoint" $ do
    it "aeson encode and decode are inverses" . property $
      \(x :: ChunkPoint) ->
        let x' = Aeson.object ["value" Aeson..= x]
        in Aeson.decode (Aeson.encode x') == Just x'
