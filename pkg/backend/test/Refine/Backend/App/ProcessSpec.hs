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
module Refine.Backend.App.ProcessSpec where

import Control.Lens
import Control.Monad (join)
import Test.Hspec

import Refine.Backend.App.Core    as App
import Refine.Backend.App.Process as App
import Refine.Backend.Database (DB)
import Refine.Backend.Test.AppRunner (provideAppRunner)
import Refine.Backend.User (UH)
import Refine.Common.Types


type AppRunner a = AppM DB UH a -> IO a

spec :: Spec
spec = do
  describe "### CollaborativeEdit" . around provideAppRunner $ do
    let crproc = CreateCollabEditProcess CollaborativeEditOnlyPhase UniversalGroup crvdoc
        crvdoc = CreateVDoc title (Abstract mempty) (VDocVersion mempty)
        title  = Title "fnorgh"

    it "create works" $ \(runner :: AppRunner (IO ())) -> do
      join . runner $ do
        CreatedCollabEditProcess _process cvdoc <- App.addProcess $ AddCollabEditProcess crproc
        pure $ (cvdoc ^. compositeVDoc . vdocTitle) `shouldBe` title

    it "read works" $ \(_runner :: AppRunner (IO ())) -> do
      pending

    it "update works" $ \(_runner :: AppRunner (IO ())) -> do
      pending

    it "delete works" $ \(_runner :: AppRunner (IO ())) -> do
      pending
