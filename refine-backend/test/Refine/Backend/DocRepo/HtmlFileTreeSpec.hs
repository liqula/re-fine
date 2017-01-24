{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.DocRepo.HtmlFileTreeSpec where

import Test.Hspec


spec :: Spec
spec = describe "htmlToFileTree" $ do
  describe "unit tests" $ do
    it "creates files for text nodes." pending
    it "creates files for comment nodes." pending
    it "creates directories for tag nodes." pending
    it "stores tag names in file TAGNAME." pending
    it "stores attribute key-value pairs in file ATTRS." pending

  describe "qc properties" $ do
    it "is inverse of htmlFromFileTree." pending
    it "creates a directory for every tag open/close element pair." pending
