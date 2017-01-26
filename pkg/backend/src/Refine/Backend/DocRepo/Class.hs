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

module Refine.Backend.DocRepo.Class where

import Refine.Backend.DocRepo.Core
import Refine.Common.Types.VDoc


class DocumentRepository dr where
  createRepo         :: dr RepoHandle
  createPatch        :: RepoHandle -> PatchHandle -> VDocVersion 'HTMLCanonical -> dr PatchHandle
  createInitialPatch :: RepoHandle -> VDocVersion 'HTMLCanonical -> dr PatchHandle
  getVersion         :: RepoHandle -> PatchHandle -> dr (VDocVersion 'HTMLCanonical)
  getChildPatches    :: RepoHandle -> PatchHandle -> dr [PatchHandle]
