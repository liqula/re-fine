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

module Refine.Backend.DocRepo.Darcs where

import Control.Lens ((^.), to, view)
import Darcs.Patch()
import Data.String.Conversions
import Data.Text.IO as ST
import Data.UUID
import System.FilePath ((</>))
import System.Random
import System.Directory

import Refine.Backend.DocRepo.Core
import Refine.Common.Types.VDoc


newUUID :: IO String
newUUID = show @UUID <$> randomIO

createRepo :: DocRepo RepoHandle
createRepo = do
  repoRoot <- view docRepoRoot
  docRepoIO $ do
    uuid <- newUUID
    let repoDir = repoRoot </> uuid
    createDirectory repoDir
    pure . RepoHandle . cs $ uuid

createPatch :: RepoHandle -> PatchHandle -> VDocVersion 'HTMLRaw -> DocRepo PatchHandle
createPatch repo _base = createInitialPatch repo

createInitialPatch :: RepoHandle -> VDocVersion 'HTMLRaw -> DocRepo PatchHandle
createInitialPatch repo vers = do
  repoRoot <- view docRepoRoot
  docRepoIO $ do
    uuid <- newUUID
    let repoDir   = repoRoot </> (repo ^. unRepoHandle . to cs)
        patchFile = repoDir </> uuid
    ST.writeFile patchFile (vers ^. unVDocVersion)  -- TODO: need to call canonicalizer!
    pure . PatchHandle $ cs uuid

getVersion :: RepoHandle -> PatchHandle -> DocRepo (VDocVersion 'HTMLCanonical)
getVersion repo vers = do
  repoRoot <- view docRepoRoot
  docRepoIO $ do
    let repoDir   = repoRoot </> (repo ^. unRepoHandle . to cs)
        patchFile = repoDir </> (vers ^. unPatchHandle . to cs)
    VDocVersion <$> ST.readFile patchFile

-- TODO: Implement
getApplicablePatches :: RepoHandle -> PatchHandle -> DocRepo [PatchHandle]
getApplicablePatches _repo _vers = pure []
