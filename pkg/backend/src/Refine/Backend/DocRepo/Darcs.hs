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
import Darcs.Patch ()
import Data.String.Conversions
import Data.Text.IO as ST
import Data.UUID
import System.FilePath ((</>))
import System.Random
import System.Directory

import Refine.Backend.Config
import Refine.Backend.DocRepo.Core
import Refine.Common.Types.VDoc


newUUID :: IO String
newUUID = show @UUID <$> randomIO

createRepo :: DocRepo RepoHandle
createRepo = do
  repoRoot <- view cfgReposRoot
  docRepoIO $ do
    uuid <- newUUID
    let repoDir = repoRoot </> uuid
    createDirectory repoDir
    pure . RepoHandle . cs $ uuid

createEdit :: RepoHandle -> EditHandle -> VDocVersion 'HTMLCanonical -> DocRepo EditHandle
createEdit repo _base = createInitialEdit repo

createInitialEdit :: RepoHandle -> VDocVersion 'HTMLCanonical -> DocRepo EditHandle
createInitialEdit repo vers = do
  repoRoot <- view cfgReposRoot
  docRepoIO $ do
    uuid <- newUUID
    let repoDir   = repoRoot </> (repo ^. unRepoHandle . to cs)
        editFile = repoDir </> uuid
    ST.writeFile editFile (vdocVersionToST vers)
    pure . EditHandle $ cs uuid

getVersion :: RepoHandle -> EditHandle -> DocRepo (VDocVersion 'HTMLCanonical)
getVersion repo vers = do
  repoRoot <- view cfgReposRoot
  docRepoIO $ do
    let repoDir   = repoRoot </> (repo ^. unRepoHandle . to cs)
        editFile = repoDir </> (vers ^. unEditHandle . to cs)
    vdocVersionFromST <$> ST.readFile editFile

-- | Get all edits that are directly based on a given edit.
--
-- TODO: Implement
getChildEdits :: RepoHandle -> EditHandle -> DocRepo [EditHandle]
getChildEdits _repo _edit = pure []
