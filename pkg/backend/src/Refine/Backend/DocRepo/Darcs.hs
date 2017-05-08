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

module Refine.Backend.DocRepo.Darcs
  ( createRepo
  , createInitialEdit
  , createEdit
  , getVersion
  , getChildEdits
  ) where

import Control.Lens ((^.), (%~), to, makeLenses)
import Data.String.Conversions
import Data.UUID
import System.FilePath ((</>))
import System.Random
import Refine.Backend.DocRepo.Core
import Refine.Common.Types.VDoc


-- * Helpers

newUUID :: IO String
newUUID = show @UUID <$> randomIO

modifyFile :: (Read a, Show a) => FilePath -> (a -> a) -> IO ()
modifyFile n f = do
    r <- readFile n
    length r `seq` writeFile n (show . f $ read r)

-- * DocRepo implementation

data Repo = Repo
    { _parents :: [(EditHandle, EditHandle)]
    , _docs    :: [(EditHandle, ST)]
    }
    deriving (Read, Show)

makeLenses ''Repo

emptyRepo :: Repo
emptyRepo = Repo [] []

createRepo :: DocRepo RepoHandle
createRepo = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    repoUuid <- newUUID
    writeFile (reposRoot </> repoUuid) $ show emptyRepo
    pure . RepoHandle . cs $ repoUuid

-- | FIXME: this may be a special case of 'createEdit'.
createInitialEdit :: RepoHandle -> VDocVersion -> DocRepo EditHandle
createInitialEdit repo vers = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let repoDir = reposRoot </> (repo ^. unRepoHandle . to cs)
    patchUuid <- newUUID  -- (it is vital to do this outside of 'withForkProcCurrentDirectory'!)
    let eh = EditHandle . cs $ patchUuid
    modifyFile repoDir $ docs %~ ((eh, vers ^. unVDocVersion):)
    pure eh

-- | Clone the repository to a temp dir, reverting the result at some state.  Read the version out.
getVersion :: RepoHandle -> EditHandle -> DocRepo VDocVersion
getVersion repo baseEdit = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let upstreamRepoDir = reposRoot </> (repo ^. unRepoHandle . to cs)
    r <- readFile upstreamRepoDir
    let v = head [v' | (eh, v') <- read r ^. docs, eh == baseEdit]
    length (cs v :: String) `seq` pure (VDocVersion v)

-- | Clone the repo to the temporary dir; do the changes at the given version; pull the new patches from
-- the new repo; remove the temporary directory.
createEdit :: RepoHandle -> EditHandle -> VDocVersion -> DocRepo EditHandle
createEdit repo baseEdit newVersion = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let upstreamRepoDir = reposRoot </> (repo ^. unRepoHandle . to cs)
    newPatch <- newUUID
    let eh = EditHandle . cs $ newPatch
    modifyFile upstreamRepoDir $ (docs %~ ((eh, newVersion ^. unVDocVersion):)) . (parents %~ ((baseEdit, eh):))
    pure eh

-- | Get all edits that are directly based on a given edit.
getChildEdits :: RepoHandle -> EditHandle -> DocRepo [EditHandle]
getChildEdits repository baseHandle = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let repoDir  = reposRoot </> (repository ^. unRepoHandle . to cs)
    r <- readFile repoDir
    let l = [eh | (bh, eh) <- read r ^. parents, bh == baseHandle]
    length l `seq` pure l

