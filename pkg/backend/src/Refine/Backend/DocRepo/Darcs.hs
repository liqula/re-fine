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

import Control.Arrow ((&&&), second)
import Control.Lens ((^.), to)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions
import Data.Text.IO as ST
import Data.UUID
import qualified Data.ByteString.Char8 as BC ( unpack )
import qualified Data.Map as Map
import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Process as Proc
import qualified System.Exit as Proc
import System.Random

import Darcs.UI.Commands        as Darcs (commandCommand)
import Darcs.UI.Commands.Add    as Darcs (add)
import Darcs.UI.Commands.Clone  as Darcs (clone)
import Darcs.UI.Commands.Init   as Darcs (initializeCmd)
import Darcs.UI.Commands.Pull   as Darcs (pull)
import Darcs.UI.Commands.Record as Darcs (record, RecordConfig(..))
import Darcs.UI.Options.Flags (DarcsFlag(WorkRepoDir, All, UpToPatch))
import Darcs.Util.Path hiding (getCurrentDirectory, setCurrentDirectory)
import Darcs.UI.Options.All
  ( UseCache(..)
  , IncludeBoring(..)
  , SetScriptsExecutable(..)
  , UMask(..)
  , UseIndex(..)
  , Compression(..)
  , Logfile(..)
  , Verbosity(..)
  , DiffAlgorithm(..)
  , WithContext(..)
  , AskLongComment(..)
  , TestChanges(..)
  , LookFor(..)
  , LookForAdds(..)
  , LookForReplaces(..)
  , LookForMoves(..)
  )

import Darcs.Repository.Job (withRepositoryDirectory, RepoJob(..))
import Darcs.Repository (readRepo)
import Darcs.Patch.Set
import Darcs.Patch.PatchInfoAnd
import Darcs.Patch.Witnesses.Ordered
import Darcs.Patch.Info
import qualified Darcs.Patch.Match as Match (MatchFlag(..))
import Darcs.Patch.Witnesses.Sealed
import Darcs.Patch.Named.Wrapped hiding (patch2patchinfo)
import Darcs.Patch.Named as Named (Named(..), patch2patchinfo)
import Darcs.Patch.Depends
import Darcs.Patch.Choices
import Darcs.UI.Commands.Unrecord (matchingHead)

import Data.GraphViz
import Data.GraphViz.Algorithms ( transitiveReduction )
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.Graph ( Graph(..), mkGraph, LNode, UEdge )
import Data.Graph.Inductive.PatriciaTree ( Gr )

import Refine.Backend.DocRepo.Core
import Refine.Common.Types.VDoc


-- * Helpers

newUUID :: IO String
newUUID = show @UUID <$> randomIO

-- | 'withCurrentDir' creates a new POSIX process to run the action in isolation from other threads.
--
-- Reason: the high-level darcs commands change the current working directory. If a second thread
-- starts to manipulate a separate document the working directories would be messed up.  Note that
-- using `withRepositoryDirectory` does not help either: some of the darcs functions that live on
-- the other side of this function call from the cli may also use or touch the current working
-- directory.
--
-- FIXME: consolidate with 'Refine.Backend.Test.Util.withTempCurrentDirectory'.
withForkProcCurrentDir :: FilePath -> IO () -> IO ()
withForkProcCurrentDir wd action = do
  ph     <- Proc.forkProcess (Dir.withCurrentDirectory wd action)
  status <- Proc.getProcessStatus True False ph
  case status of
    Just (Proc.Exited Proc.ExitSuccess) -> pure ()
    bad -> error $ "withForkProcCurrentDir: " <> show bad

-- | Run a darcs action wrapped in 'withForkProcCurrentDir'.  Then run an action that gets the
-- filepath to the current working directory of the darcs action for inspection.
withDarcsAction :: (FilePath -> IO ()) -> (FilePath -> IO a) -> IO a
withDarcsAction darcsAction inspectAction =
  withSystemTempDirectory "refine.darcs" $ \wd -> do
    withForkProcCurrentDir wd (darcsAction wd)
    inspectAction wd


-- * High level darcs commands

addCmd, cloneCmd, pullCmd
  :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()

recordCmd
  :: (AbsolutePath, AbsolutePath) -> RecordConfig -> [String] -> IO ()

addCmd    = Darcs.commandCommand Darcs.add
cloneCmd  = Darcs.commandCommand Darcs.clone
recordCmd = Darcs.commandCommand Darcs.record
pullCmd   = Darcs.commandCommand Darcs.pull

recordConfig :: String -> Darcs.RecordConfig
recordConfig uuid = Darcs.RecordConfig
  { patchname       = Just uuid
  , author          = Just "refine"
  , testChanges     = NoTestChanges
  , interactive     = Just False
  , pipe            = False
  , askDeps         = False
  , askLongComment  = Just NoEditLongComment
  , lookfor         = LookFor NoLookForAdds NoLookForReplaces NoLookForMoves
  , _workingRepoDir = Nothing
  , withContext     = NoContext
  , diffAlgorithm   = PatienceDiff
  , verbosity       = Verbose
  , logfile         = Logfile Nothing False
  , compress        = NoCompression
  , useIndex        = UseIndex
  , umask           = NoUMask
  , sse             = NoSetScriptsExecutable
  , includeBoring   = NoIncludeBoring
  , useCache        = NoUseCache
  }

contentFilePath :: FilePath
contentFilePath = "content"
  -- FIXME: consider using pkg/backend/src/Refine/Backend/DocRepo/HtmlFileTree.hs here.  we should
  -- benchmark both options and then decide which one to use.

tempRepo :: FilePath
tempRepo = "tmprepo"

refineSemaphore :: FilePath
refineSemaphore = "refine.semaphore"

-- * DocRepo implementation

createRepo :: DocRepo RepoHandle
createRepo = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    repoUuid <- newUUID
    Dir.createDirectoryIfMissing True (reposRoot </> repoUuid)
    withForkProcCurrentDir (reposRoot </> repoUuid) $ do
      repoDirA <- ioAbsolute "."
      Darcs.initializeCmd (repoDirA, repoDirA) [WorkRepoDir "."] []
    pure . RepoHandle . cs $ repoUuid


-- | FIXME: this may be a special case of 'createEdit'.
createInitialEdit :: RepoHandle -> VDocVersion -> DocRepo EditHandle
createInitialEdit repo vers = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let repoDir = reposRoot </> (repo ^. unRepoHandle . to cs)
    patchUuid <- newUUID  -- (it is vital to do this outside of 'withForkProcCurrentDirectory'!)
    withForkProcCurrentDir repoDir $ do
      ST.writeFile contentFilePath (vers ^. unVDocVersion)
      repoDirA <- ioAbsolute "."
      addCmd (repoDirA, repoDirA) [] [contentFilePath]
      recordCmd (repoDirA, repoDirA) (recordConfig patchUuid) []

    pure . EditHandle . cs $ patchUuid


-- | Clone the repository to a temp dir, reverting the result at some state.  Read the version out.
getVersion :: RepoHandle -> EditHandle -> DocRepo VDocVersion
getVersion repo baseEdit = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let upstreamRepoDir = reposRoot </> (repo ^. unRepoHandle . to cs)
        basePatch = baseEdit ^. unEditHandle . to cs

        darcsAction _ = do
          upstreamRepoDirA <- ioAbsolute upstreamRepoDir
          hereRepoDirA <- ioAbsolute "."
          cloneCmd (upstreamRepoDirA, hereRepoDirA) [UpToPatch basePatch] [upstreamRepoDir, tempRepo]

        inspectAction wd = do
          !vers <- vdocVersionFromST <$> ST.readFile (wd </> tempRepo </> contentFilePath)
          pure vers

    withDarcsAction darcsAction inspectAction


-- | Clone the repo to the temporary dir; do the changes at the given version; pull the new patches from
-- the new repo; remove the temporary directory.
createEdit :: RepoHandle -> EditHandle -> VDocVersion -> DocRepo EditHandle
createEdit repo baseEdit newVersion = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let upstreamRepoDir = reposRoot </> (repo ^. unRepoHandle . to cs)
        basePatch = baseEdit ^. unEditHandle . to cs
    newPatch <- newUUID

    let darcsAction _ = do
          upstreamRepoDirA <- ioAbsolute upstreamRepoDir
          hereRepoDirA@(toFilePath -> hereRepoDir) <- ioAbsolute "."
          cloneCmd (upstreamRepoDirA, hereRepoDirA) [UpToPatch basePatch] [upstreamRepoDir, tempRepo]

          ST.writeFile contentFilePath $ newVersion ^. unVDocVersion
          recordCmd (hereRepoDirA, hereRepoDirA) (recordConfig newPatch) [contentFilePath]

          Dir.setCurrentDirectory upstreamRepoDir
          pullCmd (upstreamRepoDirA, hereRepoDirA) [All] [hereRepoDir </> tempRepo]

        inspectAction _ = pure . EditHandle . cs $ newPatch

    withDarcsAction darcsAction inspectAction


-- | Get all edits that are directly based on a given edit.
--
-- In case of clonflicting patches the darcs select automatically
-- one patch as a parent patch. Which renders this function an inappropiate
-- information source.
--
-- This is a reference implementation for the further investigation.
getChildEdits :: RepoHandle -> EditHandle -> DocRepo [EditHandle]
getChildEdits repository baseHandle = do
  docRepoIOWithReposRoot $ \reposRoot -> do
    let repoDir  = reposRoot </> (repository ^. unRepoHandle . to cs)

        darcsAction :: FilePath -> IO ()
        darcsAction wd = do
          Dir.setCurrentDirectory repoDir
          withRepositoryDirectory NoUseCache "." $ RepoJob (\repo -> do
            -- Copied from "Darcs.UI.Command.ShowDependcies" (and heavily mutated).
            Sealed2 r <- readRepo repo >>= pruneRepo
            let rFL    = newset2FL r
                deps   = getDeps (removeInternalFL $ mapFL_FL hopefully rFL) rFL
                dGraph = transitiveReduction . graphToDot nodeLabeledParams $ makeGraph deps
                depMap = Map.unionsWith (++)
                       . fmap (uncurry Map.singleton . (toNode &&& (pure . fromNode)))
                       . edgeStmts
                       . graphStatements
                       $ dGraph
                uuids  = fmap (nodeID &&& (uuid . nodeAttributes))
                       . nodeStmts
                       . graphStatements
                       $ dGraph
                labelMap = Map.fromList uuids
                uuidMap  = Map.fromList $ fmap flipPair uuids

            result <- pure . fmap (EditHandle . cs . flip lookupE labelMap)
                   $ lookupL (lookupE basePatch uuidMap) depMap

            ST.writeFile (wd </> refineSemaphore) . cs . show $ result)

        inspectAction wd = do
          !edits <- read . cs <$> ST.readFile (wd </> refineSemaphore)
          pure edits

    withDarcsAction darcsAction inspectAction

  where
    lookupL k m = fromMaybe [] $ Map.lookup k m

    lookupE k m = fromMaybe (error $ "impossible: " <> show k) $ Map.lookup k m

    flipPair (f, s) = (s, f)

    isLabel (Label _) = True
    isLabel _         = False

    getLabel :: Attribute -> String
    getLabel (Label (StrLabel t)) = cs t
    getLabel _ = error "getChildEdits getLabel: impossible"

    uuid = head . map getLabel . filter isLabel

    basePatch :: String
    basePatch = baseHandle ^. unEditHandle . to cs

    pruneRepo r = case matchingHead [Match.OnePatch basePatch] r of
                    _ :> ps' -> (pure . seal2) $ PatchSet NilRL (reverseFL ps')

    nodeLabeledParams :: GraphvizParams n String el () String
    nodeLabeledParams =
            defaultParams { globalAttributes =
                                [GraphAttrs {attrs = []}]
                          , fmtNode = \(_,l) ->
                                [ toLabel l ]
                          }

-- * Graph

type DepsGraph = Gr String ()

-- | Copied from "Darcs.UI.Command.ShowDependcies".
makeGraph :: [SPatchAndDeps p] -> DepsGraph
makeGraph = uncurry mkGraph . second concat . unzip . map mkNodeWithEdges
    where
    mkNodeWithEdges :: SPatchAndDeps p -> (LNode String, [UEdge])
    mkNodeWithEdges (Sealed2 father, Sealed2 childs) = (mkLNode father,mkUEdges)
        where
            mkNode :: LabelledPatch (Named p) wX wY -> Int
            mkNode = fromInteger . getLabelInt . label
            mkUEdge :: [UEdge] -> LabelledPatch (Named p) wX wY -> [UEdge]
            mkUEdge les child = (mkNode father, mkNode child,()) : les
            mkLabel :: LabelledPatch (Named p) wX wY -> String
            mkLabel = BC.unpack . _piName . patch2patchinfo . lpPatch
            mkLNode :: LabelledPatch (Named p) wX wY -> LNode String
            mkLNode p = (mkNode p, mkLabel p)
            mkUEdges :: [UEdge]
            mkUEdges = foldlFL mkUEdge [] childs
