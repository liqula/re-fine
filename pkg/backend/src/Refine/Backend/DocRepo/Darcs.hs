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

import Control.Arrow ((&&&), second)
import Control.Exception
import Control.Lens ((^.), to, view)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions
import Data.Text.IO as ST
import Data.UUID
import qualified Data.ByteString.Char8 as BC ( unpack )
import qualified Data.Map as Map
import System.Directory hiding (makeAbsolute)
import System.FilePath ((</>))
import System.Random

import Darcs.UI.Commands        as Darcs (commandCommand)
import Darcs.UI.Commands.Add    as Darcs (add)
import Darcs.UI.Commands.Clone  as Darcs (clone)
import Darcs.UI.Commands.Init   as Darcs (initializeCmd)
import Darcs.UI.Commands.Pull   as Darcs (pull)
import Darcs.UI.Commands.Record as Darcs (record, RecordConfig(..))
import Darcs.UI.Options.Flags (DarcsFlag(WorkRepoDir, PatchName, All, UpToPatch))
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

import Refine.Backend.Config
import Refine.Backend.DocRepo.Core
import Refine.Common.Types.VDoc
import Refine.Prelude ((<@>))

{-
The current implementation does not support multithreaded applications.

The result of the experiment: the high level darcs commands changes
the current working directories. Having this property of the current
implementation, if a second thread starts to manipulate a document
the working directories would be messed up.

Refactoring is needed. The proposed solution is to use the
`withRepositoryDirectory NoUseCache repoDir $ RepoJob` combinator
and assemble the version in some temporary directory if it is possible.
-}

-- * Helpers

newUUID :: IO String
newUUID = show @UUID <$> randomIO

-- | Darcs changes the current directory, which has to be set
-- back after the operation.
darcsCommands :: IO a -> IO a
darcsCommands darcs = do
  cwd <- getCurrentDirectory
  !x <- darcs `finally` setCurrentDirectory cwd
  pure x

withTempDir :: FilePath -> IO a -> IO a
withTempDir fp m = m `finally` removeDirectoryRecursive fp

createCloneTmp :: FilePath -> FilePath -> IO (AbsolutePath, AbsolutePath, FilePath)
createCloneTmp repoDir uuid = do
  tmp  <- getTemporaryDirectory
  let cloneTmp = tmp </> uuid
  (,,) <$> ioAbsolute repoDir <*> ioAbsolute cloneTmp <@> cloneTmp


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
  , lookfor         = LookFor YesLookForAdds YesLookForReplaces YesLookForMoves
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


-- * DocRepo implementation

createRepo :: DocRepo RepoHandle
createRepo = do
  repoRoot <- view cfgReposRoot
  docRepoIO $ do
    uuid <- newUUID
    let repoDir = repoRoot </> uuid
    createDirectory repoDir

    repoDirA <- ioAbsolute repoDir
    darcsCommands $ do
      let repoDirs = (repoDirA, repoDirA)
      Darcs.initializeCmd repoDirs [WorkRepoDir repoDir] []

    pure . RepoHandle . cs $ uuid

contentFile :: String
contentFile = "content"

createInitialEdit :: RepoHandle -> VDocVersion 'HTMLCanonical -> DocRepo EditHandle
createInitialEdit repo vers = do
  repoRoot <- view cfgReposRoot
  docRepoIO $ do
    let repoDir  = repoRoot </> (repo ^. unRepoHandle . to cs)
    uuid <- newUUID

    ST.writeFile (repoDir </> contentFile) (vdocVersionToST vers)

    darcsCommands $ do
      repoDirA <- ioAbsolute repoDir
      setCurrentDirectory repoDir
      addCmd (repoDirA, repoDirA) [] [contentFile]
      recordCmd (repoDirA, repoDirA) (recordConfig uuid) []

    pure . EditHandle $ cs uuid


getVersion :: RepoHandle -> EditHandle -> DocRepo (VDocVersion 'HTMLCanonical)
getVersion repo vers = do

  -- Clone the repository to a temp dir reverting the result at some state
  -- Read the version out.

  repoRoot <- view cfgReposRoot
  let editVersion = vers ^. unEditHandle . to cs

  docRepoIO $ do
    darcsCommands $ do
      uuid <- newUUID
      let repoDir  = repoRoot </> (repo ^. unRepoHandle . to cs)
      (repoDirA, cloneTmpA, cloneTmp) <- createCloneTmp repoDir uuid

      cloneCmd (repoDirA, cloneTmpA) [UpToPatch editVersion] [repoDir, cloneTmp]
      withTempDir cloneTmp $ do
        !ver <- vdocVersionFromST <$> ST.readFile (cloneTmp </> contentFile)
        pure ver


createEdit :: RepoHandle -> EditHandle -> VDocVersion 'HTMLCanonical -> DocRepo EditHandle
createEdit repo base vers = do

  -- Clone the repo to the temporary dir
  -- do the changes at the given version
  -- pull the new patches from the new repo
  -- remove the temporary directory

  let editBase = base ^. unEditHandle . to cs

  repoRoot <- view cfgReposRoot

  uuid <- docRepoIO $ do
    cwd <- getCurrentDirectory
    uuid <- newUUID
    let repoDir = repoRoot </> (repo ^. unRepoHandle . to cs)
    darcsCommands $ do
      (repoDirA, cloneTmpA, cloneTmp) <- createCloneTmp repoDir uuid

      cloneCmd (repoDirA, cloneTmpA) [PatchName editBase] [repoDir, cloneTmp]
      withTempDir cloneTmp $ do
        ST.writeFile (cloneTmp </> contentFile) (vdocVersionToST vers)

        setCurrentDirectory cloneTmp
        recordCmd (repoDirA, repoDirA) (recordConfig uuid) []
        -- PatchName is necessary to fill out patchinfo fields automatically.
        setCurrentDirectory (cwd </> repoDir)
        pullCmd (repoDirA, cloneTmpA) [All] [cloneTmp]

    pure uuid

  pure . EditHandle $ cs uuid


-- | Get all edits that are directly based on a given edit.
getChildEdits :: RepoHandle -> EditHandle -> DocRepo [EditHandle]
getChildEdits repository vers = do
  repoRoot <- view cfgReposRoot

  docRepoIO $ do
    let repoDir  = repoRoot </> (repository ^. unRepoHandle . to cs)
    withRepositoryDirectory NoUseCache repoDir $ RepoJob (\repo -> do
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

      pure . fmap (EditHandle . cs . flip lookupE labelMap)
           $ lookupL (lookupE editVersion uuidMap) depMap)

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

    editVersion :: String
    editVersion = vers ^. unEditHandle . to cs

    pruneRepo r = case matchingHead [Match.OnePatch editVersion] r of
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
