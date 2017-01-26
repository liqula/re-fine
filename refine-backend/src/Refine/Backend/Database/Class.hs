module Refine.Backend.Database.Class where

import Refine.Backend.DocRepo.Core as DocRepo
import Refine.Common.Types.Comment
import Refine.Common.Types.Prelude
import Refine.Common.Types.VDoc


class Database db where

  -- * VDoc
  listVDocs          :: db [ID VDoc]
  createVDoc         :: Create VDoc -> VDocRepo -> db VDoc
  getVDoc            :: ID VDoc -> db VDoc
  vdocRepo           :: ID VDoc -> db (ID VDocRepo)

  -- * Repo
  createRepo         :: DocRepo.RepoHandle -> DocRepo.PatchHandle -> db VDocRepo
  getRepo            :: ID VDocRepo -> db VDocRepo
  getRepoFromHandle  :: DocRepo.RepoHandle -> db VDocRepo
  getRepoHandle      :: ID VDocRepo -> db DocRepo.RepoHandle
  getPatchIDs        :: ID VDocRepo -> db [ID Patch]

  -- * Patch
  createPatch        :: ID VDocRepo -> DocRepo.PatchHandle -> db Patch
  getPatch           :: ID Patch -> db Patch
  getPatchFromHandle :: DocRepo.PatchHandle -> db Patch
  getPatchHandle     :: ID Patch -> db DocRepo.PatchHandle
  patchNotes         :: ID Patch -> db [ID Note]
  patchQuestions     :: ID Patch -> db [ID Question]
  patchDiscussions   :: ID Patch -> db [ID Discussion]

  -- * Repo and patch
  patchVDocRepo      :: ID Patch -> db (ID VDocRepo)

  -- * Note
  createNote         :: ID Patch -> Create Note -> db Note
  getNote            :: ID Note  -> db Note

  -- * Question
  createQuestion     :: ID Patch    -> Create Question -> db Question
  getQuestion        :: ID Question -> db Question

  -- * Answer
  createAnswer       :: ID Question -> Create Answer -> db Answer
  getAnswer          :: ID Answer -> db Answer
  answersOfQuestion  :: ID Question -> db [Answer]

  -- * Discussion
  createDiscussion   :: ID Patch    -> Create Discussion -> db Discussion
  getDiscussion      :: ID Discussion -> db Discussion
  statementsOfDiscussion :: ID Discussion -> db [ID Statement]

  -- * Statement
  createStatement      :: ID Statement -> Create Statement -> db Statement
  getStatement         :: ID Statement  -> db Statement


-- * composite db queries

handlesForPatch
  :: (Monad db, Database db)
  => ID Patch -> db (DocRepo.RepoHandle, DocRepo.PatchHandle)
handlesForPatch pid = do
  rid <- patchVDocRepo pid
  (,) <$> getRepoHandle rid <*> getPatchHandle pid

compositeQuestion
  :: (Monad db, Database db)
  => ID Question -> db CompositeQuestion
compositeQuestion qid =
  CompositeQuestion <$> getQuestion qid <*> answersOfQuestion qid

compositeDiscussion
  :: (Monad db, Database db)
  => ID Discussion -> db CompositeDiscussion
compositeDiscussion did = CompositeDiscussion
  <$> getDiscussion did
  <*> (mapM getStatement =<< statementsOfDiscussion did)

patchComments
  :: (Monad db, Database db)
  => ID Patch -> db [Comment]
patchComments pid = do
  notes       <- mapM getNote =<< patchNotes pid
  questions   <- mapM Refine.Backend.Database.Class.compositeQuestion =<< patchQuestions pid
  discussions <- mapM Refine.Backend.Database.Class.compositeDiscussion =<< patchDiscussions pid
  pure $ concat
    [ CommentNote       <$> notes
    , CommentQuestion   <$> questions
    , CommentDiscussion <$> discussions
    ]
