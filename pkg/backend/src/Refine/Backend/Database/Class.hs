module Refine.Backend.Database.Class where

import Refine.Backend.Database.Tree
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
  vdocRepoOfEdit     :: ID Edit -> db (ID VDocRepo)

  -- * Repo
  createRepo         :: DocRepo.RepoHandle -> DocRepo.EditHandle -> db VDocRepo
  getRepo            :: ID VDocRepo -> db VDocRepo
  getRepoFromHandle  :: DocRepo.RepoHandle -> db VDocRepo
  getRepoHandle      :: ID VDocRepo -> db DocRepo.RepoHandle
  getEditIDs         :: ID VDocRepo -> db [ID Edit]

  -- * Edit
  createEdit         :: ID VDocRepo -> DocRepo.EditHandle -> db Edit
  getEdit            :: ID Edit -> db Edit
  getEditFromHandle  :: DocRepo.EditHandle -> db Edit
  getEditHandle      :: ID Edit -> db DocRepo.EditHandle
  editNotes          :: ID Edit -> db [ID Note]
  editQuestions      :: ID Edit -> db [ID Question]
  editDiscussions    :: ID Edit -> db [ID Discussion]

  -- * Repo and edit
  editVDocRepo      :: ID Edit -> db (ID VDocRepo)

  -- * Note
  createNote         :: ID Edit -> Create Note -> db Note
  getNote            :: ID Note -> db Note

  -- * Question
  createQuestion     :: ID Edit     -> Create Question -> db Question
  getQuestion        :: ID Question -> db Question

  -- * Answer
  createAnswer       :: ID Question -> Create Answer -> db Answer
  getAnswer          :: ID Answer   -> db Answer
  answersOfQuestion  :: ID Question -> db [Answer]

  -- * Discussion
  createDiscussion   :: ID Edit    -> Create Discussion -> db Discussion
  getDiscussion      :: ID Discussion -> db Discussion
  statementsOfDiscussion :: ID Discussion -> db [ID Statement]
  discussionOfStatement  :: ID Statement  -> db (ID Discussion)

  -- * Statement
  createStatement      :: ID Statement -> Create Statement -> db Statement
  getStatement         :: ID Statement -> db Statement


-- * composite db queries

handlesForEdit
  :: (Monad db, Database db)
  => ID Edit -> db (DocRepo.RepoHandle, DocRepo.EditHandle)
handlesForEdit pid = do
  rid <- editVDocRepo pid
  (,) <$> getRepoHandle rid <*> getEditHandle pid

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
  <*> (fmap (buildTree _statementParent _statementID) . mapM getStatement =<< statementsOfDiscussion did)

editComments
  :: (Monad db, Database db)
  => ID Edit -> db [Comment]
editComments pid = do
  notes       <- mapM getNote =<< editNotes pid
  questions   <- mapM Refine.Backend.Database.Class.compositeQuestion =<< editQuestions pid
  discussions <- mapM Refine.Backend.Database.Class.compositeDiscussion =<< editDiscussions pid
  pure $ concat
    [ CommentNote       <$> notes
    , CommentQuestion   <$> questions
    , CommentDiscussion <$> discussions
    ]
