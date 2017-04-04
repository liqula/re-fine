{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Entity where

import Control.Lens ((^.), to, view)
import Control.Monad (forM_, void)
import Control.Monad.Reader (ask)
import Data.Functor.Infix ((<$$>))
import Data.List ((\\))
import Data.String.Conversions (ST)
import Data.Typeable
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Lentil.Core (entityLens)
import Lentil.Types as L

import qualified Refine.Backend.Database.Class as C
import           Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import           Refine.Backend.Database.Types
import qualified Refine.Backend.DocRepo.Core as DocRepo
import           Refine.Backend.User.Core as Users (Login, LoginId, fromUserID)
import           Refine.Common.Types
import           Refine.Prelude (nothingToError)

-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep VDoc       = S.VDoc
type instance S.EntityRep Edit       = S.Edit
type instance S.EntityRep VDocRepo   = S.Repo
type instance S.EntityRep Note       = S.Note
type instance S.EntityRep Question   = S.Question
type instance S.EntityRep Discussion = S.Discussion
type instance S.EntityRep Answer     = S.Answer
type instance S.EntityRep Statement  = S.Statement
type instance S.EntityRep User       = Users.Login
type instance S.EntityRep Group      = S.Group
type instance S.EntityRep (Process a) = S.Process
type instance S.EntityRep CollaborativeEdit = S.CollabEditProcess
type instance S.EntityRep Aula              = S.AulaProcess

type instance S.ProcessDataRep CollaborativeEdit = S.CollabEditProcess
type instance S.ProcessDataRep Aula              = S.AulaProcess

type instance S.ProcessDataConnectionRep CollaborativeEdit = S.ProcessOfCollabEdit
type instance S.ProcessDataConnectionRep Aula              = S.ProcessOfAula

{-
Reading the domain structured datatypes is not a problem,
as there is no big difference, in accessing the parts, and
combine them via an applicative functor, producing an lazy value.

The problem arises when we add some information to the computed
value in an another pure computation. The changes in the value
will be lost, as there is no triger mechanism.

If we want to address this problem, we need to use lenses over
some ids, to navigate deeper in the data structures.

If the lens only a getter, the result functor won't run the update
Whenever we operate on some data we need to load the current
version from the db.

Instead of saving the whole value in the database, a generic
diff algorithm could be used.

https://hackage.haskell.org/package/gdiff
-}

-- * Helpers

idNotFound :: (Typeable d) => ID d -> DB a
idNotFound i = notFound $ unwords [show $ typeOf i, show i, "is not found."]

-- FIXME: Better error messages
unique :: [a] -> DB a
unique [x] = pure x
unique []  = notFound "Unique value is not found"
unique _   = notUnique "Value is not unique"

getEntity :: (ToBackendKey SqlBackend (S.EntityRep e), Typeable e)
          => ID e -> DB (S.EntityRep e)
getEntity eid = do
  e <- liftDB . get $ S.idToKey eid
  maybe (idNotFound eid) pure e

-- | Access the key like field in an entity and convert the key value
-- to the Domain ID.
--
-- Example:
--   * foreignKeyField S.PCComment (Entity pcid (S.PC pid cid)) == cid
--   * foreignKeyField S.PCEdit    (Entity pcid (S.PC pid cid)) == pid
foreignKeyField
  :: ToBackendKey SqlBackend (S.EntityRep a)
  => (b -> Key (S.EntityRep a)) -> Database.Persist.Entity b -> ID a
foreignKeyField column = S.keyToId . column . entityVal

-- NOTES: How to handle associations? What to update, what to keep?
vDocToRecord :: VDoc -> DB S.VDoc
vDocToRecord (VDoc _i t a r) = pure (S.VDoc t a (S.idToKey r))

updateVDoc :: ID VDoc -> VDoc -> DB ()
updateVDoc vid vdoc = do
  record <- vDocToRecord vdoc
  liftDB $ replace (S.idToKey vid) record

vdocDBLens :: EntityLens' DB (ID VDoc) VDoc
vdocDBLens = entityLens vdocEntity

vdocEntity :: L.Entity DB ID VDoc VDoc
vdocEntity = L.Entity getVDoc updateVDoc

-- | Returns the ID of the user who runs the current DB computation.
dbUser :: DB (ID User)
dbUser = do
  DBContext mu _filter <- ask
  nothingToError DBUserNotLoggedIn mu

-- FUTUREWORK: Make dbSelectOpts typesafe.
dbSelectOpts :: DB [SelectOpt entity]
dbSelectOpts = do
  DBContext _mu mfilter <- ask
  pure $ maybe [] filterToSelectOpt mfilter
  where
    filterToSelectOpt = \case
      Limit n -> [LimitTo n]

-- * VDoc

toVDoc :: ID VDoc -> Title -> Abstract -> Key S.Repo -> VDoc
toVDoc vid title abstract repoid = VDoc vid title abstract (S.keyToId repoid)

listVDocs :: DB [ID VDoc]
listVDocs = do
  opts <- dbSelectOpts
  liftDB $ S.keyToId <$$> selectKeysList [] opts

createVDoc :: Create VDoc -> VDocRepo -> DB VDoc
createVDoc pv vr = liftDB $ do
  let svdoc = S.VDoc
        (pv ^. createVDocTitle)
        (pv ^. createVDocAbstract)
        (vr ^. vdocRepoID . to S.idToKey)
  key <- insert svdoc
  void . insert $ S.VR key (vr ^. vdocRepoID . to S.idToKey)
  pure $ S.vDocElim (toVDoc (S.keyToId key)) svdoc

getVDoc :: ID VDoc -> DB VDoc
getVDoc vid = S.vDocElim (toVDoc vid) <$> getEntity vid

vdocRepo :: ID VDoc -> DB (ID VDocRepo)
vdocRepo vid = do
  opts <- dbSelectOpts
  vs <- liftDB $ foreignKeyField S.vRRepository <$$> selectList [S.VRVdoc ==. S.idToKey vid] opts
  unique vs

vdocRepoOfEdit :: ID Edit -> DB (ID VDocRepo)
vdocRepoOfEdit eid = do
  opts <- dbSelectOpts
  unique =<< liftDB
    (foreignKeyField S.rPRepository <$$> selectList [S.RPEdit ==. S.idToKey eid] opts)

-- * Repo

createRepo :: DocRepo.RepoHandle -> DocRepo.EditHandle -> DB VDocRepo
createRepo repoh edith = liftDB $ do
    let desc = "" -- TODO
        motiv = ""
    editkey <- insert $ S.Edit desc cr edith Initial motiv
    repokey <- insert $ S.Repo "title" {- TODO -} repoh editkey
    void  . insert $ S.RP repokey editkey
    pure $ VDocRepo (S.keyToId repokey) (S.keyToId editkey)
  where
    cr :: ChunkRange
    cr = ChunkRange Nothing Nothing  -- TODO


getRepo :: ID VDocRepo -> DB VDocRepo
getRepo vid = S.repoElim toVDocRepo <$> getEntity vid
  where
    toVDocRepo :: ST -> DocRepo.RepoHandle -> Key S.Edit -> VDocRepo
    toVDocRepo _desc _repoHandle pid = VDocRepo vid (S.keyToId pid)

getRepoFromHandle :: DocRepo.RepoHandle -> DB VDocRepo
getRepoFromHandle hndl = do
  opts <- dbSelectOpts
  rs <- liftDB $ selectList [S.RepoRepoHandle ==. hndl] opts
  r <- unique rs
  let rid = S.keyToId $ entityKey r
      toRepo _desc _hdnl repohead = VDocRepo rid (S.keyToId repohead)
  pure $ S.repoElim toRepo (entityVal r)

getRepoHandle :: ID VDocRepo -> DB DocRepo.RepoHandle
getRepoHandle vid = S.repoElim toRepoHandle <$> getEntity vid
  where
    toRepoHandle :: ST -> DocRepo.RepoHandle -> Key S.Edit -> DocRepo.RepoHandle
    toRepoHandle _desc repoHandle _pid = repoHandle

getEditIDs :: ID VDocRepo -> DB [ID Edit]
getEditIDs vid = liftDB $
  foreignKeyField S.rPEdit <$$> selectList [S.RPRepository ==. S.idToKey vid] []

-- * Edit

createEdit :: ID VDocRepo -> DocRepo.EditHandle -> CreateEdit -> DB Edit
createEdit rid edith ce = liftDB $ do
  key <- insert $ S.Edit
            (ce ^. createEditDesc)
            (ce ^. createEditRange)
            edith
            (ce ^. createEditKind)
            (ce ^. createEditMotiv)
  void . insert $ S.RP (S.idToKey rid) key
  let pid = S.keyToId key
  pure $ Edit
    pid
    (ce ^. createEditDesc)
    (ce ^. createEditRange)
    (ce ^. createEditKind)
    (ce ^. createEditMotiv)

getEdit :: ID Edit -> DB Edit
getEdit pid = S.editElim toEdit <$> getEntity pid
  where
    toEdit :: ST -> ChunkRange -> DocRepo.EditHandle -> EditKind -> ST -> Edit
    toEdit desc cr _handle = Edit pid desc cr

getEditFromHandle :: DocRepo.EditHandle -> DB Edit
getEditFromHandle hndl = do
  opts <- dbSelectOpts
  ps <- liftDB $ selectList [S.EditEditHandle ==. hndl] opts
  p <- unique ps
  let toEdit desc cr _hdnl = Edit (S.keyToId $ entityKey p) desc cr
  pure $ S.editElim toEdit (entityVal p)

getEditHandle :: ID Edit -> DB DocRepo.EditHandle
getEditHandle pid = S.editElim toEditHandle <$> getEntity pid
  where
    toEditHandle :: ST -> ChunkRange -> DocRepo.EditHandle -> EditKind -> ST -> DocRepo.EditHandle
    toEditHandle _desc _cr handle _kind _motiv = handle

editNotes :: ID Edit -> DB [ID Note]
editNotes pid = do
  opts <- dbSelectOpts
  liftDB $
    foreignKeyField S.pNNote <$$> selectList [S.PNEdit ==. S.idToKey pid] opts

editQuestions :: ID Edit -> DB [ID Question]
editQuestions pid = do
  opts <- dbSelectOpts
  liftDB $
    foreignKeyField S.pQQuestion <$$> selectList [S.PQEdit ==. S.idToKey pid] opts

editDiscussions :: ID Edit -> DB [ID Discussion]
editDiscussions pid = do
  opts <- dbSelectOpts
  liftDB $
    foreignKeyField S.pDDiscussion <$$> selectList [S.PDEdit ==. S.idToKey pid] opts

setEditChild :: ID Edit -> ID Edit -> DB ()
setEditChild parent child = liftDB $ do
  void . insert $ S.PC (S.idToKey parent) (S.idToKey child)

getEditChildren :: ID Edit -> DB [ID Edit]
getEditChildren parent = do
  opts <- dbSelectOpts
  liftDB $ do
    foreignKeyField S.pCChild <$$> selectList [S.PCParent ==. S.idToKey parent] opts

-- * Repo and edit

editVDocRepo :: ID Edit -> DB (ID VDocRepo)
editVDocRepo pid = do
  opts <- dbSelectOpts
  rs <- liftDB $ foreignKeyField S.rPRepository <$$> selectList [S.RPEdit ==. S.idToKey pid] opts
  unique rs

registerEdit :: ID VDocRepo -> ID Edit -> DB ()
registerEdit rid pid = void . liftDB . insert $ S.RP (S.idToKey rid) (S.idToKey pid)

-- * Note

-- TODO: Use the _lid
toNote :: ID Note -> ST -> Bool -> ChunkRange -> LoginId -> Note
toNote nid desc public range _lid = Note nid desc public range

createNote :: ID Edit -> Create Note -> DB Note
createNote pid note = do
  userId <- dbUser
  liftDB $ do
    let snote = S.Note
          (note ^. createNoteText)
          (note ^. createNotePublic)
          (note ^. createNoteRange)
          (fromUserID userId)
    key <- insert snote
    void . insert $ S.PN (S.idToKey pid) key
    pure $ S.noteElim (toNote (S.keyToId key)) snote

getNote :: ID Note -> DB Note
getNote nid = S.noteElim (toNote nid) <$> getEntity nid


-- * Question

-- TODO: User lid
toQuestion :: ID Question -> ST -> Bool -> Bool -> ChunkRange -> LoginId -> Question
toQuestion qid text answ pblc range _lid = Question qid text answ pblc range

createQuestion :: ID Edit -> Create Question -> DB Question
createQuestion pid question = do
  userId <- dbUser
  liftDB $ do
    let squestion = S.Question
          (question ^. createQuestionText)
          False -- Not answered
          (question ^. createQuestionPublic)
          (question ^. createQuestionRange)
          (fromUserID userId)
    key <- insert squestion
    void . insert $ S.PQ (S.idToKey pid) key
    pure $ S.questionElim (toQuestion (S.keyToId key)) squestion

getQuestion :: ID Question -> DB Question
getQuestion qid = S.questionElim (toQuestion qid) <$> getEntity qid


-- * Discussion

-- TODO: Login ID
toDiscussion :: ID Discussion -> Bool -> ChunkRange -> LoginId -> Discussion
toDiscussion did pblc range _lid = Discussion did pblc range

saveStatement :: ID Discussion -> S.Statement -> SQLM Statement
saveStatement did sstatement = do
  key <- insert sstatement
  void . insert $ S.DS (S.idToKey did) key
  pure $ S.statementElim (toStatement (S.keyToId key)) sstatement

createDiscussion :: ID Edit -> Create Discussion -> DB Discussion
createDiscussion pid disc = do
  userId <- dbUser
  liftDB $ do
    let sdiscussion = S.Discussion
          (disc ^. createDiscussionPublic)
          (disc ^. createDiscussionRange)
          (fromUserID userId)
    key <- insert sdiscussion
    let did = S.keyToId key
    void . insert $ S.PD (S.idToKey pid) key
    let sstatement = S.Statement
          (disc ^. createDiscussionStatementText)
          Nothing -- Top level node
    void $ saveStatement did sstatement
    pure $ S.discussionElim (toDiscussion did) sdiscussion

getDiscussion :: ID Discussion -> DB Discussion
getDiscussion did = S.discussionElim (toDiscussion did) <$> getEntity did

statementsOfDiscussion :: ID Discussion -> DB [ID Statement]
statementsOfDiscussion did = do
  opts <- dbSelectOpts
  liftDB $
    foreignKeyField S.dSStatement <$$> selectList [S.DSDiscussion ==. S.idToKey did] opts

discussionOfStatement :: ID Statement -> DB (ID Discussion)
discussionOfStatement sid = do
  opts <- dbSelectOpts
  unique =<< liftDB
    (foreignKeyField S.dSDiscussion <$$> selectList [S.DSStatement ==. S.idToKey sid] opts)


-- * Answer

toAnswer :: ID Answer -> Key S.Question -> ST -> Answer
toAnswer aid qkey = Answer aid (S.keyToId qkey)

createAnswer :: ID Question -> Create Answer -> DB Answer
createAnswer qid answer = liftDB $ do
  let sanswer = S.Answer
        (S.idToKey qid)
        (answer ^. createAnswerText)
  key <- insert sanswer
  pure $ S.answerElim (toAnswer (S.keyToId key)) sanswer

getAnswer :: ID Answer -> DB Answer
getAnswer aid = S.answerElim (toAnswer aid) <$> getEntity aid

answersOfQuestion :: ID Question -> DB [Answer]
answersOfQuestion qid = do
  opts <- dbSelectOpts
  liftDB $ do
    mkAnswer <$$> selectList [S.AnswerQuestion ==. S.idToKey qid] opts
  where
    mkAnswer e = S.answerElim (toAnswer (S.keyToId $ entityKey e)) (entityVal e)

-- * Statement

toStatement :: ID Statement -> ST -> Maybe (Key S.Statement) -> Statement
toStatement sid text parent = Statement sid text (S.keyToId <$> parent)

createStatement :: ID Statement  -> Create Statement -> DB Statement
createStatement sid statement = do
  opts <- dbSelectOpts
  ds  <- liftDB $ foreignKeyField S.dSDiscussion <$$> selectList [S.DSStatement ==. S.idToKey sid] opts
  did <- unique ds
  liftDB $ do
    let sstatement = S.Statement
          (statement ^. createStatementText)
          (Just $ S.idToKey sid)
    saveStatement did sstatement

getStatement :: ID Statement -> DB Statement
getStatement sid = S.statementElim (toStatement sid) <$> getEntity sid

-- * Group

toGroup :: [ID Group] -> [ID Group] -> ID Group -> ST -> ST -> Bool -> Group
toGroup parents children gid title desc =
  Group gid title desc parents children

createGroup :: Create Group -> DB Group
createGroup group = liftDB $ do
  let sgroup = S.Group
        (group ^. createGroupTitle)
        (group ^. createGroupDesc)
        (group ^. createGroupUniversal)
  key <- insert sgroup
  forM_ (group ^. createGroupParents) $ \parent -> do
    insert $ S.SubGroup (S.idToKey parent) key
  forM_ (group ^. createGroupChildren) $ \child -> do
    insert $ S.SubGroup key (S.idToKey child)
  pure $ S.groupElim
    (toGroup (group ^. createGroupParents)  (group ^. createGroupChildren) (S.keyToId key))
    sgroup

getChildrenOfGroup :: ID Group -> DB [ID Group]
getChildrenOfGroup gid = do
  opts <- dbSelectOpts
  (S.subGroupElim (\_parent child -> S.keyToId child) . entityVal)
    <$$> liftDB (selectList [S.SubGroupParent ==. S.idToKey gid] opts)

getParentsOfGroup :: ID Group -> DB [ID Group]
getParentsOfGroup gid = do
  opts <- dbSelectOpts
  (S.subGroupElim (\parent _child -> S.keyToId parent) . entityVal)
    <$$> liftDB (selectList [S.SubGroupChild ==. S.idToKey gid] opts)

getGroup :: ID Group -> DB Group
getGroup gid = do
  parents  <- getParentsOfGroup  gid
  children <- getChildrenOfGroup gid
  S.groupElim (toGroup parents children gid) <$> getEntity gid

modifyGroup :: ID Group -> Create Group -> DB Group
modifyGroup gid group = do
  parents  <- getParentsOfGroup gid
  children <- getChildrenOfGroup gid

  liftDB $ do
    -- Update title and desc
    update (S.idToKey gid)
      [ S.GroupTitle       =. group ^. createGroupTitle
      , S.GroupDescription =. group ^. createGroupDesc
      ]

    -- update parents

    let parentsToRemove = parents \\ group ^. createGroupParents
    let parentsToAdd    = group ^. createGroupParents \\ parents

    forM_ parentsToRemove $ \parent -> do
      deleteWhere
        [ S.SubGroupParent ==. S.idToKey parent
        , S.SubGroupChild  ==. S.idToKey gid
        ]

    forM_ parentsToAdd $ \parent -> do
      insert $ S.SubGroup (S.idToKey parent) (S.idToKey gid)

    -- update children

    let childrenToRemove = children \\ group ^. createGroupChildren
    let childrenToAdd    = group ^. createGroupChildren \\ children

    forM_ childrenToRemove $ \child -> do
      deleteWhere
        [ S.SubGroupParent ==. S.idToKey gid
        , S.SubGroupChild  ==. S.idToKey child
        ]

    forM_ childrenToAdd $ \child -> do
      insert $ S.SubGroup (S.idToKey gid) (S.idToKey child)

  getGroup gid

removeGroup :: ID Group -> DB ()
removeGroup gid = liftDB $ do
  deleteWhere [S.SubGroupChild  ==. S.idToKey gid]
  deleteWhere [S.SubGroupParent ==. S.idToKey gid]
  delete (S.idToKey gid)

addSubGroup :: ID Group -> ID Group -> DB ()
addSubGroup parent child = liftDB $ do
  void . insert $ S.SubGroup (S.idToKey parent) (S.idToKey child)

removeSubGroup :: ID Group -> ID Group -> DB ()
removeSubGroup parent child = liftDB $ do
  deleteWhere
    [ S.SubGroupParent ==. S.idToKey parent
    , S.SubGroupChild  ==. S.idToKey child
    ]

universalGroup :: DB (ID Group)
universalGroup = do
  opts <- dbSelectOpts
  xs <- (S.keyToId . entityKey) <$$> liftDB (selectList [ S.GroupUniversal ==. True ] opts)
  unique xs

-- * Roles

assignRole :: ID Group -> ID User -> Role -> DB ()
assignRole gid uid role = liftDB $ do
  void . insert $ S.Roles (S.idToKey gid) (S.idToKey uid) role

getRoles :: ID Group -> ID User -> DB [Role]
getRoles gid uid = do
  opts <- dbSelectOpts
  roles <- liftDB $ selectList [S.RolesGroup ==. S.idToKey gid, S.RolesUser ==. S.idToKey uid] opts
  pure $ (S.rolesElim (\_gid _uid role' -> role') . entityVal) <$> roles

unassignRole :: ID Group -> ID User -> Role -> DB ()
unassignRole gid uid role = liftDB $ do
  deleteWhere
    [ S.RolesGroup ==. S.idToKey gid
    , S.RolesUser  ==. S.idToKey uid
    , S.RolesRole  ==. role
    ]


-- * Process

instance C.StoreProcessData DB CollaborativeEdit where
  processDataGroupID = pure . view createDBCollabEditProcessGroupID

  createProcessData pid process = do
    liftDB $ do
      dkey <- insert $ S.CollabEditProcess
                (process ^. createDBCollabEditProcessVDocID . to S.idToKey)
                (process ^. createDBCollabEditProcessPhase)
      _ <- insert $ S.ProcessOfCollabEdit (S.idToKey pid) dkey
      pure $ CollaborativeEditDB
        (S.keyToId dkey)
        (process ^. createDBCollabEditProcessPhase)
        (process ^. createDBCollabEditProcessVDocID)

  getProcessData pid = do
    opts <- dbSelectOpts
    ceids <- foreignKeyField S.processOfCollabEditCollabEdit
             <$$> liftDB (selectList [S.ProcessOfCollabEditProcess ==. S.idToKey pid] opts)
    ceid <- unique ceids
    cedata <- getEntity ceid
    pure $ CollaborativeEditDB
      ceid
      (S.collabEditProcessPhase cedata)
      (S.keyToId $ S.collabEditProcessVdoc cedata)

  updateProcessData pid process = do
    opts <- dbSelectOpts
    ceids <- foreignKeyField S.processOfCollabEditCollabEdit
             <$$> liftDB (selectList [S.ProcessOfCollabEditProcess ==. S.idToKey pid] opts)
    ceid :: ID CollaborativeEdit <- unique ceids
    liftDB $ update (S.idToKey ceid)
      [ S.CollabEditProcessVdoc  =. process ^. createDBCollabEditProcessVDocID . to S.idToKey
      , S.CollabEditProcessPhase =. process ^. createDBCollabEditProcessPhase
      ]

  removeProcessData pdata = liftDB $ do
    let key = pdata ^. collaborativeEditDBID . to S.idToKey
    deleteWhere [S.ProcessOfCollabEditCollabEdit ==. key]
    delete key
    error "TODO: also remove VDoc and all its contents from the various tables.  see #273."

instance C.StoreProcessData DB Aula where
  processDataGroupID = pure . view createAulaProcessGroupID

  createProcessData pid process = do
    liftDB $ do
      dkey <- insert $ S.AulaProcess (process ^. createAulaProcessClassName)
      _ <- insert $ S.ProcessOfAula (S.idToKey pid) dkey
      pure $ Aula (S.keyToId dkey) (process ^. createAulaProcessClassName)

  getProcessData pid = do
    opts <- dbSelectOpts
    as  <- foreignKeyField S.processOfAulaAula
            <$$> liftDB (selectList [S.ProcessOfAulaProcess ==. S.idToKey pid] opts)
    aid <- unique as
    saula <- getEntity aid
    pure $ Aula aid (S.aulaProcessClass saula)

  updateProcessData pid process = do
    opts <- dbSelectOpts
    as  <- foreignKeyField S.processOfAulaAula
            <$$> liftDB (selectList [S.ProcessOfAulaProcess ==. S.idToKey pid] opts)
    aid :: ID Aula <- unique as
    liftDB $ update (S.idToKey aid)
      [ S.AulaProcessClass =. process ^. createAulaProcessClassName
      ]

  removeProcessData pdata = liftDB $ do
    deleteWhere [S.ProcessOfAulaAula ==. pdata ^. aulaID . to S.idToKey]
    delete (pdata ^. aulaID . to S.idToKey)

createProcess :: (C.StoreProcessData DB a) => CreateDB (Process a) -> DB (Process (ResultDB (Process a)))
createProcess process = do
  gid   <- C.processDataGroupID process
  pkey  <- liftDB . insert $ S.Process (S.idToKey gid)
  pdata <- C.createProcessData (S.keyToId pkey) process
  group <- getGroup gid
  pure $ Process (S.keyToId pkey) group pdata

getProcess :: (C.StoreProcessData DB a, Typeable a) => ID (Process a) -> DB (Process (ResultDB (Process a)))
getProcess pid = do
  process <- getEntity pid
  pdata   <- C.getProcessData pid
  group   <- getGroup (S.keyToId $ S.processGroup process)
  pure $ Process (unsafeCoerceID pid) group pdata

updateProcess :: (C.StoreProcessData DB a) => ID (Process a) -> CreateDB (Process a) -> DB ()
updateProcess pid process = do
  gid <- C.processDataGroupID process
  liftDB $ update (S.idToKey pid)
    [ S.ProcessGroup =. S.idToKey gid
    ]
  C.updateProcessData pid process

removeProcess :: (C.StoreProcessData DB a, Typeable a) => ID (Process a) -> DB ()
removeProcess pid = do
  process <- getProcess pid
  C.removeProcessData (process ^. processData)
  liftDB $ delete (process ^. processID . to S.idToKey)
