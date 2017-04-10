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
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Entity where

import Control.Lens ((^.), to, view)
import Control.Monad ((>=>), forM, forM_, void)
import Control.Monad.Reader (ask)
import Data.Functor.Infix ((<$$>))
import Data.List ((\\))
import Data.String.Conversions (ST)
import Data.Typeable
import Data.Coerce (coerce)
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
import           Refine.Common.Types.Prelude (ID(..))
import           Refine.Prelude (nothingToError, Timestamp, getCurrentTimestamp)

-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep MetaInfo   = S.MetaInfo
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
  modifyMetaID vid

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
  DBContext _mu fltrs <- ask
  pure . mconcat $ filterToSelectOpt <$> fltrs
  where
    filterToSelectOpt = \case
      Limit n -> [LimitTo n]

-- * MetaInfo

getUserAndTime :: DB (UserInfo, Timestamp)
getUserAndTime = do
  user <- view $ dbLoggedInUser . to (maybe Anonymous UserID)  -- FUTUREWORK: use IP address if available
  time <- getCurrentTimestamp
  pure (user, time)

createMetaID
  :: (ra ~ S.EntityRep a, PersistEntityBackend ra ~ BaseBackend SqlBackend, ToBackendKey SqlBackend ra)
  => ra -> DB (MetaID a)
createMetaID a = do
  ida <- S.keyToId <$> liftDB (insert a)
  (user, time) <- getUserAndTime
  let meta = S.MetaInfo user time user time
  void . liftDB $ insertKey (S.idToKey (coerce ida :: ID MetaInfo)) meta
  pure . MetaID ida $ S.metaInfoElim MetaInfo meta

addConnection
    :: (PersistEntityBackend record ~ BaseBackend SqlBackend, ToBackendKey SqlBackend record
       , ToBackendKey SqlBackend (S.EntityRep a), ToBackendKey SqlBackend (S.EntityRep b))
    => (Key (S.EntityRep a) -> Key (S.EntityRep b) -> record) -> ID a -> ID b -> DB ()
addConnection t rid mid = void . liftDB . insert $ t (S.idToKey rid) (S.idToKey mid)

modifyMetaID :: ID a -> DB ()
modifyMetaID ida = do
  (user, time) <- getUserAndTime
  meta <- getEntity idm
  let meta' = meta {S.metaInfoModBy = user, S.metaInfoModAt = time}
  liftDB $ replace (S.idToKey idm) meta'
  where
    idm = coerce ida :: ID MetaInfo

getMeta :: ID a -> DB (MetaID a)
getMeta ida = do
  meta <- getEntity (coerce ida :: ID MetaInfo)
  pure . MetaID ida $ S.metaInfoElim MetaInfo meta

-- * VDoc

toVDoc :: MetaID VDoc -> Title -> Abstract -> Key S.Repo -> VDoc
toVDoc vid title abstract repoid = VDoc vid title abstract (S.keyToId repoid)

listVDocs :: DB [ID VDoc]
listVDocs = do
  opts <- dbSelectOpts
  liftDB $ S.keyToId <$$> selectKeysList [] opts

createVDoc :: Create VDoc -> VDocRepo -> DB VDoc
createVDoc pv vr = do
  let svdoc = S.VDoc
        (pv ^. createVDocTitle)
        (pv ^. createVDocAbstract)
        (vr ^. vdocRepoID . to S.idToKey)
  mid <- createMetaID svdoc
  addConnection S.VR (mid ^. miID) (vr ^. vdocRepoID)
  pure $ S.vDocElim (toVDoc mid) svdoc

getVDoc :: ID VDoc -> DB VDoc
getVDoc vid = do
    mid <- getMeta vid
    S.vDocElim (toVDoc mid) <$> getEntity vid

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

-- * DocRepo and VDoc

vDocRepoVDoc :: ID VDocRepo -> DB (ID VDoc)
vDocRepoVDoc rid = do
  repos <- foreignKeyField S.vRVdoc
            <$$> liftDB (selectList [S.VRRepository ==. S.idToKey rid] [])
  unique repos

-- * Edit

createEdit :: ID VDocRepo -> DocRepo.EditHandle -> CreateEdit -> DB Edit
createEdit rid edith ce = do
  mid <- createMetaID $ S.Edit
            (ce ^. createEditDesc)
            (ce ^. createEditRange)
            edith
            (ce ^. createEditKind)
            (ce ^. createEditMotiv)
  addConnection S.RP rid (mid ^. miID)
  pure $ Edit
    mid
    (ce ^. createEditDesc)
    (ce ^. createEditRange)
    (ce ^. createEditKind)
    (ce ^. createEditMotiv)

getEdit :: ID Edit -> DB Edit
getEdit pid = do
    mid <- getMeta pid
    let toEdit :: ST -> ChunkRange -> DocRepo.EditHandle -> EditKind -> ST -> Edit
        toEdit desc cr _handle = Edit mid desc cr
    S.editElim toEdit <$> getEntity pid

getEditFromHandle :: DocRepo.EditHandle -> DB Edit
getEditFromHandle hndl = do
  opts <- dbSelectOpts
  ps <- liftDB $ selectList [S.EditEditHandle ==. hndl] opts
  p <- unique ps
  mid <- getMeta . S.keyToId $ entityKey p
  let toEdit desc cr _hdnl = Edit mid desc cr
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
toNote :: MetaID Note -> ST -> Bool -> ChunkRange -> LoginId -> Note
toNote nid desc public range _lid = Note nid desc public range

createNote :: ID Edit -> Create Note -> DB Note
createNote pid note = do
  userId <- dbUser
  let snote = S.Note
          (note ^. createNoteText)
          (note ^. createNotePublic)
          (note ^. createNoteRange)
          (fromUserID userId)
  mid <- createMetaID snote
  addConnection S.PN pid (mid ^. miID)
  pure $ S.noteElim (toNote mid) snote

getNote :: ID Note -> DB Note
getNote nid = do
  mid <- getMeta nid
  S.noteElim (toNote mid) <$> getEntity nid


-- * Question

-- TODO: User lid
toQuestion :: MetaID Question -> ST -> Bool -> Bool -> ChunkRange -> LoginId -> Question
toQuestion qid text answ pblc range _lid = Question qid text answ pblc range

createQuestion :: ID Edit -> Create Question -> DB Question
createQuestion pid question = do
  userId <- dbUser
  let squestion = S.Question
          (question ^. createQuestionText)
          False -- Not answered
          (question ^. createQuestionPublic)
          (question ^. createQuestionRange)
          (fromUserID userId)
  mid <- createMetaID squestion
  addConnection S.PQ pid (mid ^. miID)
  pure $ S.questionElim (toQuestion mid) squestion

getQuestion :: ID Question -> DB Question
getQuestion qid = do
  mid <- getMeta qid
  S.questionElim (toQuestion mid) <$> getEntity qid


-- * Discussion

-- TODO: Login ID
toDiscussion :: MetaID Discussion -> Bool -> ChunkRange -> LoginId -> Discussion
toDiscussion did pblc range _lid = Discussion did pblc range

saveStatement :: ID Discussion -> S.Statement -> DB Statement
saveStatement did sstatement = do
  mid <- createMetaID sstatement
  addConnection S.DS did (mid ^. miID)
  pure $ S.statementElim (toStatement mid) sstatement

createDiscussion :: ID Edit -> Create Discussion -> DB Discussion
createDiscussion pid disc = do
  userId <- dbUser
  let sdiscussion = S.Discussion
          (disc ^. createDiscussionPublic)
          (disc ^. createDiscussionRange)
          (fromUserID userId)
  mid <- createMetaID sdiscussion
  addConnection S.PD pid (mid ^. miID)
  let sstatement = S.Statement
          (disc ^. createDiscussionStatementText)
          Nothing -- Top level node
  void $ saveStatement (mid ^. miID) sstatement
  pure $ S.discussionElim (toDiscussion mid) sdiscussion

getDiscussion :: ID Discussion -> DB Discussion
getDiscussion did = do
  mid <- getMeta did
  S.discussionElim (toDiscussion mid) <$> getEntity did

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

toAnswer :: MetaID Answer -> Key S.Question -> ST -> Answer
toAnswer aid qkey = Answer aid (S.keyToId qkey)

createAnswer :: ID Question -> Create Answer -> DB Answer
createAnswer qid answer = do
  let sanswer = S.Answer
        (S.idToKey qid)
        (answer ^. createAnswerText)
  mid <- createMetaID sanswer
  pure $ S.answerElim (toAnswer mid) sanswer

getAnswer :: ID Answer -> DB Answer
getAnswer aid = do
  mid <- getMeta aid
  S.answerElim (toAnswer mid) <$> getEntity aid

answersOfQuestion :: ID Question -> DB [Answer]
answersOfQuestion qid = do
  opts <- dbSelectOpts
  sls <- liftDB $ selectList [S.AnswerQuestion ==. S.idToKey qid] opts
  forM sls $ \e -> do
    mid <- getMeta . S.keyToId $ entityKey e
    pure $ S.answerElim (toAnswer mid) (entityVal e)

-- * Statement

toStatement :: MetaID Statement -> ST -> Maybe (Key S.Statement) -> Statement
toStatement sid text parent = Statement sid text (S.keyToId <$> parent)

createStatement :: ID Statement  -> Create Statement -> DB Statement
createStatement sid statement = do
  opts <- dbSelectOpts
  ds  <- liftDB $ foreignKeyField S.dSDiscussion <$$> selectList [S.DSStatement ==. S.idToKey sid] opts
  did <- unique ds
  let sstatement = S.Statement
          (statement ^. createStatementText)
          (Just $ S.idToKey sid)
  saveStatement did sstatement

getStatement :: ID Statement -> DB Statement
getStatement sid = do
  mid <- getMeta sid
  S.statementElim (toStatement mid) <$> getEntity sid

-- * Group

toGroup :: [ID Group] -> [ID Group] -> MetaID Group -> ST -> ST -> Bool -> Group
toGroup parents children gid title desc =
  Group gid title desc parents children

createGroup :: Create Group -> DB Group
createGroup group = do
  let sgroup = S.Group
        (group ^. createGroupTitle)
        (group ^. createGroupDesc)
        (group ^. createGroupUniversal)
  mid <- createMetaID sgroup
  forM_ (group ^. createGroupParents) $ \parent -> addConnection S.SubGroup parent (mid ^. miID)
  forM_ (group ^. createGroupChildren) $ \child -> addConnection S.SubGroup (mid ^. miID) child
  pure $ S.groupElim
    (toGroup (group ^. createGroupParents) (group ^. createGroupChildren) mid)
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
  mid <- getMeta gid
  S.groupElim (toGroup parents children mid) <$> getEntity gid

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

  modifyMetaID gid
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
      pure $ CollaborativeEdit
        (S.keyToId dkey)
        (process ^. createDBCollabEditProcessPhase)
        (process ^. createDBCollabEditProcessVDocID)

  getProcessData pid = do
    opts <- dbSelectOpts
    ceids <- foreignKeyField S.processOfCollabEditCollabEdit
             <$$> liftDB (selectList [S.ProcessOfCollabEditProcess ==. S.idToKey pid] opts)
    ceid <- unique ceids
    cedata <- getEntity ceid
    pure $ CollaborativeEdit
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
    let key = pdata ^. collaborativeEditID . to S.idToKey
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

createProcess :: (C.StoreProcessData DB a) => CreateDB (Process a) -> DB (Process a)
createProcess process = do
  gid   <- C.processDataGroupID process
  mid   <- createMetaID $ S.Process (S.idToKey gid)
  pdata <- C.createProcessData (mid ^. miID) process
  group <- getGroup gid
  pure $ Process mid group pdata

getProcess :: (C.StoreProcessData DB a, Typeable a) => ID (Process a) -> DB (Process a)
getProcess pid = do
  process <- getEntity pid
  pdata   <- C.getProcessData pid
  group   <- getGroup (S.keyToId $ S.processGroup process)
  mid     <- getMeta pid
  pure $ Process mid group pdata

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
  C.removeProcessData (process ^. processPayload)
  liftDB $ delete (process ^. processID . to S.idToKey)

vDocProcess :: ID VDoc -> DB (ID (Process CollaborativeEdit))
vDocProcess vid = do
  -- CollabEditProcess
  cedits <- entityKey <$$> liftDB (selectList [S.CollabEditProcessVdoc ==. S.idToKey vid] [])
  cedit <- unique cedits
  -- ProcessOfCollabEdit
  processes <- foreignKeyField S.processOfCollabEditProcess
                <$$> liftDB (selectList [S.ProcessOfCollabEditCollabEdit ==. cedit] [])
  unique processes


-- * GroupOf

instance (C.StoreProcessData DB a, Typeable a) => C.GroupOf DB (Process a) where
  groupOf = fmap (view processGroup) . getProcess

instance C.GroupOf DB VDoc where
  groupOf = vDocProcess >=> C.groupOf

instance C.GroupOf DB VDocRepo where
  groupOf = vDocRepoVDoc >=> C.groupOf

instance C.GroupOf DB Edit where
  groupOf = editVDocRepo >=> C.groupOf

-- * ProcessOf

type instance C.ProcessPayload Edit = C.ProcessPayload VDocRepo

instance C.ProcessOf DB Edit where
  processOf = editVDocRepo >=> C.processOf

type instance C.ProcessPayload VDocRepo = C.ProcessPayload VDoc

instance C.ProcessOf DB VDocRepo where
  processOf = vDocRepoVDoc >=> C.processOf

type instance C.ProcessPayload VDoc = CollaborativeEdit

instance C.ProcessOf DB VDoc where
  processOf = vDocProcess >=> getProcess
