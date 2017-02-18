{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Refine.Backend.Database.Entity where

import Control.Lens ((^.), to)
import Control.Monad (void)
import Control.Monad.Reader (ask)
import Data.Functor.Infix ((<$$>))
import Data.String.Conversions (ST)
import Data.Typeable
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Lentil.Core (entityLens)
import Lentil.Types as L

import           Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import           Refine.Backend.Database.Types
import qualified Refine.Backend.DocRepo.Core as DocRepo
import           Refine.Backend.User.Core as Users (Login, LoginId, fromUserID)
import           Refine.Common.Types
import           Refine.Prelude (maybeError)

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

toChunkRange :: DBChunkRange -> ID a -> ChunkRange a
toChunkRange r i = ChunkRange i (r ^. dbChunkRangeBegin) (r ^. dbChunkRangeEnd)

mkDBChunkRange :: CreateChunkRange -> DBChunkRange
mkDBChunkRange cr = DBChunkRange (cr ^. createChunkRangeBegin) (cr ^. createChunkRangeEnd)

vdocDBLens :: EntityLens' DB (ID VDoc) VDoc
vdocDBLens = entityLens vdocEntity

vdocEntity :: L.Entity DB ID VDoc VDoc
vdocEntity = L.Entity getVDoc updateVDoc

-- | Returns the ID of the user who runs the current DB computation.
dbUser :: DB (ID User)
dbUser = do
  DBContext mu <- ask
  maybeError DBUserNotLoggedIn mu

-- * VDoc

toVDoc :: ID VDoc -> Title -> Abstract -> Key S.Repo -> VDoc
toVDoc vid title abstract repoid = VDoc vid title abstract (S.keyToId repoid)

listVDocs :: DB [ID VDoc]
listVDocs = liftDB $ S.keyToId <$$> selectKeysList [] []

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
  vs <- liftDB $ foreignKeyField S.vRRepository <$$> selectList [S.VRVdoc ==. S.idToKey vid] []
  unique vs

vdocRepoOfEdit :: ID Edit -> DB (ID VDocRepo)
vdocRepoOfEdit eid = unique =<< liftDB
  (foreignKeyField S.rPRepository <$$> selectList [S.RPEdit ==. S.idToKey eid] [])

-- * Repo

createRepo :: DocRepo.RepoHandle -> DocRepo.EditHandle -> DB VDocRepo
createRepo repoh edith = liftDB $ do
    let desc = "" -- TODO
    pkey <- insert $ S.Edit desc edith
    key  <- insert $ S.Repo "title" {- TODO -} repoh pkey
    void  . insert $ S.RP key pkey
    pure $ VDocRepo (S.keyToId key) (S.keyToId pkey)

getRepo :: ID VDocRepo -> DB VDocRepo
getRepo vid = S.repoElim toVDocRepo <$> getEntity vid
  where
    toVDocRepo :: ST -> DocRepo.RepoHandle -> Key S.Edit -> VDocRepo
    toVDocRepo _desc _repoHandle pid = VDocRepo vid (S.keyToId pid)

getRepoFromHandle :: DocRepo.RepoHandle -> DB VDocRepo
getRepoFromHandle hndl = do
  rs <- liftDB $ selectList [S.RepoRepoHandle ==. hndl] []
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

createEdit :: ID VDocRepo -> DocRepo.EditHandle -> DB Edit
createEdit rid edith = liftDB $ do
  let desc = "" -- TODO
  key <- insert $ S.Edit desc edith
  void . insert $ S.RP (S.idToKey rid) key
  let pid = S.keyToId key
      cr = ChunkRange pid Nothing Nothing  -- TODO
  pure $ Edit pid desc cr

getEdit :: ID Edit -> DB Edit
getEdit pid = S.editElim toEdit <$> getEntity pid
  where
    cr :: ChunkRange Edit
    cr = ChunkRange pid Nothing Nothing  -- TODO

    toEdit :: ST -> DocRepo.EditHandle -> Edit
    toEdit desc _handle = Edit pid desc cr

getEditFromHandle :: DocRepo.EditHandle -> DB Edit
getEditFromHandle hndl = do
  ps <- liftDB $ selectList [S.EditEditHandle ==. hndl] []
  p <- unique ps
  let pid = S.keyToId $ entityKey p
      cr = ChunkRange pid Nothing Nothing  -- TODO
      toEdit desc _hdnl = Edit (S.keyToId $ entityKey p) desc cr
  pure $ S.editElim toEdit (entityVal p)

getEditHandle :: ID Edit -> DB DocRepo.EditHandle
getEditHandle pid = S.editElim toEditHandle <$> getEntity pid
  where
    toEditHandle :: ST -> DocRepo.EditHandle -> DocRepo.EditHandle
    toEditHandle _desc handle = handle

editNotes :: ID Edit -> DB [ID Note]
editNotes pid = liftDB $
  foreignKeyField S.pNNote <$$> selectList [S.PNEdit ==. S.idToKey pid] []

editQuestions :: ID Edit -> DB [ID Question]
editQuestions pid = liftDB $
  foreignKeyField S.pQQuestion <$$> selectList [S.PQEdit ==. S.idToKey pid] []

editDiscussions :: ID Edit -> DB [ID Discussion]
editDiscussions pid = liftDB $
  foreignKeyField S.pDDiscussion <$$> selectList [S.PDEdit ==. S.idToKey pid] []

setEditChild :: ID Edit -> ID Edit -> DB ()
setEditChild parent child = liftDB $ do
  void . insert $ S.PC (S.idToKey parent) (S.idToKey child)

getEditChildren :: ID Edit -> DB [ID Edit]
getEditChildren parent = liftDB $ do
  foreignKeyField S.pCChild <$$> selectList [S.PCParent ==. S.idToKey parent] []

-- * Repo and edit

editVDocRepo :: ID Edit -> DB (ID VDocRepo)
editVDocRepo pid = do
  rs <- liftDB $ foreignKeyField S.rPRepository <$$> selectList [S.RPEdit ==. S.idToKey pid] []
  unique rs

registerEdit :: ID VDocRepo -> ID Edit -> DB ()
registerEdit rid pid = void . liftDB . insert $ S.RP (S.idToKey rid) (S.idToKey pid)

-- * Note

-- TODO: Use the _lid
toNote :: ID Note -> ST -> Bool -> DBChunkRange -> LoginId -> Note
toNote nid desc public range _lid = Note nid desc public (toChunkRange range nid)

createNote :: ID Edit -> Create Note -> DB Note
createNote pid note = do
  userId <- dbUser
  liftDB $ do
    let snote = S.Note
          (note ^. createNoteText)
          (note ^. createNotePublic)
          (note ^. createNoteRange . to mkDBChunkRange)
          (fromUserID userId)
    key <- insert snote
    void . insert $ S.PN (S.idToKey pid) key
    pure $ S.noteElim (toNote (S.keyToId key)) snote

getNote :: ID Note -> DB Note
getNote nid = S.noteElim (toNote nid) <$> getEntity nid

addNoteUserAccess :: ID Note -> ID User -> DB ()
addNoteUserAccess nid uid = void . liftDB . insert $ S.NoteAcc (S.idToKey nid) (S.idToKey uid)

removeNoteAccess :: ID Note -> ID User -> DB ()
removeNoteAccess nid uid = void . liftDB . deleteBy $ S.UniNA (S.idToKey nid) (S.idToKey uid)

usersOfNote :: ID Note -> DB [ID User]
usersOfNote nid = foreignKeyField S.noteAccUser <$$> liftDB (selectList [S.NoteAccNote ==. S.idToKey nid] [])

-- * Question

-- TODO: User lid
toQuestion :: ID Question -> ST -> Bool -> Bool -> DBChunkRange -> LoginId -> Question
toQuestion qid text answ pblc range _lid = Question qid text answ pblc (toChunkRange range qid)

createQuestion :: ID Edit -> Create Question -> DB Question
createQuestion pid question = do
  userId <- dbUser
  liftDB $ do
    let squestion = S.Question
          (question ^. createQuestionText)
          False -- Not answered
          (question ^. createQuestionPublic)
          (question ^. createQuestionRange . to mkDBChunkRange)
          (fromUserID userId)
    key <- insert squestion
    void . insert $ S.PQ (S.idToKey pid) key
    pure $ S.questionElim (toQuestion (S.keyToId key)) squestion

getQuestion :: ID Question -> DB Question
getQuestion qid = S.questionElim (toQuestion qid) <$> getEntity qid

addQuestionUserAccess :: ID Question -> ID User -> DB ()
addQuestionUserAccess qid uid = void . liftDB . insert $ S.QstnAcc (S.idToKey qid) (S.idToKey uid)

removeQuestionUserAccess :: ID Question -> ID User -> DB ()
removeQuestionUserAccess qid uid = void . liftDB . deleteBy $ S.UniQA (S.idToKey qid) (S.idToKey uid)

usersOfQuestion :: ID Question -> DB [ID User]
usersOfQuestion qid = foreignKeyField S.qstnAccUser <$$> liftDB (selectList [S.QstnAccQuestion ==. S.idToKey qid] [])

-- * Discussion

-- TODO: Login ID
toDiscussion :: ID Discussion -> Bool -> DBChunkRange -> LoginId -> Discussion
toDiscussion did pblc range _lid = Discussion did pblc (toChunkRange range did)

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
          (disc ^. createDiscussionRange . to mkDBChunkRange)
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
statementsOfDiscussion did = liftDB $
  foreignKeyField S.dSStatement <$$> selectList [S.DSDiscussion ==. S.idToKey did] []

discussionOfStatement :: ID Statement -> DB (ID Discussion)
discussionOfStatement sid = unique =<< liftDB
  (foreignKeyField S.dSDiscussion <$$> selectList [S.DSStatement ==. S.idToKey sid] [])

addDiscussionUserAccess :: ID Discussion -> ID User -> DB ()
addDiscussionUserAccess did uid = void . liftDB . insert $ S.DscnAcc (S.idToKey did) (S.idToKey uid)

removeDiscussionUserAccess :: ID Discussion -> ID User -> DB ()
removeDiscussionUserAccess did uid = void . liftDB . deleteBy $ S.UniDA (S.idToKey did) (S.idToKey uid)

usersOfDiscussion :: ID Discussion -> DB [ID User]
usersOfDiscussion did = foreignKeyField S.dscnAccUser <$$> liftDB (selectList [S.DscnAccDiscussion ==. S.idToKey did] [])

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
getAnswer _aid = undefined

answersOfQuestion :: ID Question -> DB [Answer]
answersOfQuestion qid = liftDB $ do
  mkAnswer <$$> selectList [S.AnswerQuestion ==. S.idToKey qid] []
  where
    mkAnswer e = S.answerElim (toAnswer (S.keyToId $ entityKey e)) (entityVal e)

-- * Statement

toStatement :: ID Statement -> ST -> Maybe (Key S.Statement) -> Statement
toStatement sid text parent = Statement sid text (S.keyToId <$> parent)

createStatement :: ID Statement  -> Create Statement -> DB Statement
createStatement sid statement = do
  ds  <- liftDB $ foreignKeyField S.dSDiscussion <$$> selectList [S.DSStatement ==. S.idToKey sid] []
  did <- unique ds
  liftDB $ do
    let sstatement = S.Statement
          (statement ^. createStatementText)
          (Just $ S.idToKey sid)
    saveStatement did sstatement

getStatement :: ID Statement -> DB Statement
getStatement sid = S.statementElim (toStatement sid) <$> getEntity sid
