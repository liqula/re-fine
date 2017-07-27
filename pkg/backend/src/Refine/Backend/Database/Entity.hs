{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Backend.Prelude as P hiding (get)

import           Database.Persist (get)
import           Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)
import qualified Data.Set as Set
import           Lentil.Core (entityLens)
import           Lentil.Types as L
import qualified Web.Users.Persistent as Users
import qualified Web.Users.Persistent.Definitions as Users

import           Refine.Backend.Database.Core
import qualified Refine.Backend.Database.Schema as S
import           Refine.Backend.Database.Types
import           Refine.Backend.Database.Tree
import           Refine.Common.Types
import           Refine.Common.Types.Prelude (ID(..))
import qualified Refine.Common.OT as OT
import           Refine.Prelude (nothingToError, Timestamp, getCurrentTimestamp)

-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep MetaInfo   = S.MetaInfo
type instance S.EntityRep VDoc       = S.VDoc
type instance S.EntityRep Edit       = S.Edit
type instance S.EntityRep Note       = S.Note
type instance S.EntityRep Question   = S.Question
type instance S.EntityRep Discussion = S.Discussion
type instance S.EntityRep Answer     = S.Answer
type instance S.EntityRep Statement  = S.Statement
type instance S.EntityRep User       = Users.Login
type instance S.EntityRep Group      = S.Group

{-
[not sure what this comment is about any more..  fisx]

Reading the domain structured datatypes is not a problem:
there is no big difference between accessing the parts and
combining them via an applicative functor, producing an lazy value.

The problem arises when we add some information to the computed
value in another pure computation. The changes in the value
will be lost, as there is no trigger mechanism.

If we want to address this problem, we need to use lenses over
some ids to navigate deeper in the data structures.

If the lens is only a getter, the result functor won't run the update;
whenever we operate on some data we need to load the current
version from the db.

Instead of saving the whole value in the database, a generic
diff algorithm could be used.

https://hackage.haskell.org/package/gdiff
-}


-- * Helpers

idNotFound :: (Typeable d) => ID d -> DB a
idNotFound i = notFound $ unwords [show $ typeOf i, show i, "is not found."]

unique :: String -> [a] -> DB a
unique _      [x] = pure x
unique errmsg []  = notFound errmsg
unique errmsg _   = notUnique errmsg

getEntityRep :: (ToBackendKey SqlBackend (S.EntityRep e), Typeable e)
             => ID e -> DB (S.EntityRep e)
getEntityRep eid = do
  e <- liftDB . get $ S.idToKey eid
  maybe (idNotFound eid) pure e

-- | Access the key like field in an entity and convert the key value
-- to the Domain ID.
--
-- Examples:
-- >>> foreignKeyField S.PCComment (Entity pcid (S.PC pid cid)) == cid
-- >>> foreignKeyField S.PCEdit    (Entity pcid (S.PC pid cid)) == pid
foreignKeyField
  :: ToBackendKey SqlBackend (S.EntityRep a)
  => (b -> Key (S.EntityRep a)) -> P.Entity b -> ID a
foreignKeyField column = S.keyToId . column . entityVal

-- NOTES: How to handle associations? What to update, what to keep?
vDocToRecord :: VDoc -> DB S.VDoc
vDocToRecord (VDoc _i t a r g) = pure (S.VDoc t a (Just $ S.idToKey r) (S.idToKey g))

updateVDoc :: ID VDoc -> VDoc -> DB ()
updateVDoc vid vdoc = do
  record <- vDocToRecord vdoc
  liftDB $ replace (S.idToKey vid) record
  modifyMetaID vid

moveVDocHead :: ID VDoc -> ID Edit -> DB ()
moveVDocHead vid eid = do
  liftDB $ update (S.idToKey vid) [S.VDocHeadId =. Just (S.idToKey eid)]
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

-- | Converts an internal UserID representation to the common UserID.
--
-- FIXME: get rid of this (how do we do it with the other common types?)
toUserID :: Users.LoginId -> ID User
toUserID = ID . fromSqlKey

-- | Inverse of 'toUserID'.
fromUserID :: ID User -> Users.LoginId
fromUserID (ID i) = toSqlKey i


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
  :: (ra ~ S.EntityRep a, PersistEntityBackend ra ~ BaseBackend SqlBackend, ToBackendKey SqlBackend ra
     , HasMetaInfo a)
  => ra -> DB (MetaID a)
createMetaID a = do
  ida <- S.keyToId <$> liftDB (insert a)
  createMetaID_ ida

createMetaID_
  :: (HasMetaInfo a) => ID a -> DB (MetaID a)
createMetaID_ ida = do
  (user, time) <- getUserAndTime
  let meta = S.MetaInfo (metaInfoType ida) user time user time
  void . liftDB $ insert meta
  pure . MetaID ida $ S.metaInfoElim (const MetaInfo) meta

addConnection
    :: (PersistEntityBackend record ~ BaseBackend SqlBackend, ToBackendKey SqlBackend record
       , ToBackendKey SqlBackend (S.EntityRep a), ToBackendKey SqlBackend (S.EntityRep b))
    => (Key (S.EntityRep a) -> Key (S.EntityRep b) -> record) -> ID a -> ID b -> DB ()
addConnection t rid mid = void . liftDB . insert $ t (S.idToKey rid) (S.idToKey mid)

getMetaInfo :: HasMetaInfo a => ID a -> DB (P.Entity (S.EntityRep MetaInfo))
getMetaInfo ida = fromMaybe (error $ "no meta info for " <> show (metaInfoType ida)) <$> do
  liftDB . getBy $ S.UniMetaInfo (metaInfoType ida)

modifyMetaID :: HasMetaInfo a => ID a -> DB ()
modifyMetaID ida = do
  meta <- getMetaInfo ida
  (user, time) <- getUserAndTime
  liftDB $ update (entityKey meta) [S.MetaInfoModBy =. user, S.MetaInfoModAt =. time]

getMeta :: HasMetaInfo a => ID a -> DB (MetaID a)
getMeta ida = do
  meta <- getMetaInfo ida
  pure . MetaID ida . S.metaInfoElim (const MetaInfo) $ entityVal meta

getMetaEntity
  :: (ToBackendKey SqlBackend (S.EntityRep e), Typeable e, HasMetaInfo e)
  => (MetaID e -> S.EntityRep e -> b) -> ID e -> DB b
getMetaEntity f i = do
    mid <- getMeta i
    f mid <$> getEntityRep i


-- * VDoc

toVDoc :: MetaID VDoc -> Title -> Abstract -> Maybe (Key S.Edit) -> Key S.Group -> VDoc
toVDoc vid title abstract (Just repoid) g = VDoc vid title abstract (S.keyToId repoid) (S.keyToId g)
toVDoc _ _ _ Nothing _ = error "impossible"

listVDocs :: DB [ID VDoc]
listVDocs = do
  opts <- dbSelectOpts
  liftDB $ S.keyToId <$$> selectKeysList [] opts

createVDoc :: Create VDoc -> VDocVersion -> DB VDoc
createVDoc pv vdoc = do
  let svdoc = S.VDoc
        (pv ^. createVDocTitle)
        (pv ^. createVDocAbstract)
        Nothing -- hack: use a dummy key which will be replaced by a proper one before createVDoc returns
        (S.idToKey $ pv ^. createVDocGroup)
  mid <- createMetaID svdoc
  e <- createEdit (mid ^. miID) mempty CreateEdit
    { _createEditDesc        = "initial document version"
    , _createEditVDocVersion = vdoc
    , _createEditKind        = Initial
    }
  let e' = S.idToKey (e ^. editMetaID . miID) :: Key S.Edit
  liftDB $ update (S.idToKey $ mid ^. miID) [S.VDocHeadId =. Just e']
  pure $ S.vDocElim (toVDoc mid) svdoc {S.vDocHeadId = Just e'}

getVDoc :: ID VDoc -> DB VDoc
getVDoc = getMetaEntity (S.vDocElim . toVDoc)


-- * Repo

getEditIDs :: ID VDoc -> DB [ID Edit]
getEditIDs vid = liftDB $ S.keyToId . entityKey <$$> selectList [S.EditRepository ==. S.idToKey vid] []


-- * Edit

createEdit :: ID VDoc -> EditSource (ID Edit) -> CreateEdit -> DB Edit
createEdit rid me ce = do
  mid <- createMetaID $ S.Edit
            (ce ^. createEditDesc)
            (ce ^. createEditVDocVersion)
            (S.idToKey rid)
            (ce ^. createEditKind)
            (DBVotes mempty)
  liftDB . forM_ (_unEditSource me) $ \(edit, parent) ->
      insert $ S.ParentChild (S.idToKey parent) (RawContentEdit edit) (S.idToKey $ mid ^. miID)
  getEdit $ mid ^. miID

updateEdit :: ID Edit -> Create Edit -> DB ()
updateEdit eid ce = do
  liftDB $ update (S.idToKey eid)
    [ S.EditEditVDoc =. (ce ^. createEditVDocVersion)
    , S.EditDesc     =. (ce ^. createEditDesc)
    , S.EditKind     =. (ce ^. createEditKind)
    ]
  modifyMetaID eid

getEdit :: ID Edit -> DB Edit
getEdit eid = do
  src         <- getEditSource eid
  notes       <- Set.fromList <$> editNotes eid
  discussions <- Set.fromList <$> editDiscussions eid
  children    <- Set.fromList <$> getEditChildren eid
  getMetaEntity (\mid -> S.editElim $
                  \desc d vdoc kind (DBVotes vs) ->
                    Edit mid desc kind src (S.keyToId vdoc) d vs children notes discussions) eid

getEditSource :: ID Edit -> DB (EditSource (ID Edit))
getEditSource eid = do
  parents <- entityVal <$$> liftDB (selectList [S.ParentChildChild ==. S.idToKey eid] [])
  pure $ EditSource [(edit, S.keyToId parent) | S.ParentChild parent (RawContentEdit edit) _ <- parents]

-- | for each parent of an edit, update the diff between the edit and its parent
updateEditSource :: ID Edit -> (ID Edit{-parent-} -> OT.Edit RawContent -> OT.Edit RawContent) -> DB ()
updateEditSource eid f = do
  parents <- entityVal <$$> liftDB (selectList [S.ParentChildChild ==. S.idToKey eid] [])
  liftDB . forM_ parents $ \(S.ParentChild parent (RawContentEdit edit) _) -> do
    upsertBy (S.UniPC parent $ S.idToKey eid)
             (error "updateEditSource")
             [ S.ParentChildEdit =. RawContentEdit (f (S.keyToId parent) edit) ]

getVersion :: ID Edit -> DB VDocVersion
getVersion pid = S.editElim (\_ vdoc _ _ _ -> vdoc) <$> getEntityRep pid

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

getEditChildren :: ID Edit -> DB [ID Edit]
getEditChildren parent = do
  opts <- dbSelectOpts
  liftDB $ do
    foreignKeyField S.parentChildChild <$$> selectList [S.ParentChildParent ==. S.idToKey parent] opts

updateVotes :: ID Edit -> (Votes -> Votes) -> DB ()
updateVotes eid f = do
  vs <- getVotes eid
  liftDB $ update (S.idToKey eid) [ S.EditVotes =. DBVotes (f vs) ]

getVotes :: ID Edit -> DB Votes
getVotes eid = do
  getMetaEntity (\_ -> S.editElim $ \_ _ _ _ (DBVotes vs) -> vs) eid

getVoteCount :: ID Edit -> DB VoteCount
getVoteCount = fmap votesToCount . getVotes


-- * Repo and edit

vdocOfEdit :: ID Edit -> DB (ID VDoc)
vdocOfEdit pid = S.editElim (\_ _ vid _ _ -> S.keyToId vid) <$> getEntityRep pid


-- * Note

-- FIXME: Use the @_lid@ argument
toNote :: MetaID Note -> ST -> Bool -> RangePosition -> Users.LoginId -> Note
toNote nid desc public range _lid = Note nid desc public (unRangePosition range)

createNote :: ID Edit -> Create Note -> DB Note
createNote pid note = do
  userId <- dbUser
  let snote = S.Note
          (note ^. createNoteText)
          (note ^. createNotePublic)
          (RangePosition $ note ^. createNoteRange)
          (fromUserID userId)
  mid <- createMetaID snote
  addConnection S.PN pid (mid ^. miID)
  pure $ S.noteElim (toNote mid) snote

getNote :: ID Note -> DB Note
getNote = getMetaEntity (S.noteElim . toNote)


-- * Question

-- FIXME: user the @_lid@ argument
toQuestion :: MetaID Question -> ST -> Bool -> Bool -> RangePosition -> Users.LoginId -> Question
toQuestion qid text answ pblc range _lid = Question qid text answ pblc (unRangePosition range)

createQuestion :: ID Edit -> Create Question -> DB Question
createQuestion pid question = do
  userId <- dbUser
  let squestion = S.Question
          (question ^. createQuestionText)
          False -- Not answered
          (question ^. createQuestionPublic)
          (RangePosition $ question ^. createQuestionRange)
          (fromUserID userId)
  mid <- createMetaID squestion
  addConnection S.PQ pid (mid ^. miID)
  pure $ S.questionElim (toQuestion mid) squestion

getQuestion :: ID Question -> DB Question
getQuestion = getMetaEntity (S.questionElim . toQuestion)


-- * Discussion

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
          (RangePosition $ disc ^. createDiscussionRange)
          (fromUserID userId)
  mid <- createMetaID sdiscussion
  addConnection S.PD pid (mid ^. miID)
  let sstatement = S.Statement
          (disc ^. createDiscussionStatementText)
          Nothing -- Top level node
  void $ saveStatement (mid ^. miID) sstatement
  getDiscussion $ mid ^. miID

rebaseDiscussion :: ID Edit -> ID Discussion -> (Range Position -> Range Position) -> DB Discussion
rebaseDiscussion eid did tr = do
  d <- getEntityRep did
  let sdiscussion = S.discussionElim (\pblc (RangePosition r) -> S.Discussion pblc (RangePosition $ tr r)) d
  mid <- createMetaID sdiscussion
  addConnection S.PD eid (mid ^. miID)
  sts <- statementsOfDiscussion did
  forM_ sts $ \sid -> addConnection S.DS (mid ^. miID) sid
  getDiscussion $ mid ^. miID

getDiscussion :: ID Discussion -> DB Discussion
getDiscussion did = do
  (mid, d) <- getMetaEntity (,) did
  s <- statementsOfDiscussion did
  t <- buildTree (^. statementParent) (^. statementID) <$> mapM getStatement s
  -- FIXME: use the @_lid@ argument
  pure $ S.discussionElim (\pblc range _lid -> Discussion mid pblc (unRangePosition range) t) d


statementsOfDiscussion :: ID Discussion -> DB [ID Statement]
statementsOfDiscussion did = do
  opts <- dbSelectOpts
  liftDB $
    foreignKeyField S.dSStatement <$$> selectList [S.DSDiscussion ==. S.idToKey did] opts

discussionOfStatement :: ID Statement -> DB (ID Discussion)
discussionOfStatement sid = do
  opts <- dbSelectOpts
  unique "discussionOfStatement" =<< liftDB
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
getAnswer = getMetaEntity (S.answerElim . toAnswer)

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
  did <- unique "createStatement" ds
  let sstatement = S.Statement
          (statement ^. createStatementText)
          (Just $ S.idToKey sid)
  saveStatement did sstatement

getStatement :: ID Statement -> DB Statement
getStatement = getMetaEntity (S.statementElim . toStatement)


-- * User

-- | FUTUREWORK: this is an instance of the VarArg problem: the functions in 'UserStorageBackend'
-- class take different numbers of arguments, but they always take `db` first.  it would be nice to,
-- instead of:
--
-- >>> runUsersCmd (\h -> Users.createUser h user)
-- >>> runUsersCmd (\h -> Users.authUser h username password sessionDuration)
--
-- we could write:
--
-- >>> runUsersCmdVarArg Users.createUser user
-- >>> runUsersCmdVarArg Users.authUser username password sessionDuration
runUsersCmd :: (Users.Persistent -> IO a) -> DB a
runUsersCmd cmd = liftDB . ReaderT $ \(sqlBackend :: SqlBackend) ->
  liftIO $ cmd (Users.Persistent (`runReaderT` sqlBackend))


-- * Group

toGroup :: [ID Group] -> [ID Group] -> MetaID Group -> ST -> ST -> Group
toGroup parents children gid title desc =
  Group gid title desc parents children

createGroup :: Create Group -> DB Group
createGroup group = do
  let sgroup = S.Group
        (group ^. createGroupTitle)
        (group ^. createGroupDesc)
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
  getMetaEntity (S.groupElim . toGroup parents children) gid

getGroups :: DB [Group]
getGroups = do
  gids <- liftDB $ selectKeysList [] []
  mapM (getGroup . S.keyToId) gids

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
