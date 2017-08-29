{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Backend.Database.Entity where
#include "import_backend.hs"

import qualified Database.Persist as Persist
import           Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)
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

-- FIXME: Generate this as the part of the lentil library.
type instance S.EntityRep MetaInfo   = S.MetaInfo
type instance S.EntityRep VDoc       = S.VDoc
type instance S.EntityRep Edit       = S.Edit
type instance S.EntityRep Discussion = S.Discussion
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
idNotFound i = notFound $ unwords ["not found:", show i, "::", show $ typeOf i]

unique :: String -> [a] -> DB a
unique _      [x] = pure x
unique errmsg []  = notFound errmsg
unique errmsg _   = notUnique errmsg

getEntityRep :: (ToBackendKey SqlBackend (S.EntityRep e), Typeable e)
             => ID e -> DB (S.EntityRep e)
getEntityRep eid = do
  e <- liftDB . Persist.get $ S.idToKey eid
  maybe (idNotFound eid) pure e

-- | Access the key like field in an entity and convert the key value
-- to the Domain ID.
--
-- Examples:
-- >>> foreignKeyField S.PCComment (Entity pcid (S.PC pid cid)) == cid
-- >>> foreignKeyField S.PCEdit    (Entity pcid (S.PC pid cid)) == pid
foreignKeyField
  :: ToBackendKey SqlBackend (S.EntityRep a)
  => (b -> Key (S.EntityRep a)) -> Persist.Entity b -> ID a
foreignKeyField column = S.keyToId . column . entityVal

-- NOTES: How to handle associations? What to update, what to keep?
vDocToRecord :: VDoc -> DB S.VDoc
vDocToRecord (VDoc _i t a r g _) = pure (S.VDoc t a (Just $ S.idToKey r) (S.idToKey g))

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

getMetaInfo :: HasMetaInfo a => ID a -> DB (Persist.Entity (S.EntityRep MetaInfo))
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

toVDoc :: EditStats -> MetaID VDoc -> Title -> Abstract -> Maybe (Key S.Edit) -> Key S.Group -> VDoc
toVDoc stats vid title abstract (Just repoid) g = VDoc vid title abstract (S.keyToId repoid) (S.keyToId g) stats
toVDoc _ _ _ _ Nothing _ = error "impossible"

listVDocs :: DB [ID VDoc]
listVDocs = do
  opts <- dbSelectOpts
  liftDB $ S.keyToId <$$> selectKeysList [] opts

createVDoc :: CreateVDoc -> DB VDoc
createVDoc pv = do
  let svdoc = S.VDoc
        (pv ^. createVDocTitle)
        (pv ^. createVDocAbstract)
        Nothing -- hack: use a dummy key which will be replaced by a proper one before createVDoc returns
        (S.idToKey $ pv ^. createVDocGroup)
  mid <- createMetaID svdoc
  e <- createEdit (mid ^. miID) mempty CreateEdit
    { _createEditDesc        = "initial document version"
    , _createEditVDocVersion = pv ^. createVDocInitVersion
    , _createEditKind        = Initial
    }
  let e' = S.idToKey (e ^. editMetaID . miID) :: Key S.Edit
  liftDB $ update (S.idToKey $ mid ^. miID) [S.VDocHeadId =. Just e']
  getVDoc (mid ^. miID)

getVDoc :: ID VDoc -> DB VDoc
getVDoc i = do
  (mid, x) <- getMetaEntity (,) i
  let eid = S.vDocElim (\_ _ (Just ei) _ -> ei) x
  editIds <- foreignKeyField S.parentChildChild <$$> liftDB (selectList [S.ParentChildParent ==. eid] [])
  discussionIds <- foreignKeyField S.pDDiscussion <$$> liftDB (selectList [S.PDEdit ==. eid] [])
  statementIds <- forM discussionIds $ \did ->
    entityKey <$$> liftDB (selectList [S.StatementDiscussion ==. S.idToKey did] [])
  editsMeta <- (^. miMeta) <$$> mapM getMeta (S.keyToId eid: editIds :: [ID Edit])
  discussionsMeta <- (^. miMeta) <$$> mapM getMeta (discussionIds :: [ID Discussion])
  statementsMeta <- (^. miMeta) <$$> mapM getMeta (S.keyToId <$> concat statementIds :: [ID Statement])
  let metas = mid ^. miMeta: (editsMeta <> discussionsMeta <> statementsMeta)
      users = length $ nub [u | UserID u <- ((^. metaCreatedBy) <$> metas) <> ((^. metaChangedBy) <$> metas)]
      stats = EditStats users (length editIds) (length discussionIds)
  pure $ S.vDocElim (toVDoc stats mid) x

headOfVDoc :: ID VDoc -> DB (ID Edit)
headOfVDoc i = do
  x <- getEntityRep i
  pure $ S.vDocElim (\_ _ (Just ei) _ -> S.keyToId ei) x


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

updateEdit :: ID Edit -> CreateEdit -> DB ()
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
  discussions <- Map.fromList <$> editDiscussions eid
  children    <- Set.fromList <$> getEditChildren eid
  getMetaEntity (\mid -> S.editElim $
                  \desc d vdoc kind (DBVotes vs) ->
                    Edit mid desc kind src (S.keyToId vdoc) d vs children discussions) eid

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

getVersion :: ID Edit -> DB RawContent
getVersion eid = S.editElim (\_ vdoc _ _ _ -> vdoc) <$> getEntityRep eid

editDiscussions :: ID Edit -> DB [(ID Discussion, Range Position)]
editDiscussions pid = do
  opts <- dbSelectOpts
  liftDB $
    S.pDElim (\_ a (RangePosition b) -> (S.keyToId a, b)) . entityVal
      <$$> selectList [S.PDEdit ==. S.idToKey pid] opts

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
vdocOfEdit eid = S.editElim (\_ _ vid _ _ -> S.keyToId vid) <$> getEntityRep eid


-- * Discussion

createDiscussion :: ID Edit -> CreateDiscussion (Range Position) -> DB Discussion
createDiscussion pid disc = do
  let sdiscussion = S.Discussion
          (DBVotes mempty)
          (disc ^. createDiscussionIsNote)
  mid <- createMetaID sdiscussion
  void . liftDB . insert $
    S.PD (S.idToKey pid) (S.idToKey $ mid ^. miID) (RangePosition $ disc ^. createDiscussionRange)
  vid <- view editVDoc <$> getEdit pid
  void . saveStatement vid $ S.Statement
          (disc ^. createDiscussionStatementText)
          Nothing -- Top level node
          (S.idToKey $ mid ^. miID)
  getDiscussion $ mid ^. miID

rebaseDiscussion :: ID Edit -> ID Edit -> ID Discussion -> (Range Position -> Range Position) -> DB Discussion
rebaseDiscussion baseid eid did tr = do
  Just conn <- liftDB . getBy $ S.UniPD (S.idToKey baseid) (S.idToKey did)
  let newrange = S.pDElim (\_ _ r -> RangePosition . tr $ unRangePosition r) $ entityVal conn
  void . liftDB . insert $ S.PD (S.idToKey eid) (S.idToKey did) newrange
  getDiscussion did

statementsOfDiscussion :: ID Discussion -> DB [ID Statement]
statementsOfDiscussion did = do
  opts <- dbSelectOpts
  liftDB $
    (S.keyToId . entityKey) <$$> selectList [S.StatementDiscussion ==. S.idToKey did] opts

getDiscussion :: ID Discussion -> DB Discussion
getDiscussion did = do
  (mid, d) <- getMetaEntity (,) did
  s@(_:_) <- statementsOfDiscussion did
  t <- buildTree (^. statementParent) (^. statementID) <$> mapM getStatement s
  vid <- vdocOfDiscussion did
  pure $ S.discussionElim (Discussion mid vid t . unDBVotes) d

discussionOfStatement :: ID Statement -> DB (ID Discussion)
discussionOfStatement sid = do
  e <- getEntityRep sid
  pure $ S.statementElim (\_ _ i -> S.keyToId i) e

vdocOfDiscussion :: ID Discussion -> DB (ID VDoc)
vdocOfDiscussion did = do
  opts <- dbSelectOpts
  ((editOfDiscussion :: Persist.Entity S.PD) : _) <- liftDB $ selectList [S.PDDiscussion ==. S.idToKey did] opts
  let eid :: ID Edit = (S.pDElim (\i _ _ -> S.keyToId i) . entityVal) editOfDiscussion
  view editVDoc <$> getEdit eid

updateDiscussionVotes :: ID Discussion -> (Votes -> Votes) -> DB ()
updateDiscussionVotes i f = do
  vs <- getDiscussionVotes i
  liftDB $ update (S.idToKey i) [ S.DiscussionVotes =. DBVotes (f vs) ]

getDiscussionVotes :: ID Discussion -> DB Votes
getDiscussionVotes i = do
  getMetaEntity (\_ -> S.discussionElim $ \(DBVotes vs) _ -> vs) i


-- * Statement

toStatement :: MetaID Statement -> ID VDoc -> ST -> Maybe (Key S.Statement) -> Key S.Discussion -> Statement
toStatement sid vid text parent _ = Statement sid vid text (S.keyToId <$> parent)

saveStatement :: ID VDoc -> S.Statement -> DB Statement
saveStatement vid sstatement = do
  mid <- createMetaID sstatement
  pure $ S.statementElim (toStatement mid vid) sstatement

createStatement :: ID Statement  -> CreateStatement -> DB Statement
createStatement sid statement = do
  did <- discussionOfStatement sid
  vid <- vdocOfDiscussion did
  saveStatement vid $ S.Statement
          (statement ^. createStatementText)
          (Just $ S.idToKey sid)
          (S.idToKey did)

updateStatement :: ID Statement  -> CreateStatement -> DB Statement
updateStatement sid statement = do
  () <- liftDB $ update (S.idToKey sid) [ S.StatementText =. statement ^. createStatementText ]
  modifyMetaID sid
  getStatement sid

getStatement :: ID Statement -> DB Statement
getStatement sid = do
  (mid, sstatement) <- getMetaEntity (,) sid
  vid <- vdocOfDiscussion =<< discussionOfStatement sid
  pure $ S.statementElim (toStatement mid vid) sstatement


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

toGroup :: [ID Group] -> [ID Group] -> [ID VDoc] -> [ID User] -> MetaID Group -> ST -> ST -> Group
toGroup parents children vdocs members gid title desc =
  Group gid title desc parents children vdocs members

createGroup :: CreateGroup -> DB Group
createGroup group = do
  let sgroup = S.Group
        (group ^. createGroupTitle)
        (group ^. createGroupDesc)
  mid <- createMetaID sgroup
  forM_ (group ^. createGroupParents) $ \parent -> addConnection S.SubGroup parent (mid ^. miID)
  forM_ (group ^. createGroupChildren) $ \child -> addConnection S.SubGroup (mid ^. miID) child
  forM_ (Map.toList $ group ^. createGroupMembers) $ \(user, member)
    -> when member $ assignGroupRole (mid ^. miID) user GroupMember
  getGroup (mid ^. miID)

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

getVDocsOfGroup :: ID Group -> DB [ID VDoc]
getVDocsOfGroup gid = do
  opts <- dbSelectOpts
  (S.keyToId . entityKey)
    <$$> liftDB (selectList [S.VDocGroup ==. S.idToKey gid] opts)

getGroup :: ID Group -> DB Group
getGroup gid = do
  parents  <- getParentsOfGroup  gid
  children <- getChildrenOfGroup gid
  vdocs    <- getVDocsOfGroup    gid
  roles    <- liftDB $ selectList [ S.GroupRolesGroup ==. S.idToKey gid
                                  , S.GroupRolesRole ==. GroupMember
                                  ] []
  let members = S.groupRolesElim (\_gid uid _role -> S.keyToId uid) . entityVal <$> roles
  getMetaEntity (S.groupElim . toGroup parents children vdocs members) gid

getGroups :: DB [Group]
getGroups = do
  gids <- liftDB $ selectKeysList [] []
  mapM (getGroup . S.keyToId) gids

modifyGroup :: ID Group -> CreateGroup -> DB Group
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

  forM_ (Map.toList $ group ^. createGroupMembers) $ \(user, member) -> do
    roles <- getGroupRolesIn gid user
    if member
      then when (GroupMember `notElem` roles) $ assignGroupRole gid user GroupMember
      else when (GroupMember `elem` roles) $ unassignGroupRole gid user GroupMember

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

assignGroupRole :: ID Group -> ID User -> GroupRole -> DB ()
assignGroupRole gid uid role = liftDB $ do
  void . insert $ S.GroupRoles (S.idToKey gid) (S.idToKey uid) role

getGroupRolesIn :: ID Group -> ID User -> DB [GroupRole]
getGroupRolesIn gid uid = do
  opts <- dbSelectOpts
  roles <- liftDB $ selectList [S.GroupRolesGroup ==. S.idToKey gid, S.GroupRolesUser ==. S.idToKey uid] opts
  pure $ (S.groupRolesElim (\_gid _uid role' -> role') . entityVal) <$> roles

getGroupRoles :: ID User -> DB [(GroupRole, ID Group)]
getGroupRoles uid = do
  opts <- dbSelectOpts
  roles <- liftDB $ selectList [S.GroupRolesUser ==. S.idToKey uid] opts
  pure $ (S.groupRolesElim (\gid _uid role' -> (role', S.keyToId gid)) . entityVal) <$> roles

unassignGroupRole :: ID Group -> ID User -> GroupRole -> DB ()
unassignGroupRole gid uid role = liftDB $ do
  deleteWhere
    [ S.GroupRolesGroup ==. S.idToKey gid
    , S.GroupRolesUser  ==. S.idToKey uid
    , S.GroupRolesRole  ==. role
    ]


assignGlobalRole :: ID User -> GlobalRole -> DB ()
assignGlobalRole uid role = liftDB $ do
  void . insert $ S.GlobalRoles (S.idToKey uid) role

getGlobalRoles :: ID User -> DB [GlobalRole]
getGlobalRoles uid = do
  opts <- dbSelectOpts
  roles <- liftDB $ selectList [S.GlobalRolesUser ==. S.idToKey uid] opts
  pure $ (S.globalRolesElim (\__uid role' -> role') . entityVal) <$> roles

unassignGlobalRole :: ID User -> GlobalRole -> DB ()
unassignGlobalRole uid role = liftDB $ do
  deleteWhere
    [ S.GlobalRolesUser  ==. S.idToKey uid
    , S.GlobalRolesRole  ==. role
    ]
