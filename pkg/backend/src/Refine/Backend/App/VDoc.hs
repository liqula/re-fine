{-# LANGUAGE CPP #-}
#include "language_backend.hs"

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.VDoc where
#include "import_backend.hs"

import           Data.Maybe
import qualified Web.Users.Types as Users

import           Refine.Backend.App.Access
import           Refine.Backend.App.Core
import           Refine.Backend.App.Smtp
import           Refine.Backend.App.User
import qualified Refine.Backend.Database.Class as DB
import           Refine.Backend.Database.Entity (fromUserID)
import qualified Refine.Common.Access.Policy as AP
import           Refine.Common.Types
import           Refine.Common.Types.Core (OTDoc)
import qualified Refine.Common.OT as OT
import           Refine.Common.VDoc.OT
import           Refine.Common.VDoc.Draft

listVDocs :: App [VDoc]
listVDocs = do
  appLog "listVDocs"
  db $ mapM DB.getVDoc =<< DB.listVDocs

-- | Creates a 'VDoc'.
createVDoc :: CreateVDoc -> App VDoc
createVDoc pv = do
  appLog "createVDoc"
  assertCreds $ AP.createVDoc (pv ^. createVDocGroup)
  vdoc <- db $ DB.createVDoc pv
  invalidateCaches $ Set.fromList [CacheKeyGroup $ pv ^. createVDocGroup]
  pure vdoc

updateVDoc :: ID VDoc -> UpdateVDoc -> App VDoc
updateVDoc vid (UpdateVDoc title abstract) = do
  appLog "updateVDoc"
  vdoc' <- db $ do
    vdoc <- DB.getVDoc vid
    let vdoc' = vdoc
          & vdocTitle .~ title
          & vdocAbstract .~ abstract
    DB.updateVDoc vid vdoc'
    pure vdoc'
  invalidateCaches $ Set.fromList [CacheKeyVDoc vid]
  pure vdoc'

getVDoc :: ID VDoc -> App VDoc
getVDoc i = do
  appLog "getVDoc"
  db $ DB.getVDoc i

getVDocVersion :: ID Edit -> App RawContent
getVDocVersion eid = do
  appLog "getVDocVersion"
  db $ DB.getVersion eid

updateEdit :: ID Edit -> CreateEdit -> App Edit
updateEdit eid edit = do
  appLog "updateEdit"
  -- assertPerms eid [Create]  -- FIXME: http://zb2/re-fine/re-fine/issues/358
  db $ do
    olddoc :: RawContent <- DB.getVersion eid
    let new :: RawContent = edit ^. createEditVDocVersion
    dff <- either error pure $
            -- error reporting is not great:
            -- - this is an internal error, and it may be possible to rule it out on the type level.
            -- - this should be an 'AppError', but it happens in the DB monad.
            -- - since we 'error' out sloppily, the backend console says 'SQLite3 returned ErrorError
            --   while attempting to perform step.' and the frontend says 'Error in $: Failed
            --   reading: not a valid json value'.
        OT.diff (deleteMarksFromRawContent olddoc) (deleteMarksFromRawContent new)
    DB.updateEdit eid edit
    DB.updateEditSource eid $ \_ e -> e <> dff
  invalidateCaches $ Set.fromList [CacheKeyEdit eid]
  db $ DB.getEdit eid

addEdit :: ID Edit -> CreateEdit -> App Edit
addEdit baseeid edit = do
  appLog "addEdit"
  -- assertPerms baseeid [Create]  -- FIXME: http://zb2/re-fine/re-fine/issues/358
    -- (note that the user must have create permission on the *base
    -- edit*, not the edit about to get created.)
  ed <- db $ do
    rid <- DB.vdocOfEdit baseeid
    olddoc :: RawContent <- DB.getVersion baseeid
    dff <- either error pure $
            -- error reporting is not great:
            -- - this is an internal error, and it may be possible to rule it out on the type level.
            -- - this should be an 'AppError', but it happens in the DB monad.
            -- - since we 'error' out sloppily, the backend console says 'SQLite3 returned ErrorError
            --   while attempting to perform step.' and the frontend says 'Error in $: Failed
            --   reading: not a valid json value'.
        OT.diff (deleteMarksFromRawContent olddoc)
                (deleteMarksFromRawContent $ edit ^. createEditVDocVersion)
    DB.createEdit rid (EditSource [(dff, baseeid)]) edit
  invalidateCaches $ Set.fromList [CacheKeyEdit baseeid]
  pure ed

addEditAndMerge :: ID Edit -> CreateEdit -> App Edit
addEditAndMerge baseeid edit = do
  e <- addEdit baseeid edit
  mergeEdit $ e ^. editID
  pure e

getEdit :: ID Edit -> App Edit
getEdit = db . DB.getEdit

addMerge :: ID Edit -> ID Edit -> ID Edit -> App Edit
addMerge base eid1 eid2 = do
  appLog $ "merge " <> show eid1 <> " with " <> show eid2 <> " based on " <> show base
  (either throwError pure =<<) . db $ do
    rid <- DB.vdocOfEdit eid1
    rid' <- DB.vdocOfEdit eid2
    if rid' /= rid then pure . Left $ AppMergeError base eid1 eid2 (cs $ show rid' <> " /= " <> show rid) else do
      edit1 <- DB.getEdit eid1
      edit2 <- DB.getEdit eid2
      case ( [e | (e, d) <- edit1 ^. editSource . unEditSource, d == base]
           , [e | (e, d) <- edit2 ^. editSource . unEditSource, d == base]
           ) of
        ([e1], [e2]) -> Right <$> do
          doc :: RawContent <- DB.getVersion base
          let (diff1, diff2) = OT.merge doc e1 e2
              newdoc = OT.patch (e1 <> diff1) doc
          DB.createEdit rid (EditSource [(diff1, eid1), (diff2, eid2)])
            $ CreateEdit (edit2 ^. editDesc) newdoc (edit2 ^. editKind)
        res -> pure . Left $ AppMergeError base eid1 eid2 (cs $ show res)

mergeEdit :: ID Edit -> App ()
mergeEdit = rebaseHeadToEdit

-- | Move HEAD marker (which is our name for the latest release) to a given edit.  All contributions
-- based on the old HEAD are rebased to the new HEAD.  All authors of rebased contributions are
-- notified by email.
rebaseHeadToEdit :: ID Edit -> App ()
rebaseHeadToEdit eid = do
  appLog $ "rebase to " <> show eid
  (vid, hid, ch) <- db $ do
    vid <- DB.vdocOfEdit eid
    vdoc <- DB.getVDoc vid
    let hid = vdoc ^. vdocHeadEdit
    DB.moveVDocHead vid eid
    (,,) vid hid <$> DB.getEditChildren hid
  when (eid `notElem` ch) . throwError $ AppRebaseError eid

  -- move edits
  movedEditOwners :: [MetaInfo]
    <- view (editMetaID . miMeta)
       <$$> forM (filter (/= eid) ch) (addMerge hid eid)

  movedCommentOwners :: [MetaInfo] <- db $ do
    edit <- DB.getEdit eid
    base <- DB.getEdit hid
    let diff = head [di | (di, d) <- edit ^. editSource . unEditSource, d == hid]
        trRange = transformRangeOTDoc
                  (concat (coerce diff :: [OT.Edit OTDoc]))
                  (rawContentToDoc $ base ^. editVDocVersion)

    -- move discussions
    movedDiscussions :: [Discussion]
       <- forM (Set.toList $ base ^. editDiscussions') $ \did -> DB.rebaseDiscussion eid did trRange
    pure $ view (discussionMetaID . miMeta) <$> movedDiscussions

  notifyContributionAuthorsOfMovement $ movedEditOwners <> movedCommentOwners
  invalidateCaches $ Set.fromList [CacheKeyVDoc vid]
    -- what needs invalidation?
    --  * vdoc: yes
    --  * old edit: no
    --  * all children of old edit: no (the rebases have new IDs and will be found by the clients via the vdoc)
    --  * new edit: no (just created)

notifyContributionAuthorsOfMovement :: [MetaInfo] -> App ()
notifyContributionAuthorsOfMovement mis = forM_ uids $ \uid -> do
  Just (u :: Users.User) <- dbUsersCmd (`Users.getUserById` fromUserID uid)
  sendMailTo $ EmailMessage u
    "your stuff has changed."
    "some of the things you did on the refine platform have been rebased due to other changes in the same document(s)."
  where
    getuid (UserID uid) = Just uid
    getuid _ = Nothing
    uids = nub . mapMaybe getuid $ (view metaCreatedBy <$> mis) <> (view metaChangedBy <$> mis)

-- | Throw an error if chunk range does not fit 'RawContent' identified by edit.
-- FIXME: for RawContent this still needs to be implemented.
validateCreateChunkRange :: ID Edit -> Range Position -> App ()
validateCreateChunkRange _ _ = pure ()  -- throwError AppVDocVersionError

toggleSimpleVoteOnEdit :: ID Edit -> Vote -> App Bool
toggleSimpleVoteOnEdit eid v = do
  let f (Just v') | v' == v = Nothing
      f _ = Just v
  rebased <- withCurrentUser $ \user -> changeSimpleVoteOnEdit eid $ Map.alter f user
  invalidateCaches $ Set.fromList [CacheKeyEdit eid]
  pure rebased

deleteSimpleVoteOnEdit :: ID Edit -> App ()
deleteSimpleVoteOnEdit eid = do
  void . withCurrentUser $ changeSimpleVoteOnEdit eid . Map.delete
  invalidateCaches $ Set.fromList [CacheKeyEdit eid]

atLeastOneUpvote :: VoteCount -> Bool
atLeastOneUpvote vc = fromMaybe 0 (Map.lookup Yeay vc) >= 1

rebasePossible :: DB.Database db => ID Edit -> db Bool
rebasePossible eid = do
  vd <- (^. vdocHeadEdit) <$> (DB.vdocOfEdit eid >>= DB.getVDoc)
  ed <- DB.getEdit eid
  pure $ vd `elem` (snd <$> (ed ^. editSource . unEditSource))

changeSimpleVoteOnEdit :: ID Edit -> (Votes -> Votes) -> App Bool{-rebase happened-}
changeSimpleVoteOnEdit eid f = do
  mkrebase <- db $ do
    DB.updateVotes eid f
    vs <- DB.getVoteCount eid
    if atLeastOneUpvote vs then rebasePossible eid else pure False
  when mkrebase $ rebaseHeadToEdit eid
  pure mkrebase

getSimpleVotesOnEdit :: ID Edit -> App VoteCount
getSimpleVotesOnEdit eid = db $ DB.getVoteCount eid
