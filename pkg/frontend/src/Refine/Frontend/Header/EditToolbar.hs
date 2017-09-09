{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -Wno-orphans #-}

module Refine.Frontend.Header.EditToolbar where
#include "import_frontend.hs"

import Control.Arrow ((+++))

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Common.VDoc.OT (docRanges)
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Document.Store
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Debug
import           Refine.Frontend.Types


mkEditToolbarProps :: HasCallStack => Maybe Edit -> EditToolbarProps
mkEditToolbarProps medit = EditToolbarProps isInitial  -- (mkLinkEditorProps undefined) -- FIXME: #452
  where
    isInitial = if maybe False null $ medit ^? _Just . editSource . unEditSource
      then EditIsInitial
      else EditIsNotInitial

{-
-- FIXME: #452
mkLinkEditorProps :: HasCallStack => EditorState -> LinkEditorProps
mkLinkEditorProps es
    | rangeIsEmpty rc sel = LinkButtonDisabled
    | any (doRangesOverlap sel) linkranges = LinkButtonDeletes
    | otherwise = LinkButtonAdds . cs $ rangeText BlockBoundaryIsEmpty rc sel
  where
    sel = _selectionRange . fromSelectionState rc $ getSelection es
    rc = convertToRaw $ getCurrentContent es
    linkranges = fmap (fromStyleRange rc) . unRanges . mconcat . fmap (toStyleRanges rc . snd)
        $ docRanges False lineElemLength (\((Atom l, _), _) -> [() | isLink l]) rc
      where
        isLink (Just (EntityLink _)) = True
        isLink _ = False
-}

getDocumentState :: AccessState -> GlobalState -> DocumentState
getDocumentState as gs@(gsEditID' -> Just baseid)
  = mapDocumentState
      (const . fromMaybe False
             $ (==) <$> (as ^? accLoginState . lsCurrentUser . loggedInUser . to UserID)
                    <*> ((^. editMetaID . miMeta . metaCreatedBy) <$> cacheLookup gs eid))
      (const $ gsRawContent gs)
      (fromMaybe (error "edit is not in cache") . cacheLookup gs)
      (\(did, ed) -> discussionProps
                            (fromMaybe (Left did) $ do
                                d <- cacheLookup gs did
                                e <- cacheLookup gs =<< baseid
                                r <- Map.lookup did $ e ^. editDiscussions'
                                pure $ Right (r, d))
                            (gsRawContent gs)
                            (StatementPropDetails
                               ed
                               (as ^? accLoginState . lsCurrentUser . loggedInUser)
                               ((^. userName) <$> (gs ^. gsServerCache . scUsers))
                            )
                            (gs ^. gsHeaderState . hsDiscussionFlatView)
      )
      dst
  where
    dst = gs ^. gsDocumentState
    eid = case dst of
      DocumentStateDiff _ _ i _ _ -> i
      _ -> error "impossible - getDocumentState"
getDocumentState _ _
  = error "getDocumentState: no gsVDoc"

-- | TODO:c rename to 'getWipedDocumentState'
wipeDocumentState :: AccessState -> GlobalState -> WipedDocumentState
wipeDocumentState as gs = case getDocumentState as gs of
  DocumentStateView{}                  -> WipedDocumentStateView
  DocumentStateDiff i _ edit collapsed editable -> WipedDocumentStateDiff i edit collapsed editable
  DocumentStateEdit _ meid             -> WipedDocumentStateEdit $ mkEditToolbarProps (cacheLookup gs =<< meid)
  DocumentStateDiscussion dp           -> WipedDocumentStateDiscussion $ DiscussionToolbarProps
                                            (either (const Nothing) (Just . (^. discussionID)) disc)
                                            (dp ^. discPropsFlatView)
                                            (either (error "impossible @wipeDocumentState") (^. discussionIsNote) disc)
                                            (votesToCount $ either (error "impossible @wipeDocumentState") (^. discussionVotes) disc)
    where disc = id +++ snd $ dp ^. discPropsDiscussion

-- (this is not a good place for this instance, but it avoids import cycles, and the class is
-- deprecated anyway.  when refactoring Icon.hs, we need to think about how to do this better, see
-- #439.)
instance IconButtonPropsOnClick [EditorStoreAction] where
  runIconButtonPropsOnClick _ _ = mconcat . fmap dispatch
  defaultOnClick = assert False $ error "defaultOnClick is deprecated."

editToolbar :: View' '[EditToolbarProps]
editToolbar = mkView' "editToolbar" $ \ep -> do
  let editButton icon = defaultIconButtonProps @[EditorStoreAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-" <> icon, "dark") XXLarge
        & iconButtonPropsElementName  .~ "btn-index"

  let props :: IbuttonProps [GlobalAction]
      props = emptyIbuttonProps "Close" [DocumentAction DocumentCancelSave, DocumentAction UpdateDocumentStateView]
        & ibListKey      .~ "cancel"
        & ibLabel        .~ "cancel"
        & ibEnabled      .~ True
        & ibSize         .~ XXLarge
   in ibutton_ props

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Edit_toolbar_h1"
    & iconButtonPropsListKey      .~ "h1"
    & iconButtonPropsLabel        .~ "header 1"
    & iconButtonPropsOnClick      .~ [DocumentToggleBlockType Header1]

  iconButton_ $ editButton "Edit_toolbar_h2"
    & iconButtonPropsListKey      .~ "h2"
    & iconButtonPropsLabel        .~ "header 2"
    & iconButtonPropsOnClick      .~ [DocumentToggleBlockType Header2]

  iconButton_ $ editButton "Edit_toolbar_h3"
    & iconButtonPropsListKey      .~ "h3"
    & iconButtonPropsLabel        .~ "header 3"
    & iconButtonPropsOnClick      .~ [DocumentToggleBlockType Header3]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Edit_toolbar_bold"
    & iconButtonPropsListKey      .~ "bold"
    & iconButtonPropsLabel        .~ "bold"
    & iconButtonPropsOnClick      .~ [DocumentToggleStyle Bold]

  iconButton_ $ editButton "Edit_toolbar_italic"
    & iconButtonPropsListKey      .~ "italic"
    & iconButtonPropsLabel        .~ "italic"
    & iconButtonPropsOnClick      .~ [DocumentToggleStyle Italic]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Edit_toolbar_bullets"
    & iconButtonPropsListKey      .~ "bullets"
    & iconButtonPropsLabel        .~ "bullets"
    & iconButtonPropsOnClick      .~ [DocumentToggleBlockType BulletPoint]

  iconButton_ $ editButton "Edit_toolbar_numbers"
    & iconButtonPropsListKey      .~ "numbers"
    & iconButtonPropsLabel        .~ "numbers"
    & iconButtonPropsOnClick      .~ [DocumentToggleBlockType EnumPoint]

{- FIXME: #452
  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  let props :: IbuttonProps [GlobalAction]
      props = emptyIbuttonProps "Edit_toolbar_link" onclick
        & ibListKey      .~ "link"
        & ibLabel        .~ case ep ^. editToolbarPropsLinkEditor of
            LinkButtonDisabled -> "links"
            LinkButtonDeletes  -> "delete link"
            LinkButtonAdds _   -> "add link"
        & ibEnabled      .~ case ep ^. editToolbarPropsLinkEditor of
            LinkButtonDisabled -> False
            _                  -> True
        & ibSize         .~ XXLarge

      onclick = case ep ^. editToolbarPropsLinkEditor of
        LinkButtonDisabled -> []
        LinkButtonDeletes  -> [DocumentRemoveLink]
        LinkButtonAdds l   -> [HeaderAction $ OpenEditToolbarLinkEditor l]

   in ibutton_ props
-}

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Edit_toolbar_undo"
    & iconButtonPropsListKey      .~ "undo"
    & iconButtonPropsLabel        .~ "undo"
    & iconButtonPropsOnClick      .~ [DocumentUndo]

  iconButton_ $ editButton "Edit_toolbar_redo"
    & iconButtonPropsListKey      .~ "redo"
    & iconButtonPropsLabel        .~ "redo"
    & iconButtonPropsOnClick      .~ [DocumentRedo]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Save"
    & iconButtonPropsListKey      .~ "save"
    & iconButtonPropsLabel        .~ "save"
    & iconButtonPropsOnClick      .~ [DocumentRequestSave $ ep ^. editToolbarPropsInitial]

editToolbar_ :: HasCallStack => EditToolbarProps -> ReactElementM eventHandler ()
editToolbar_ = view_' editToolbar "editToolbar_"
