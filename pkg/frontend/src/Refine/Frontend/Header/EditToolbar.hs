{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Frontend.Header.EditToolbar where

import Refine.Frontend.Prelude

import           Refine.Common.Types
import           Refine.Common.VDoc.OT (docRanges)
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Access


mkEditToolbarProps :: HasCallStack => Maybe Edit -> EditorState -> EditToolbarProps
mkEditToolbarProps medit = EditToolbarProps isInitial . mkLinkEditorProps
  where
    isInitial = if maybe False null $ medit ^? _Just . editSource . unEditSource
      then EditIsInitial
      else EditIsNotInitial

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

getDocumentState :: AccessState -> GlobalState -> DocumentState
getDocumentState as gs@(view gsEditID -> Just{})
  = mapDocumentState
      (const . fromMaybe False
             $ (==) <$> (as ^? accLoginState . lsCurrentUser . loggedInUser . userID . to UserID)
                    <*> ((^. editMetaID . miMeta . metaCreatedBy) <$> cacheLookup gs eid))
      (const $ gsRawContent gs)
      (fromMaybe (error "edit is not in cache") . cacheLookup gs)
      (\(did, ed) -> discussionProps (maybe (Left did) Right $ cacheLookup gs did)
                                     (gsRawContent gs)
                                     (StatementPropDetails
                                        ed
                                        (as ^? accLoginState . lsCurrentUser . loggedInUser . userID)
                                        ((^. userName) <$> (gs ^. gsServerCache . scUsers))
                                     )
                                     (gs ^. gsHeaderState . hsDiscussionFlatView)
      )
      dst
  where
    dst = gs ^. gsDocumentState
    eid = case dst of
      DocumentStateDiff _ _ _ i _ _ -> i
      _ -> error "impossible - getDocumentState"
getDocumentState _ _
  = error "getDocumentState: no gsVDoc"

wipeDocumentState :: AccessState -> GlobalState -> WipedDocumentState
wipeDocumentState as gs = case getDocumentState as gs of
  DocumentStateView{}                  -> WipedDocumentStateView
  DocumentStateDiff i _ _ edit collapsed editable -> WipedDocumentStateDiff i edit collapsed editable
  DocumentStateEdit es _ meid          -> WipedDocumentStateEdit $ mkEditToolbarProps (cacheLookup gs =<< meid) es
  DocumentStateDiscussion dp           -> WipedDocumentStateDiscussion $ DiscussionToolbarProps
                                            (either id (^.discussionID) $ dp ^. discPropsDiscussion)
                                            (dp ^. discPropsFlatView)

editToolbar_ :: HasCallStack => EditToolbarProps -> ReactElementM eventHandler ()
editToolbar_ ep = do
  let editButton icon = defaultIconButtonProps @[GlobalAction]
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
    & iconButtonPropsOnClick      .~ [DocumentAction $ DocumentToggleBlockType Header1]

  iconButton_ $ editButton "Edit_toolbar_h2"
    & iconButtonPropsListKey      .~ "h2"
    & iconButtonPropsLabel        .~ "header 2"
    & iconButtonPropsOnClick      .~ [DocumentAction $ DocumentToggleBlockType Header2]

  iconButton_ $ editButton "Edit_toolbar_h3"
    & iconButtonPropsListKey      .~ "h3"
    & iconButtonPropsLabel        .~ "header 3"
    & iconButtonPropsOnClick      .~ [DocumentAction $ DocumentToggleBlockType Header3]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Edit_toolbar_bold"
    & iconButtonPropsListKey      .~ "bold"
    & iconButtonPropsLabel        .~ "bold"
    & iconButtonPropsOnClick      .~ [DocumentAction $ DocumentToggleStyle Bold]

  iconButton_ $ editButton "Edit_toolbar_italic"
    & iconButtonPropsListKey      .~ "italic"
    & iconButtonPropsLabel        .~ "italic"
    & iconButtonPropsOnClick      .~ [DocumentAction $ DocumentToggleStyle Italic]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Edit_toolbar_bullets"
    & iconButtonPropsListKey      .~ "bullets"
    & iconButtonPropsLabel        .~ "bullets"
    & iconButtonPropsOnClick      .~ [DocumentAction $ DocumentToggleBlockType BulletPoint]

  iconButton_ $ editButton "Edit_toolbar_numbers"
    & iconButtonPropsListKey      .~ "numbers"
    & iconButtonPropsLabel        .~ "numbers"
    & iconButtonPropsOnClick      .~ [DocumentAction $ DocumentToggleBlockType EnumPoint]

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
        LinkButtonDeletes  -> [DocumentAction DocumentRemoveLink]
        LinkButtonAdds l   -> [HeaderAction $ OpenEditToolbarLinkEditor l]

   in ibutton_ props

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  iconButton_ $ editButton "Edit_toolbar_undo"
    & iconButtonPropsListKey      .~ "undo"
    & iconButtonPropsLabel        .~ "undo"
    & iconButtonPropsOnClick      .~ [DocumentAction DocumentUndo]

  iconButton_ $ editButton "Edit_toolbar_redo"
    & iconButtonPropsListKey      .~ "redo"
    & iconButtonPropsLabel        .~ "redo"
    & iconButtonPropsOnClick      .~ [DocumentAction DocumentRedo]

  div_ ["className" $= "c-vdoc-toolbar__separator"] ""

  let props :: IbuttonProps [GlobalAction]
      props = emptyIbuttonProps "Save" [DocumentAction . DocumentSave . FormBegin $ ep ^. editToolbarPropsInitial]
        & ibListKey      .~ "save"
        & ibLabel        .~ "save"
        & ibEnabled      .~ True
        & ibSize         .~ XXLarge
   in ibutton_ props
