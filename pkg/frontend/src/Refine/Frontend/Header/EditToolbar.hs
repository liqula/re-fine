{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

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
import           Refine.Frontend.Icon.Svg as Svg
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

getDocumentStateProps :: AccessState -> GlobalState -> DocumentStateProps
getDocumentStateProps as gs@(view gsEditID -> Just baseid)
  = mapDocumentState
      (const . fromMaybe False
             $ (==) <$> (as ^? accLoginState . lsCurrentUser . loggedInUser . to UserID)
                    <*> ((^. editMetaID . miMeta . metaCreatedBy) <$> cacheLookup gs eid))
      (const $ gs ^. gsRawContent)
      (fromMaybe (error "edit is not in cache") . cacheLookup gs)
      (\(did, ed) -> discussionProps
                            (fromMaybe (Left did) $ do
                                d <- cacheLookup gs did
                                e <- cacheLookup gs =<< baseid
                                r <- Map.lookup did $ e ^. editDiscussions'
                                pure $ Right (r, d))
                            (gs ^. gsRawContent)
                            (StatementPropDetails ed ((^. userName) <$> (gs ^. gsServerCache . scUsers)))
                            (case gs ^. gsHeaderState of Just hs -> hs ^. hsDiscussionFlatView)
      )
      dst
  where
    Just dst = gs ^. gsDocumentState
    eid = case dst of
      DocumentStateDiff _ _ i _ _ -> i
      _ -> error "getDocumentStateProps: impossible"
getDocumentStateProps _ _
  = error "getDocumentStateProps: gsEditID came up empty!"

wipeDocumentState :: AccessState -> GlobalState -> GlobalState_ WipedDocumentState
wipeDocumentState as gs = const getWipedDocumentState <$> gs
  where
    getWipedDocumentState :: WipedDocumentState
    getWipedDocumentState = case getDocumentStateProps as gs of
      DocumentStateView{}                  -> WipedDocumentStateView
      DocumentStateDiff i _ edit collapsed editable -> WipedDocumentStateDiff i edit collapsed editable
      DocumentStateEdit _ meid             -> WipedDocumentStateEdit $ mkEditToolbarProps (cacheLookup gs =<< meid)
      DocumentStateDiscussion dp           -> WipedDocumentStateDiscussion $ DiscussionToolbarProps
                                                (either (const Nothing) (Just . (^. discussionID)) disc)
                                                (dp ^. discPropsFlatView)
                                                (either (error "wipeDocumentState: impossible") (^. discussionIsNote) disc)
                                                (votesToCount $ either (error "wipeDocumentState: impossible") (^. discussionVotes) disc)
        where disc = id +++ snd $ dp ^. discPropsDiscussion

-- (this is not a good place for this instance, but it avoids import cycles, and the class is
-- deprecated anyway.  when refactoring Icon.hs, we need to think about how to do this better, see
-- #439.)
instance IbuttonOnClick [EditorStoreAction] 'EventHandlerCode where
  runIbuttonOnClick _ _ = mconcat . fmap dispatch

editToolbar :: View' '[EditToolbarProps]
editToolbar = mkView' "editToolbar" $ \ep -> do
  let props icon acts = emptyIbuttonProps icon acts & ibSize .~ XXLarge

  div_ ["className" $= "main-content__header"] $ do
    div_ ["className" $= "main-content__header-inner fisx-css-toolbar-flex c-vdoc-toolbar"] $ do
      ibutton_ $ props (ButtonImageIcon Svg.Close ColorSchemaEdit)
        [DocumentAction DocumentCancelSave, DocumentAction UpdateDocumentStateView]
        & ibListKey .~ "1"

      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarH1 ColorSchemaDark) [DocumentToggleBlockType Header1]
        & ibListKey .~ "2"
      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarH2 ColorSchemaDark) [DocumentToggleBlockType Header2]
        & ibListKey .~ "3"
      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarH3 ColorSchemaDark) [DocumentToggleBlockType Header3]
        & ibListKey .~ "4"

      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarBold ColorSchemaDark) [DocumentToggleStyle Bold]
        & ibListKey .~ "5"
      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarItalic ColorSchemaDark) [DocumentToggleStyle Italic]
        & ibListKey .~ "6"

      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarBullets ColorSchemaDark) [DocumentToggleBlockType BulletPoint]
        & ibListKey .~ "7"
      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarNumbers ColorSchemaDark) [DocumentToggleBlockType EnumPoint]
        & ibListKey .~ "8"

    {- FIXME: #452
      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      let props :: IbuttonProps [GlobalAction]
          props = props "Edit_toolbar_link" onclick
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

      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarUndo ColorSchemaDark) [DocumentUndo]
        & ibListKey .~ "9"
      ibutton_ $ props (ButtonImageIcon Svg.EditToolbarRedo ColorSchemaDark) [DocumentRedo]
        & ibListKey .~ "10"

      div_ ["className" $= "c-vdoc-toolbar__separator"] ""

      ibutton_ $ props (ButtonImageIcon Svg.Save ColorSchemaDark) [DocumentRequestSave $ ep ^. editToolbarPropsInitial]
        & ibListKey .~ "11"

editToolbar_ :: HasCallStack => EditToolbarProps -> ReactElementM eventHandler ()
editToolbar_ = view_' editToolbar "editToolbar_"
