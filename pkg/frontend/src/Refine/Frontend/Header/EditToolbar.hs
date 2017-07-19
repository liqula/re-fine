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
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.EditToolbar where

import Refine.Frontend.Prelude

import           Refine.Common.Types
import           Refine.Common.VDoc.OT (docRanges)
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Icon
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Header.Types


mkEditToolbarProps :: HasCallStack => EditorState -> EditToolbarProps
mkEditToolbarProps es
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

wipeDocumentState :: DocumentState -> WipedDocumentState
wipeDocumentState = \case
  DocumentStateView{}                  -> WipedDocumentStateView
  DocumentStateDiff i _ _ edit collapsed -> WipedDocumentStateDiff i edit collapsed
  DocumentStateEdit es _ _             -> WipedDocumentStateEdit $ mkEditToolbarProps es

editToolbar_ :: HasCallStack => EditToolbarProps -> ReactElementM eventHandler ()
editToolbar_ ep = do
  let editButton icon = defaultIconButtonProps @[GlobalAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-" <> icon, "dark") XXLarge
        & iconButtonPropsElementName  .~ "btn-index"

  let props :: IbuttonProps [GlobalAction]
      props = emptyIbuttonProps "Close" [DocumentAction DocumentCancelSave]
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
        & ibLabel        .~ case ep of
            LinkButtonDisabled -> "links"
            LinkButtonDeletes  -> "delete link"
            LinkButtonAdds _   -> "add link"
        & ibEnabled      .~ case ep of
            LinkButtonDisabled -> False
            _                  -> True
        & ibSize         .~ XXLarge

      onclick = case ep of
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
      props = emptyIbuttonProps "Save" [DocumentAction RequestDocumentSave]
        & ibListKey      .~ "save"
        & ibLabel        .~ "save"
        & ibEnabled      .~ True
        & ibSize         .~ XXLarge
   in ibutton_ props
