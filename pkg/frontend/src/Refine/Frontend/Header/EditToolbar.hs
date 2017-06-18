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

data EditToolbarProps
    = LinkButtonDisabled
    | LinkButtonDeletes
    | LinkButtonAdds ST

mkEditToolbarProps :: GlobalState -> EditToolbarProps
mkEditToolbarProps st
    | rangeIsEmpty rc sel = LinkButtonDisabled
    | any (doRangesOverlap sel) linkranges = LinkButtonDeletes
    | otherwise = LinkButtonAdds . cs $ rangeText BlockBoundaryIsEmpty rc sel
  where
    es = st ^. gsDocumentState . documentStateVal
    sel = _selectionRange . fromSelectionState rc $ getSelection es
    rc = convertToRaw $ getCurrentContent es
    linkranges = fmap (fromStyleRange rc) . unRanges . mconcat . fmap (toStyleRanges rc . snd)
        $ docRanges False lineElemLength (\((Atom l, _), _) -> [() | isLink l]) rc
      where
        isLink (Just (EntityLink _)) = True
        isLink _ = False


editToolbar :: EditToolbarProps -> View '[]
editToolbar ep = mkView "EditToolbar" $ do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          let editButton icon = defaultIconButtonProps @[GlobalAction]
                & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-" <> icon, "dark") XXLarge
                & iconButtonPropsElementName  .~ "btn-index"

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

          iconButton_ $ editButton mempty
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXLarge
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsOnClick      .~ [DocumentAction RequestDocumentSave]

editToolbar_ :: EditToolbarProps -> ReactElementM eventHandler ()
editToolbar_ ep = view_ (editToolbar ep) "editToolbar_"
