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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.Toolbar where

import           React.Flux
import           Data.String.Conversions

import           Refine.Common.Types ( EditKind(..) )
import qualified Refine.Frontend.Header.Types as RS
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets

toolbar :: View '[()]
toolbar = mkView "Toolbar" $ \() ->
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-Index_desktop", "dark") XXL)
                      "btn-index"
                      ""
                      ""
                      "index"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-New_Comment", "dark") XXL)
                      "btn-add-annotation"  -- RENAME: annotation => comment
                      ""
                      "comment"
                      "new comment"
                      False
                      (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction RS.ToggleCommentToolbarExtension))
                      []

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-New_Edit", "dark") XXL)
                      "btn-add-modification"  -- (RENAME: Edit)
                      ""
                      "edit"
                      "new edit"
                      False
                      (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction RS.ToggleEditToolbarExtension))
                      []

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          -- in HTML, these two icons are divs:
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" False ("icon-Comment", "dark") XXL)
                      "all-annotations"  -- RENAME: annotation => comment
                      ""
                      ""
                      "all comments"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []

          iconButton_ $ IconButtonProps
                      -- (IconProps "c-vdoc-toolbar" False ("icon-Edit", "dark") XXL)
                      (IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXL)
                      "all-modifications"  -- (RENAME: Edit)
                      ""
                      ""
                      "all edits"
                      False
                      (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                      []

          iconButtonWithAlignment_ $
            IconButtonWithAlignmentProps
              (IconButtonProps
                (IconProps "c-vdoc-toolbar" True ("icon-Reader", "bright") XXL)
                "btn-read-mode"
                ""
                ""
                "read mode"
                False
                (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                [])
              True
              Nothing

toolbar_ :: ReactElementM eventHandler ()
toolbar_ = view_ toolbar "toolbar_" ()


newtype CommentToolbarExtensionProps = CommentToolbarExtensionProps
  { _ctepStatus :: RS.ToolbarExtensionStatus
  }

commentToolbarExtension :: View '[CommentToolbarExtensionProps]
commentToolbarExtension = mkView "CommentToolbarExtension" $ \case
  (CommentToolbarExtensionProps RS.CommentToolbarExtensionWithSelection) -> frame $ do
    div_ "Please select the text you would like to comment on"
  (CommentToolbarExtensionProps RS.CommentToolbarExtensionWithButtons) -> frame $ do
    iconButton_ $ IconButtonProps
                (IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L)
                "btn-new-ann-text"
                ""
                "comment"
                "text-specific comment"
                False
                (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction RS.StartTextSpecificComment))
                []
    iconButton_ $ IconButtonProps
                (IconProps "c-vdoc-toolbar-extension" True ("icon-Index_desktop", "dark") L)
                "btn-new-ann-doc" -- RENAME: ann => comment
                ""
                "comment"
                "general comment"
                False
                (\_ -> RS.dispatch RS.ShowNotImplementedYet)
                []
  (CommentToolbarExtensionProps _) -> mempty
  where
    frame :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
    frame children = div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ [classNames [ ("c-vdoc-toolbar-extension__annotation", True)   -- RENAME: annotation => comment
                           , ("c-vdoc-toolbar-extension--expanded", True) ]
               ]
            children


commentToolbarExtension_ :: CommentToolbarExtensionProps -> ReactElementM eventHandler ()
commentToolbarExtension_ !props = view_ commentToolbarExtension "commentToolbarExtension_" props


newtype EditToolbarExtensionProps = EditToolbarExtensionProps
  { _etepStatus :: RS.ToolbarExtensionStatus
  }

editToolbarExtension :: View '[EditToolbarExtensionProps]
editToolbarExtension = mkView "EditToolbarExtension" $ \case
  (EditToolbarExtensionProps RS.EditToolbarExtension) -> do
    div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ ["className" $= "c-vdoc-toolbar-extension__modification c-vdoc-toolbar-extension--expanded"] $ do  -- (RENAME: Edit)
            editButton Phrasing
            editButton Meaning
            editButton Grammar

  (EditToolbarExtensionProps _) -> mempty
  where
    editButton :: EditKind -> ReactElementM eventHandler ()
    editButton kind =
      iconButton_ $ IconButtonProps
                  (IconProps "c-vdoc-toolbar-extension" True ("icon-New_Edit", "dark") L)
                  "btn-new-mod-text" -- RENAME: mod => edit
                  ""
                  "edit"
                  (cs (show kind))
                  False
                  (\e -> stopPropagation e : RS.dispatch (RS.HeaderAction (RS.StartEdit kind)))
                  []


editToolbarExtension_ :: EditToolbarExtensionProps -> ReactElementM eventHandler ()
editToolbarExtension_ !props = view_ editToolbarExtension "editToolbarExtension_" props
