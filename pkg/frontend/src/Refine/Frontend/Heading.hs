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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Heading where

import           Control.Concurrent (forkIO)
import           Control.Monad (forM_)
import           GHCJS.Types (JSVal)
import           Data.String.Conversions
import           Data.Text (split)
import           React.Flux
import           React.Flux.Lifecycle

import           Refine.Common.Types

import           Refine.Frontend.UtilityWidgets
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


menuButton :: ReactView ()
menuButton = defineView "MenuButton" $ \() ->
  span_ ["className" $= "c-mainmenu"] $ do
    button_ ["aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
    span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"

menuButton_ :: ReactElementM eventHandler ()
menuButton_ = view menuButton () mempty

data DocumentHeaderProps = DocumentHeaderProps
  { _headerTitle :: Title
  , _headerAbstract :: Abstract
  }

documentHeader :: ReactView DocumentHeaderProps
documentHeader = defineView "DocumentHeader" $ \props ->
  div_ ["className" $= "row row-align-middle c-vdoc-header"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
          div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
              documentTitle_ $ _headerTitle props
              documentAbstract_ $ _headerAbstract props
              phases_

documentHeader_ :: DocumentHeaderProps -> ReactElementM eventHandler ()
documentHeader_ props = view documentHeader props mempty

documentTitle :: ReactView Title
documentTitle = defineView "DocumentTitle" $ \title ->
  h1_ . elemText . cs $ _unTitle title

documentTitle_ :: Title -> ReactElementM eventHandler ()
documentTitle_ title = view documentTitle title mempty

documentAbstract :: ReactView Abstract
documentAbstract = defineView "DocumentAbstract" $ \abstract ->
  div_ ["className" $= "c-vdoc-header__description"] $ do
    let paragraphs = split (== '\n') . cs $ _unAbstract abstract
    div_ ["className" $= "c-vdoc-header__description"] . mconcat $ (p_ . elemText) <$> paragraphs

documentAbstract_ :: Abstract -> ReactElementM eventHandler ()
documentAbstract_ abstract = view documentAbstract abstract mempty


phases :: ReactView ()
phases = defineView "Phases" $ \() ->
  div_ ["className" $= "c-vdoc-header__phases"] $ do
    h5_ "Phases"
    div_ ["className" $= "c-vdoc-header__phase c-vdoc-header__phase--active"] "Text Collaboration"
    div_ ["className" $= "c-vdoc-header__phase"] "Vote"
    div_ ["className" $= "c-vdoc-header__phase"] "Result"


phases_ :: ReactElementM eventHandler ()
phases_ = view phases () mempty

editToolbar :: ReactView ()
editToolbar = defineView "EditToolbar" $ \() ->
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
                      (\_ -> [])

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-New_Comment", "dark") XXL)
                      "btn-add-annotation"  -- RENAME: annotation => comment
                      ""
                      "annotation"  -- RENAME: annotation => comment
                      "new annotation"  -- RENAME: annotation => comment
                      False
                      (\_ -> [])

          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" True ("icon-New_Edit", "dark") XXL)
                      "btn-add-modification"  -- (RENAME: Edit)
                      ""
                      "modification"  -- (RENAME: Edit)
                      "new modification"  -- (RENAME: Edit)
                      False
                      (\_ -> [])

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          -- in HTML, these two icons are divs:
          iconButton_ $ IconButtonProps
                      (IconProps "c-vdoc-toolbar" False ("icon-Comment", "dark") XXL)
                      "all-annotations"  -- RENAME: annotation => comment
                      ""
                      ""
                      "all annotations"  -- RENAME: annotation => comment
                      False
                      (\_ -> [])

          iconButton_ $ IconButtonProps
                      -- (IconProps "c-vdoc-toolbar" False ("icon-Edit", "dark") XXL)
                      (IconProps "c-vdoc-toolbar" True ("icon-Edit_view", "dark") XXL)
                      "all-modifications"  -- (RENAME: Edit)
                      ""
                      ""
                      "all modifications"  -- (RENAME: Edit)
                      False
                      (\_ -> [])

          iconButtonWithAlignment_ $ IconButtonWithAlignmentProps (IconButtonProps
                                    (IconProps "c-vdoc-toolbar" True ("icon-Reader", "bright") XXL)
                                    "btn-read-mode"
                                    ""
                                    "" -- data-content-type is not set for this one...
                                    "read mode"
                                    False
                                    (\_ -> []))
                                    True
                                    Nothing

editToolbar_ :: ReactElementM eventHandler ()
editToolbar_ = view editToolbar () mempty

editToolbarExtension :: ReactView ()
editToolbarExtension = defineView "EditToolbarExtension" $ \() ->
  div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
        div_ ["className" $= "c-vdoc-toolbar-extension__annotation"] $ do  -- RENAME: annotation => comment
            iconButton_ $ IconButtonProps
                        (IconProps "c-vdoc-toolbar-extension" True ("icon-Index_desktop", "dark") L) -- TODO why index and not comment?
                        "btn-new-ann-doc"
                        ""
                        "annotation"  -- RENAME: annotation => comment
                        "document annotation"  -- RENAME: annotation => comment
                        False
                        (\_ -> [])
            iconButton_ $ IconButtonProps
                        (IconProps "c-vdoc-toolbar-extension" True ("icon-Comment", "dark") L)
                        "btn-new-ann-text"
                        ""
                        "annotation"  -- RENAME: annotation => comment
                        "annotation related to text"  -- RENAME: annotation => comment
                        False
                        (\_ -> [])

        div_ ["className" $= "c-vdoc-toolbar-extension__modification"] $ do  -- (RENAME: Edit)
            iconButton_ $ IconButtonProps
                        (IconProps "c-vdoc-toolbar-extension" True ("icon-New_Edit", "dark") L)
                        "btn-new-mod-text"
                        ""
                        "annotation"  -- RENAME: annotation => comment
                        "new modification"  -- (RENAME: Edit)
                        False
                        (\_ -> [])


editToolbarExtension_ :: ReactElementM eventHandler ()
editToolbarExtension_ = view editToolbarExtension () mempty

headerSizeCapture :: ReactView ()
headerSizeCapture = defineLifecycleView "HeaderSizeCapture" () lifecycleConfig
   { lRender = \_state _props ->
       div_ ["className" $= "c-fullheader"] childrenPassedToView
   , lComponentDidMount = Just $ \_propsandstate ldom _ -> do
             this <- lThis ldom
             height <- js_getBoundingClientRectHeight this
             _ <- forkIO $ do
                 let actions = RS.dispatch $ RS.AddHeaderHeight height
                 forM_ actions executeAction
             return ()
   }

headerSizeCapture_ :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
headerSizeCapture_ = view headerSizeCapture ()

foreign import javascript unsafe
  "$1.getBoundingClientRect().height"
  js_getBoundingClientRectHeight :: JSVal -> IO Int
