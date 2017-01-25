{-# LANGUAGE OverloadedStrings #-}

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
          iconButton_ $ IconButtonProps "c-vdoc-toolbar"
                      "btn-index"
                      ""
                      True
                      ("icon-Index_desktop", "dark")
                      "index"
                      XXL

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          iconButton_ $ IconButtonProps "c-vdoc-toolbar"
                      "btn-add-annotation"
                      "annotation"
                      True
                      ("icon-New_Comment", "dark")
                      "new annotation"
                      XXL

          iconButton_ $ IconButtonProps "c-vdoc-toolbar"
                      "btn-add-modification"
                      "modification"
                      True
                      ("icon-New_Edit", "dark")
                      "new modification"
                      XXL

          div_ ["className" $= "c-vdoc-toolbar__separator"] ""

          -- in HTML, these two icons are divs:
          iconButton_ $ IconButtonProps "c-vdoc-toolbar"
                      "all-annotations"
                      ""
                      False
                      ("icon-Comment", "dark")
                      "all annotations"
                      XXL

          iconButton_ $ IconButtonProps "c-vdoc-toolbar"
                      "all-modifications"
                      ""
                      False
                      ("icon-Edit", "dark")
                      "all modifications"
                      XXL

          iconButtonWithAlignment_ $ IconButtonWithAlignmentProps (IconButtonProps "c-vdoc-toolbar"
                                    "btn-read-mode"
                                    "" -- data-content-type is not set for this one...
                                    True
                                    ("icon-Reader", "bright")
                                    "read mode"
                                    XXL)
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
        div_ ["className" $= "c-vdoc-toolbar-extension__annotation"] $ do
            iconButton_ $ IconButtonProps "c-vdoc-toolbar-extension"
                        "btn-new-ann-doc"
                        "annotation"
                        True
                        ("icon-Index_desktop", "dark")  -- why index and not comment?
                        "document annotation"
                        L
            iconButton_ $ IconButtonProps "c-vdoc-toolbar-extension"
                        "btn-new-ann-text"
                        "annotation"
                        True
                        ("icon-Comment", "dark")
                        "annotation related to text"
                        L

        div_ ["className" $= "c-vdoc-toolbar-extension__modification"] $ do
            iconButton_ $ IconButtonProps "c-vdoc-toolbar-extension"
                        "btn-new-mod-text"
                        "annotation"
                        True
                        ("icon-New_Edit", "dark")
                        "new modification"
                        L


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
