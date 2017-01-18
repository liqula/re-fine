{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Heading where

import           Control.Concurrent (forkIO)
import           Control.Monad (forM_)
import           GHCJS.Types (JSVal)
import           Data.String.Conversions
import           React.Flux
import           React.Flux.Lifecycle

import           Refine.Common.Types

import           Refine.Frontend.UtilityWidgets
import qualified Refine.Frontend.RefineStore as RS
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

newtype DocumentHeaderProps = DocumentHeaderProps
  { _headerTitle :: Title
  }

documentHeader :: ReactView DocumentHeaderProps
documentHeader = defineView "DocumentHeader" $ \props ->
  div_ ["className" $= "row row-align-middle c-vdoc-header"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
          div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do

              h1_ . elemText . cs . _unTitle $ _headerTitle props
              div_ ["className" $= "c-vdoc-header__description"] $ do
                  p_ "Auch gibt es niemanden, der den Schmerz an sich liebt, sucht oder wünscht, nur, weil er Schmerz ist, es sei denn, es kommt zu zufälligen Umständen, in denen Mühen und Schmerz ihm große Freude bereiten können. Um ein triviales Beispiel zu nehmen, wer von uns unterzieht sich je anstrengender körperlicher Betätigung, außer um Vorteile daraus zu ziehen? Aber wer hat irgend ein Recht, einen Menschen zu tadeln, der die Entscheidung trifft, eine Freude zu genießen, die keine unangenehmen Folgen hat, oder einen, der Schmerz vermeidet, welcher keine daraus resultierende Freude nach sich zieht!"
                  p_ "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie in Buchstabhausen an der Küste des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen. Nicht einmal von der allmächtigen Interpunktion werden die Blindtexte beherrscht – ein geradezu unorthographisches Leben. Eines Tages aber beschloß eine kleine Zeile Blindtext, ihr Name war Lorem Ipsum, hinaus zu gehen in die weite Grammatik. Der große Oxmox riet ihr davon ab, da es dort wimmele von bösen Kommata, wilden Fragezeichen und hinterhältigen Semikoli, doch das Blindtextchen ließ sich nicht beirren. Es packte seine sieben Versalien, schob sich sein Initial in den Gürtel und machte sich auf den Weg."

              phases_

documentHeader_ :: DocumentHeaderProps -> ReactElementM eventHandler ()
documentHeader_ props = view documentHeader props mempty


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
