{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.Dialog
  ( commentInput_
  , addComment_
  , addEdit_
  , contributionDialogTextForm
  ) where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Document.FFI.Types
import           Refine.Frontend.Icon
import qualified Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store ()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Console (gracefulError)
import           Refine.Frontend.ThirdPartyViews (skylight_)
import           Refine.Frontend.TKey
import           Refine.Frontend.Types
import           Refine.Frontend.Util


-- * styles

dialogWidth :: HasCallStack => Int
dialogWidth = 640

leftFor :: HasCallStack => Int -> Int
leftFor windowWidth = (windowWidth - dialogWidth) `quot` 2

dialogStyles :: HasCallStack => [Decl]
dialogStyles = [ -- Style "display" ("block" :: String)
                 -- Style "minHeight" ("200px" :: String)
                 -- Style "padding" ("3rem 1.0rem 1.0rem" :: String)

                  decl "width" (Px dialogWidth)
                , decl "marginLeft" (Px 0)
                , decl "marginTop" (Px 0)
                , zindex ZIxDialog

                , decl "position" (Ident "absolute")
                ]

addCommentDialogStyles :: HasCallStack => [Decl]
addCommentDialogStyles = [decl "backgroundColor" (Ident "rgba(219, 204, 221, 1)")] <> dialogStyles

overlayStyles :: HasCallStack => [Decl]
overlayStyles =
  [ zindex ZIxOverlay
  , decl "backgroundColor" (Ident "rgba(255, 255, 255, 0.8)")
  ]


-- * elements / components used for all contribution kinds

addContributionDialogFrame
    :: ST -> Maybe SelectionStateWithPx -> Int
    -> ReactElementM 'EventHandlerCode () -> ReactElementM 'EventHandlerCode ()
addContributionDialogFrame title mrange windowWidth child =
    let top = case mrange of
              Nothing -> 30
              Just range -> (range ^. sstBottomOffset . unOffsetFromViewportTop)
                          + (range ^. sstScrollOffset . unScrollOffsetOfViewport)
        extraStyles = [ decl "top" (Px $ top + 5)
                      , decl "left" (Px $ leftFor windowWidth)
                      , decl "height" (Px 560)
                      ]
    in skylight_ ["isVisible" &= True
             , React.on "onCloseClicked"   $ \_ -> simpleHandler $ dispatch (ContributionAction HideCommentEditor)
             , React.on "onOverlayClicked" $ \_ -> simpleHandler $ dispatch (ContributionAction HideCommentEditor)
             , "dialogStyles" @@= (addCommentDialogStyles <> extraStyles)
             , "overlayStyles" @@= overlayStyles
             , "titleStyle" @@= [decl "margin" (Px 0)]
             ]  $ do

      span_ [ "className" $= "c-vdoc-overlay-content__title"
            , "style" @@=
                    [ decl "fontSize" (Rem 1.125)
                    , decl "lineHeight" (Mm 1.15)
                    , decl "marginBottom" (Rem 0.875)
                    , decl "marginLeft" (Rem 1)
                    , decl "fontWeight" (Ident "bold")
                    ]
            ] (elemText title)

      hr_ []

      child

contributionDialogTextForm :: HasCallStack => Lens' st ST -> st -> Int -> ST -> ReactElementM ('StatefulEventHandlerCode st) ()
contributionDialogTextForm stateLens st' stepNumber promptText = do
  div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
    p_ $ do
      elemString $ "Step " <> show stepNumber <> ": "
      span_ ["className" $= "bold"] $ do
        elemText promptText

  contributionDialogTextFormInner 600 240 stateLens st'

contributionDialogTextFormInner :: HasCallStack => Int -> Int -> Lens' st ST -> st -> ReactElementM ('StatefulEventHandlerCode st) ()
contributionDialogTextFormInner width height stateLens st' = do
  form_ [ "target" $= "#"
        , "action" $= "POST"] $ do
    textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"  -- RENAME: annotation => comment
              , "className" $= "o-form-input__textarea"
              , "style" @@=
                      [ decl "resize" (Ident "none")
                      , decl "width" (Px width)
                      , decl "height" (Px height)
                      ]
              -- Update the current state with the current text in the textbox, sending no actions
              , onChange $ \evt -> simpleHandler $ \st -> ([], Just $ st & stateLens .~ target evt "value")
              , "value" $= cs (st' ^. stateLens)
              ]
      mempty


-- * comments

addComment :: HasCallStack => Translations -> View '[AddContributionProps (LocalStateRef CommentInputState)]
addComment __ = mkView "AddComment" $ \props -> addContributionDialogFrame
  (__ add_a_comment)
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  (commentInput_ $ props ^. acpLocalState)

addComment_ :: HasCallStack => Translations -> AddContributionProps (LocalStateRef CommentInputState) -> ReactElementM eventHandler ()
addComment_ __ = view_ (addComment __) "addComment_"


commentInput :: HasCallStack => LocalStateRef CommentInputState -> View '[]
commentInput lst = mkPersistentStatefulView "CommentInput" lst Nothing $ \st ->
  do
    let smkind = st ^? commentInputStateData . commentInfoKind . _Just
    let stext  = st ^. commentInputStateData . commentInfoDesc

    div_ $ do
      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 1: "
          span_ ["className" $= "bold"] "Select a type for your comment:"

      div_ ["className" $= "c-vdoc-overlay-content__annotation-type"] $ do  -- RENAME: annotation => comment
        let props :: CommentKind -> IbuttonProps CommentKind
            props ckind = emptyIbuttonProps (ButtonImageIcon (img ckind) ColorSchemaBright) ckind
              & ibListKey .~ lk ckind
              where
                lk CommentKindNote       = "note"
                lk CommentKindDiscussion = "discussion"

                img CommentKindNote       = Svg.Note
                img CommentKindDiscussion = Svg.Discussion

        sibutton_ commentInputStateNoteButton       st $ props CommentKindNote
        sibutton_ commentInputStateDiscussionButton st $ props CommentKindDiscussion

      hr_ []

      contributionDialogTextForm (commentInputStateData . commentInfoDesc) st 2 "enter your comment:"

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 3: "
          span_ ["className" $= "bold"] "finish"

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Save ColorSchemaBright)
                    [ ContributionAction $ SubmitComment (CommentInfo stext (fromJust smkind))
                    , ContributionAction ClearRange
                    , ContributionAction HideCommentEditor
                    ]
        & ibEnabled .~ not (ST.null stext || isNothing smkind)

commentInput_ :: HasCallStack => LocalStateRef CommentInputState -> ReactElementM eventHandler ()
commentInput_ lst = view_ (commentInput lst) "commentInput_"


-- * edits

-- FIXME: is it a good thing that the local state is lost whenever the
-- props change?  (something about people trying to compute the
-- initial state from props, perhaps?)
-- to put it in another way: often it would be nice if we could change
-- the props value via a global action and keep going with the local
-- state.

addEdit :: HasCallStack => View '[AddContributionProps (EditInfo (Maybe EditKind), EditorState)]
addEdit = mkView "AddEdit" $ \props -> addContributionDialogFrame
  "add an edit"
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  (editInput_ (props ^. acpLocalState))

addEdit_ :: HasCallStack => AddContributionProps (EditInfo (Maybe EditKind), EditorState) -> ReactElementM eventHandler ()
addEdit_ = view_ addEdit "addEdit_"


-- | FUTUREWORK: there is *some* code sharing between 'editInput_' and 'commentInput_', but there may be
-- room for more.
--
-- FUTUREWORK: kind change is a nice example of local signals between two components.  how is this
-- handled in react?  should we have a second global store here that is just shared between
-- 'editInput' and and 'editKindForm'?
editInput :: HasCallStack => (EditInfo (Maybe EditKind), EditorState) -> View '[]
editInput (einfo, estate) = mkPersistentStatefulView "EditInput" (einfo ^. editInfoLocalStateRef) Nothing $
  \st@(EditInputState (EditInfo desc mkind rst) _) -> do
    div_ $ do
      elemString "Step 1: "
      span_ ["className" $= "bold"] "Type of this edit:"
      div_ $ editKindForm_ st

    hr_ []

    contributionDialogTextForm (editInputStateData . editInfoDesc) st 2 "describe your motivation for this edit:"

    hr_ []

    ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Save ColorSchemaBright)
      [ DocumentAction . DocumentSave $ FormComplete (EditInfo desc (fromJust mkind) rst, estate)
      , DocumentAction UpdateDocumentStateView
      , ContributionAction ClearRange
      ]
      & ibEnabled .~ not (ST.null desc || isNothing mkind)
      & ibSize .~ XXLarge

editInput_ :: HasCallStack => (EditInfo (Maybe EditKind), EditorState) -> ReactElementM eventHandler ()
editInput_ st = view_ (editInput st) "editInput_"


-- | (FUTUREWORK: somewhere in here, there is a radio button widget
-- that works with any Bounded/Enum type...)
editKindForm_ :: HasCallStack => EditInputState -> ReactElementM ('StatefulEventHandlerCode EditInputState) ()
editKindForm_ st = do
  div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] $ do
          pure ()
        div_ ["className" $= "c-vdoc-toolbar-extension__modification c-vdoc-toolbar-extension--expanded"] $ do  -- (RENAME: Edit)
          forM_ [Grammar, Phrasing, Meaning] $ \ekind -> do
            sibutton_ (mouseIsOver ekind) st (props ekind)
            div_ ["className" $= "c-vdoc-toolbar__separator"] ""
  where
    mouseIsOver :: EditKind -> Lens' EditInputState ButtonState
    mouseIsOver ekind f (wide :: EditInputState) = outof <$> f into
      where
        outof :: ButtonState -> EditInputState
        outof bs = wide & editInputStateButtons %~ (cleanupEditInputStateButtons . ((ekind, bs):))

        into :: ButtonState
        into = fromMaybe def . lookup ekind $ wide ^. editInputStateButtons

    props :: EditKind -> IbuttonProps EditKind
    props ekind = emptyIbuttonProps (ButtonImageIcon Svg.EditNew Svg.ColorSchemaBright) ekind
      & ibListKey .~ cs (toLower <$> show ekind)
