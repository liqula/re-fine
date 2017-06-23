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
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.Dialog where

import Refine.Frontend.Prelude

import qualified Data.Map.Strict as M
import qualified Data.Text as ST
import qualified Data.Tree as Tree
import           Language.Css.Syntax
import qualified React.Flux as RF

import           Refine.Common.Types hiding (Style)
import           Refine.Frontend.Test.Console (gracefulError)
import           Refine.Frontend.ThirdPartyViews (skylight_)
import           Refine.Frontend.Contribution.Types
import qualified Refine.Frontend.Colors as C
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.TKey
import           Refine.Frontend.Types
import           Refine.Frontend.Util

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


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

-- RENAME: addCommentDialogStyles
vdoc_overlay_content__add_comment :: HasCallStack => [Decl]
vdoc_overlay_content__add_comment = [decl "backgroundColor" C.VDocComment] <> dialogStyles

-- is vdoc_overlay_content__comment in CSS

-- RENAME: showNoteDialogStyles
vdoc_overlay_content__note :: HasCallStack => [Decl]
vdoc_overlay_content__note = [decl "backgroundColor" C.VDocNote] <> dialogStyles

-- RENAME: showDiscussionDialogStyles
vdoc_overlay_content__discussion :: HasCallStack => [Decl]
vdoc_overlay_content__discussion = [decl "backgroundColor" C.VDocDiscussion] <> dialogStyles

overlayStyles :: HasCallStack => [Decl]
overlayStyles =
  [ zindex ZIxOverlay
  , decl "backgroundColor" C.OverlayBackdrop
  ]


-- * comments

showComment :: HasCallStack => View '[CommentDisplayProps]
showComment = mkView "ShowComment" $ \props ->
  let extraStyles = [ decl "top" (Px $ props ^. cdpTopOffset . unOffsetFromDocumentTop + 5)
                    , decl "left" (Px . leftFor $ props ^. cdpWindowWidth)
                    , decl "height" (Px 0)
                    , decl "minHeight" (Px 100)
                    ]
  in skylight_ ["isVisible" &= True
           , RF.on "onCloseClicked"   $ \_ -> dispatch (ContributionAction HideContributionDialog)
           , RF.on "onOverlayClicked" $ \_ -> dispatch (ContributionAction HideContributionDialog)
           , "dialogStyles" @@= ((props ^. cdpContentStyle) <> extraStyles)
           , "overlayStyles" @@= overlayStyles
           , "closeButtonStyle" @@= [decl "top" (Px 0), decl "bottom" (Px 0)]
           , "titleStyle" @@= [decl "margin" (Px 0)]
           ] $ do
    -- div_ ["className" $= "c-vdoc-overlay-content c-vdoc-overlay-content--comment"] $ do

        div_ ["style" @@= [decl "marginLeft" (Percentage 96)]] $ do             -- FIXME: How to do this properly?
          icon_ (IconProps "c-vdoc-overlay-content" False (props ^. cdpIconStyle) XLarge)

        div_ ["className" $= "c-vdoc-overlay-content__copy"] $ elemText (props ^. cdpCommentText)

        -- edit/comment user meta data -->
        div_ ["className" $= "c-vdoc-overlay-meta"] $ do
            span_ ["className" $= "c-vdoc-overlay-meta__user-avatar"] $ do
                icon_ (IconProps "c-vdoc-overlay-meta" False ("icon-User", "bright") Medium)
            span_ ["className" $= "c-vdoc-overlay-meta__user"] $ elemCS (props ^. cdpUserName)
            span_ ["className" $= "c-vdoc-overlay-meta__date"] $ elemCS (props ^. cdpCreationDate) -- or what is this?
        -- END: edit/comment user meta data -->

        -- vote buttons -->
        div_ ["className" $= "c-vdoc-overlay-votes"] $ do

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-up"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_positive", "dark") XLarge)

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-down"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_negative", "dark") XLarge)
        -- END: vote buttons -->

        div_ ["style" @@= [decl "marginBottom" (Px 20)]] "" -- make some space for the close button

showComment_ :: HasCallStack => CommentDisplayProps -> ReactElementM eventHandler ()
showComment_ !props = view_ showComment "showComment_" props


showNoteProps :: HasCallStack => M.Map (ID Note) Note -> GlobalState -> ShowNoteProps
showNoteProps notes rs = case (maybeNote, maybeOffset) of
  (Just note, Just offset) -> ShowNotePropsJust note offset (rs ^. gsScreenState . ssWindowWidth)
  (Just note, Nothing)     -> err "note" note "offset" ShowNotePropsNothing
  (Nothing,   Just offset) -> err "offset" offset "note" ShowNotePropsNothing
  _                        -> ShowNotePropsNothing
  where
    maybeContribID = rs ^. gsContributionState . csDisplayedContributionID
    maybeNoteID :: Maybe (ID Note) = getNoteID =<< maybeContribID
    maybeNote = (`M.lookup` notes) =<< maybeNoteID
    maybeOffset = do
      nid <- maybeNoteID
      rs ^? gsContributionState . csAllVertialSpanBounds . allVertialSpanBounds
          . at (ContribIDNote nid) . _Just . vertialSpanBoundsBottom

    err haveT haveV missT = gracefulError (unwords ["showNoteProps: we have a", haveT, show haveV, "but no", missT])


showNote :: HasCallStack => View '[ShowNoteProps]
showNote = mkView "ShowNote" $ \case
  ShowNotePropsNothing -> mempty
  ShowNotePropsJust note top windowWidth1 ->
    let commentText1  = (note ^. noteText)
        iconStyle1    = ("icon-Note", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         vdoc_overlay_content__note top windowWidth1)

showNote_ :: HasCallStack => ShowNoteProps -> ReactElementM eventHandler ()
showNote_ !props = view_ showNote "showNote_" props


showDiscussionProps :: HasCallStack => M.Map (ID Discussion) CompositeDiscussion -> GlobalState -> ShowDiscussionProps
showDiscussionProps discussions rs = case (maybeDiscussion, maybeOffset) of
  (Just discussion, Just offset) -> ShowDiscussionPropsJust discussion offset (rs ^. gsScreenState . ssWindowWidth)
  (Just discussion, Nothing)     -> err "discussion" discussion "offset" ShowDiscussionPropsNothing
  (Nothing,         Just offset) -> err "offset" offset "discussion" ShowDiscussionPropsNothing
  _                              -> ShowDiscussionPropsNothing
  where
    maybeContribID = rs ^. gsContributionState . csDisplayedContributionID
    maybeDiscussionID :: Maybe (ID Discussion) = getDiscussionID =<< maybeContribID
    maybeDiscussion = (`M.lookup` discussions) =<< maybeDiscussionID
    maybeOffset = do
      did <- maybeDiscussionID
      rs ^? gsContributionState . csAllVertialSpanBounds . allVertialSpanBounds
          . at (ContribIDDiscussion did) . _Just . vertialSpanBoundsBottom

    err haveT haveV missT = gracefulError (unwords ["showNoteProps: we have a", haveT, show haveV, "but no", missT])


showDiscussion :: HasCallStack => View '[ShowDiscussionProps]
showDiscussion = mkView "ShowDiscussion" $ \case
  ShowDiscussionPropsNothing -> mempty
  ShowDiscussionPropsJust discussion top windowWidth1 ->
    let commentText1  = (Tree.rootLabel (discussion ^. compositeDiscussionTree) ^. statementText)
        iconStyle1    = ("icon-Discussion", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         vdoc_overlay_content__discussion top windowWidth1)

showDiscussion_ :: HasCallStack => ShowDiscussionProps -> ReactElementM eventHandler ()
showDiscussion_ !props = view_ showDiscussion "showDiscussion_" props


showQuestion :: HasCallStack => View '[ShowQuestionProps]
showQuestion = mkView "ShowQuestion" $ \case
  ShowQuestionProps Nothing -> mempty
  ShowQuestionProps (Just question) ->
    let overlayStyle1 = [decl "backgroundColor" C.VDocQuestion]
        commentText1  = (question ^. compositeQuestion . questionText)
        iconStyle1    = ("icon-Question", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         overlayStyle1 (OffsetFromDocumentTop 0) 800)

showQuestion_ :: HasCallStack => ShowQuestionProps -> ReactElementM eventHandler ()
showQuestion_ !props = view_ showQuestion "showQuestion_" props


addContributionDialogFrame
    :: Bool -> ST -> Maybe SelectionStateWithPx -> Int
    -> ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
addContributionDialogFrame False _ _ _ _ = pure ()
addContributionDialogFrame True title mrange windowWidth child =
    let top = case mrange of
              Nothing -> 30
              Just range -> (range ^. sstBottomOffset . unOffsetFromViewportTop)
                          + (range ^. sstScrollOffset . unScrollOffsetOfViewport)
        extraStyles = [ decl "top" (Px $ top + 5)
                      , decl "left" (Px $ leftFor windowWidth)
                      , decl "height" (Px 560)
                      ]
    in skylight_ ["isVisible" &= True
             , RF.on "onCloseClicked"   $ \_ -> dispatch (ContributionAction HideCommentEditor)
             , RF.on "onOverlayClicked" $ \_ -> dispatch (ContributionAction HideCommentEditor)
             , "dialogStyles" @@= (vdoc_overlay_content__add_comment <> extraStyles)
             , "overlayStyles" @@= overlayStyles
             , "titleStyle" @@= [decl "margin" (Px 0)]
             ]  $ do

      icon_ (IconProps "c-vdoc-overlay-content" False ("icon-New_Comment", "dark") XLarge)

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

addComment :: HasCallStack => Translations -> View '[AddContributionProps ()]
addComment __ = mkView "AddComment" $ \props -> addContributionDialogFrame
  (props ^. acpVisible)
  (__ add_a_comment)
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  commentInput_

addComment_ :: HasCallStack => Translations -> AddContributionProps () -> ReactElementM eventHandler ()
addComment_ __ !props = view_ (addComment __) "addComment_" props


-- | TODO: move to its own section "contribution component fragments" or something
contributionDialogTextForm :: HasCallStack => Lens' st ST -> Int -> ST -> ReactElementM (StatefulViewEventHandler st) ()
contributionDialogTextForm stateLens stepNumber promptText = do
  div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
    p_ $ do
      elemString $ "Step " <> show stepNumber <> ": "
      span_ ["className" $= "bold"] $ do
        elemText promptText

  form_ [ "target" $= "#"
        , "action" $= "POST"] $ do
    textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"  -- RENAME: annotation => comment
              , "className" $= "o-form-input__textarea"
              , "style" @@=
                      [ decl "resize" (Ident "none")
                      , decl "width" (Px 600)
                      , decl "height" (Px 240)
                      ]
              -- Update the current state with the current text in the textbox, sending no actions
              , onChange $ \evt st -> ([], Just $ st & stateLens .~ target evt "value")
              ]
      mempty


commentInput :: HasCallStack => View '[]
commentInput = mkStatefulView "CommentInput" (CommentInputState (CommentInfo "" Nothing) False False) $ \st ->
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
            props ckind = emptyIbuttonProps (img ckind) ckind
              & ibListKey       .~ l ckind
              & ibHighlightWhen .~ highlightWhen ckind
              & ibLabel         .~ l ckind
              & ibEnabled       .~ True
              & ibSize          .~ Large
              where
                l CommentKindNote       = "note"
                l CommentKindDiscussion = "discussion"

                img CommentKindNote       = "Note"
                img CommentKindDiscussion = "Discussion"

                highlightWhen k
                  | smkind == Just k = HighlightAlways
                  | otherwise        = HighlightOnMouseOver

        sibutton_ commentInputStateMouseOverNote       st $ props CommentKindNote
        sibutton_ commentInputStateMouseOverDiscussion st $ props CommentKindDiscussion

      hr_ []

      contributionDialogTextForm (commentInputStateData . commentInfoDesc) 2 "enter your comment:"

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 3: "
          span_ ["className" $= "bold"] "finish"

      let notATextOrKind = ST.null stext || isNothing smkind
        in iconButton_ $ defaultIconButtonProps @[GlobalAction]
          & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" False ("icon-Share", "dark") Large
          & iconButtonPropsElementName  .~ "submit"
          & iconButtonPropsLabel        .~ "submit"
          & iconButtonPropsDisabled     .~ notATextOrKind
          & iconButtonPropsOnClick      .~
                [ ContributionAction $ SubmitComment (CommentInfo stext (fromJust smkind))
                , ContributionAction ClearRange
                , ContributionAction HideCommentEditor
                ]

commentInput_ :: HasCallStack => ReactElementM eventHandler ()
commentInput_ = view_ commentInput "commentInput_"


-- * edits

-- FIXME: add meta button that allows to open an close the edit
-- dialog.  for now, the description is local and the edit kind is
-- global.  we want the state to be completely local and then stashed
-- globally when the dialog is closed.

-- FIXME: is it a good thing that the local state is lost whenever the
-- props change?  (something about people trying to compute the
-- initial state from props, perhaps?)

addEdit :: HasCallStack => View '[AddContributionProps (Maybe EditKind)]
addEdit = mkView "AddEdit" $ \props -> addContributionDialogFrame
  (props ^. acpVisible)
  "add an edit"
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  (editInput_ (props ^. acpKind))

addEdit_ :: HasCallStack => AddContributionProps (Maybe EditKind) -> ReactElementM eventHandler ()
addEdit_ = view_ addEdit "addEdit_"


-- | FUTUREWORK: there is *some* code sharing between 'editInput_' and 'commentInput_', but there may be
-- room for more.
--
-- FUTUREWORK: kind change is a nice example of local signals between two components.  how is this
-- handled in react?  should we have a second global store here that is just shared between
-- 'editInput' and and 'editKindForm'?
editInput :: HasCallStack => View '[Maybe EditKind]
editInput = mkStatefulView "EditInput" "" $ \curState mkind -> do
    div_ $ do
      elemString "Step 1: "
      span_ ["className" $= "bold"] "Type of this edit:"
      liftViewToStateHandler $ editKindForm_ (DocumentAction . DocumentUpdateEditKind) (EditKindFormProps mkind)

    hr_ []

    contributionDialogTextForm id 2 "describe your motivation for this edit:"

    hr_ []

    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsOnClick      .~ [ DocumentAction $ DocumentSave curState
                                             , ContributionAction ClearRange
                                             ]

editInput_ :: HasCallStack => Maybe EditKind -> ReactElementM eventHandler ()
editInput_ = view_ editInput "editInput_"


newtype EditKindFormProps = EditKindFormProps (Maybe EditKind)
  deriving (Eq)

instance UnoverlapAllEq EditKindFormProps

-- | FIXME: this component should be moved closer to "Refine.Frontend.Contribution.Dialog".  (not
-- sure about the structure in general.  perhaps more code shuffling is indicated at some point.)
editKindForm :: HasCallStack => (EditKind -> GlobalAction) -> View '[EditKindFormProps]
editKindForm onSelect = mkView "EditKindForm" $ \(EditKindFormProps mactive) -> do
    div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
      div_ ["className" $= "grid-wrapper"] $ do
        div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
          div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] ""
          div_ ["className" $= "c-vdoc-toolbar-extension__modification c-vdoc-toolbar-extension--expanded"] $ do  -- (RENAME: Edit)
            editButton mactive `mapM_` [Grammar, Phrasing, Meaning]
  where
    editButton :: Maybe EditKind -> EditKind -> ReactElementM eventHandler ()
    editButton mactive kind =
      let size = if Just kind == mactive then XXLarge else Large in
      iconButton_ $ defaultIconButtonProps @[GlobalAction]
        & iconButtonPropsListKey      .~ cs (show kind)
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar-extension" True ("icon-New_Edit", "dark") size
        & iconButtonPropsElementName  .~ "btn-new-mod-text" -- RENAME: mod => edit
        & iconButtonPropsLabel        .~ cs (show kind)
        & iconButtonPropsOnClick      .~ [onSelect kind]
        & iconButtonPropsClickPropag  .~ False

editKindForm_ :: HasCallStack => (EditKind -> GlobalAction) -> EditKindFormProps -> ReactElementM ViewEventHandler ()
editKindForm_ onSelect = view_ (editKindForm onSelect) "editToolbarExtension_"
