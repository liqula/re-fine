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
import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.TKey
import           Refine.Frontend.Types
import           Refine.Frontend.Util

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

dialogWidth :: Int
dialogWidth = 640

leftFor :: Int -> Int
leftFor windowWidth = (windowWidth - dialogWidth) `quot` 2

dialogStyles :: [Decl]
dialogStyles = [ -- Style "display" ("block" :: String)
                 -- Style "minHeight" ("200px" :: String)
                 -- Style "padding" ("3rem 1.0rem 1.0rem" :: String)

                  decl "width" (Px dialogWidth)
                , decl "marginLeft" (Px 0)
                , decl "marginTop" (Px 0)
                , decl @Int "zIndex" 6050

                , decl "position" (Ident "absolute")
                ]

-- RENAME: addCommentDialogStyles
vdoc_overlay_content__add_comment :: [Decl]
vdoc_overlay_content__add_comment = [decl "backgroundColor" C.VDocComment] <> dialogStyles

-- is vdoc_overlay_content__comment in CSS

-- RENAME: showNoteDialogStyles
vdoc_overlay_content__note :: [Decl]
vdoc_overlay_content__note = [decl "backgroundColor" C.VDocNote] <> dialogStyles

-- RENAME: showDiscussionDialogStyles
vdoc_overlay_content__discussion :: [Decl]
vdoc_overlay_content__discussion = [decl "backgroundColor" C.VDocDiscussion] <> dialogStyles

overlayStyles :: [Decl]
overlayStyles =
  [ decl @Int "zIndex" 6010
  , decl "backgroundColor" C.OverlayBackdrop
  ]


showComment :: View '[CommentDisplayProps]
showComment = mkView "ShowComment" $ \props ->
  let extraStyles = [ decl "top" (Px $ props ^. cdpTopOffset . unOffsetFromDocumentTop + 5)
                    , decl "left" (Px . leftFor $ props ^. cdpWindowWidth)
                    , decl "height" (Px 0)
                    , decl "minHeight" (Px 100)
                    ]
  in skylight_ ["isVisible" &= True
           , RF.on "onCloseClicked"   $ \_ -> dispatch (ContributionAction HideCommentOverlay)
           , RF.on "onOverlayClicked" $ \_ -> dispatch (ContributionAction HideCommentOverlay)
           , "dialogStyles" @= declsToJSON ((props ^. cdpContentStyle) <> extraStyles)
           , "overlayStyles" @= declsToJSON overlayStyles
           , "closeButtonStyle" @= declsToJSON [decl "top" (Px 0), decl "bottom" (Px 0)]
           , "titleStyle" @= declsToJSON [decl "margin" (Px 0)]
           ] $ do
    -- div_ ["className" $= "c-vdoc-overlay-content c-vdoc-overlay-content--comment"] $ do

        div_ [style [decl "marginLeft" (Percentage 96)]] $ do             -- FIXME: How to do this properly?
          icon_ (IconProps "c-vdoc-overlay-content" False (props ^. cdpIconStyle) XL)

        div_ ["className" $= "c-vdoc-overlay-content__copy"] $ elemText (props ^. cdpCommentText)

        -- edit/comment user meta data -->
        div_ ["className" $= "c-vdoc-overlay-meta"] $ do
            span_ ["className" $= "c-vdoc-overlay-meta__user-avatar"] $ do
                icon_ (IconProps "c-vdoc-overlay-meta" False ("icon-User", "bright") M)
            span_ ["className" $= "c-vdoc-overlay-meta__user"] $ elemCS (props ^. cdpUserName)
            span_ ["className" $= "c-vdoc-overlay-meta__date"] $ elemCS (props ^. cdpCreationDate) -- or what is this?
        -- END: edit/comment user meta data -->

        -- vote buttons -->
        div_ ["className" $= "c-vdoc-overlay-votes"] $ do

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-up"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_positive", "dark") XL)

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-down"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_negative", "dark") XL)
        -- END: vote buttons -->

        div_ [style [decl "marginBottom" (Px 20)]] "" -- make some space for the close button

showComment_ :: CommentDisplayProps -> ReactElementM eventHandler ()
showComment_ !props = view_ showComment "showComment_" props


showNoteProps :: M.Map (ID Note) Note -> GlobalState -> ShowNoteProps
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
      rs ^? gsContributionState . csMarkPositions . markPositionsMap
          . at (ContribIDNote nid) . _Just . markPositionBottom

    err haveT haveV missT = gracefulError (unwords ["showNoteProps: we have a", haveT, show haveV, "but no", missT])


showNote :: View '[ShowNoteProps]
showNote = mkView "ShowNote" $ \case
  ShowNotePropsNothing -> mempty
  ShowNotePropsJust note top windowWidth1 ->
    let commentText1  = (note ^. noteText)
        iconStyle1    = ("icon-Note", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         vdoc_overlay_content__note top windowWidth1)

showNote_ :: ShowNoteProps -> ReactElementM eventHandler ()
showNote_ !props = view_ showNote "showNote_" props


showDiscussionProps :: M.Map (ID Discussion) CompositeDiscussion -> GlobalState -> ShowDiscussionProps
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
      rs ^? gsContributionState . csMarkPositions . markPositionsMap
          . at (ContribIDDiscussion did) . _Just . markPositionBottom

    err haveT haveV missT = gracefulError (unwords ["showNoteProps: we have a", haveT, show haveV, "but no", missT])


showDiscussion :: View '[ShowDiscussionProps]
showDiscussion = mkView "ShowDiscussion" $ \case
  ShowDiscussionPropsNothing -> mempty
  ShowDiscussionPropsJust discussion top windowWidth1 ->
    let commentText1  = (Tree.rootLabel (discussion ^. compositeDiscussionTree) ^. statementText)
        iconStyle1    = ("icon-Discussion", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         vdoc_overlay_content__discussion top windowWidth1)

showDiscussion_ :: ShowDiscussionProps -> ReactElementM eventHandler ()
showDiscussion_ !props = view_ showDiscussion "showDiscussion_" props


showQuestion :: View '[ShowQuestionProps]
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

showQuestion_ :: ShowQuestionProps -> ReactElementM eventHandler ()
showQuestion_ !props = view_ showQuestion "showQuestion_" props


addContributionDialogFrame :: Bool -> ST -> Maybe Range -> Int -> ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
addContributionDialogFrame False _ _ _ _ = pure ()
addContributionDialogFrame True title mrange windowWidth child =
    let top = case mrange of
              Nothing -> 30
              Just range -> (range ^. rangeBottomOffset . unOffsetFromViewportTop)
                          + (range ^. rangeScrollOffset . unScrollOffsetOfViewport)
        extraStyles = [ decl "top" (Px $ top + 5)
                      , decl "left" (Px $ leftFor windowWidth)
                      , decl "height" (Px 560)
                      ]
    in skylight_ ["isVisible" &= True
             , RF.on "onCloseClicked"   $ \_ -> dispatch (ContributionAction HideCommentEditor)
             , RF.on "onOverlayClicked" $ \_ -> dispatch (ContributionAction HideCommentEditor)
             , "dialogStyles" @= declsToJSON (vdoc_overlay_content__add_comment <> extraStyles)
             , "overlayStyles" @= declsToJSON overlayStyles
             , "titleStyle" @= declsToJSON [decl "margin" (Px 0)]
             ]  $ do

      icon_ (IconProps "c-vdoc-overlay-content" False ("icon-New_Comment", "dark") XL)

      span_ [ "className" $= "c-vdoc-overlay-content__title"
            , style [ decl "fontSize" (Rem 1.125)
                    , decl "lineHeight" (Mm 1.15)
                    , decl "marginBottom" (Rem 0.875)
                    , decl "marginLeft" (Rem 1)
                    , decl "fontWeight" (Ident "bold")
                    ]
            ] (elemText title)

      hr_ []

      child

addComment :: Translations -> View '[AddContributionProps CommentKind]
addComment __ = mkView "AddComment" $ \props -> addContributionDialogFrame
  (props ^. acpVisible)
  (__ add_a_comment)
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  (commentInput_ props)

addComment_ :: Translations -> AddContributionProps CommentKind -> ReactElementM eventHandler ()
addComment_ __ !props = view_ (addComment __) "addComment_" props


contributionDialogTextForm :: Int -> ST -> ReactElementM (StatefulViewEventHandler AddContributionFormState) ()
contributionDialogTextForm stepNumber promptText = do
  div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
    p_ $ do
      elemString $ "Step " <> show stepNumber <> ": "
      span_ ["className" $= "bold"] $ do
        elemText promptText

  form_ [ "target" $= "#"
        , "action" $= "POST"] $ do
    textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"  -- RENAME: annotation => comment
              , "className" $= "o-wysiwyg o-form-input__textarea"
              , style [ decl "resize" (Ident "none")
                      , decl "width" (Px 600)
                      , decl "height" (Px 240)
                      ]
              -- Update the current state with the current text in the textbox, sending no actions
              , onChange $ \evt st -> ([], Just $ st & addContributionFormState .~ target evt "value")
              ]
      mempty


commentInput :: View '[AddContributionProps CommentKind]
commentInput = mkStatefulView "CommentInput" (AddContributionFormState "") $ \curState props ->
    div_ $ do
      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 1: "
          span_ ["className" $= "bold"] "Select a type for your comment:"

      let checkAcpKind k = if props ^. acpKind == Just k then "RO" else "dark"

      div_ ["className" $= "c-vdoc-overlay-content__annotation-type"] $ do  -- RENAME: annotation => comment
        iconButton_ $ def @IconButtonProps
          & iconButtonPropsListKey      .~ "note"
          & iconButtonPropsIconProps . iconPropsBlockName .~ "c-vdoc-overlay-content"
          & iconButtonPropsIconProps . iconPropsDesc      .~ ("icon-Note", checkAcpKind CommentKindNote)
          & iconButtonPropsElementName  .~ "category"  -- RENAME: category => kind
          & iconButtonPropsModuleName   .~ "comment"
          & iconButtonPropsLabel        .~ "add a node"
          & iconButtonPropsOnClick      .~ [ContributionAction $ SetCommentKind CommentKindNote]

        span_ [style [decl "marginRight" (Rem 1)]] mempty

        iconButton_ $ def @IconButtonProps
          & iconButtonPropsListKey      .~ "discussion"
          & iconButtonPropsIconProps . iconPropsBlockName .~ "c-vdoc-overlay-content"
          & iconButtonPropsIconProps . iconPropsDesc      .~ ("icon-Discussion", checkAcpKind CommentKindDiscussion)
          & iconButtonPropsElementName  .~ "category"
          & iconButtonPropsModuleName   .~ "discussion"  -- RENAME: category => kind
          & iconButtonPropsLabel        .~ "start a discussion"
          & iconButtonPropsOnClick      .~ [ContributionAction $ SetCommentKind CommentKindDiscussion]

      hr_ []

      contributionDialogTextForm 2 "enter your comment:"

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 3: "
          span_ ["className" $= "bold"] "finish"

      let notATextOrKind = 0 == ST.length (curState ^. addContributionFormState)
                        || isNothing (props ^. acpKind)
        in iconButton_ $ def @IconButtonProps
          & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" False ("icon-Share", "dark") L
          & iconButtonPropsElementName  .~ "submit"
          & iconButtonPropsLabel        .~ "submit"
          & iconButtonPropsDisabled     .~ notATextOrKind
          & iconButtonPropsOnClick      .~
                [ ContributionAction $ SubmitComment (curState ^. addContributionFormState) (props ^. acpKind)
                , ContributionAction ClearRange
                , ContributionAction HideCommentEditor
                ]

commentInput_ :: AddContributionProps CommentKind -> ReactElementM eventHandler ()
commentInput_ !props = view_ commentInput "commentInput_" props


addEdit :: View '[AddContributionProps EditKind]
addEdit = mkView "AddEdit" $ \props -> addContributionDialogFrame
  (props ^. acpVisible)
  "add an edit"
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  (editInput_ props)

addEdit_ :: AddContributionProps EditKind -> ReactElementM eventHandler ()
addEdit_ = view_ addEdit "addEdit_"


-- | FUTUREWORK: there is *some* code sharing between 'editInput_' and 'commentInput_', but there may be
-- room for more.
--
-- FUTUREWORK: kind change is a nice example of local signals between two components.  how is this
-- handled in react?  should we have a second global store here that is just shared between
-- 'editInput' and and 'editKindForm'?
editInput :: View '[AddContributionProps EditKind]
editInput = mkStatefulView "EditInput" (AddContributionFormState "") $ \curState props -> do
    div_ $ do
      elemString "Step 1: "
      span_ ["className" $= "bold"] "Type of this edit:"
      liftViewToStateHandler $ editKindForm_ (DocumentAction . DocumentUpdateEditKind) (EditKindFormProps $ props ^. acpKind)

    hr_ []

    contributionDialogTextForm 2 "describe your motivation for this edit:"

    hr_ []

    iconButton_ $ def @IconButtonProps
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXL
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsOnClick      .~ [ DocumentAction $ DocumentSave (curState ^. addContributionFormState)
                                             , ContributionAction ClearRange
                                             ]

    iconButton_ $ def @IconButtonProps
            & iconButtonPropsListKey      .~ "cancel"
            & iconButtonPropsIconProps    .~ IconProps "c-mainmenu-header" True ("icon-Close", "dark") XXL
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "cancel"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsOnClick      .~ [DocumentAction DocumentCancelSave]


editInput_ :: AddContributionProps EditKind -> ReactElementM eventHandler ()
editInput_ !props = view_ editInput "editInput_" props
