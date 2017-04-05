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


module Refine.Frontend.Contribution.Dialog where

import           Control.Lens (makeLenses, (^.), (^?), (&), (.~), at, to, _Just)
import           Data.Default (def)
import           Data.JSString (JSString)
import           Data.Maybe (isNothing)
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import qualified Data.Text as DT
import qualified Data.Tree as Tree
import           React.Flux

import           Refine.Common.Types
import           Refine.Frontend.Test.Console (gracefulError)
import           Refine.Frontend.ThirdPartyViews (skylight_)
import qualified Refine.Frontend.Contribution.Types as RS
import qualified Refine.Frontend.Colors as C
import           Refine.Frontend.CS
import           Refine.Frontend.TKey
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Store.Types as RS
import           Refine.Frontend.Style
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

dialogWidth :: Int
dialogWidth = 640

leftFor :: Int -> Int
leftFor windowWidth = (windowWidth - dialogWidth) `quot` 2

dialogStyles :: [Style]
dialogStyles = [ -- Style "display" ("block" :: String)
                --, Style "minHeight" ("200px" :: String)
                 -- Style "padding" ("3rem 1.0rem 1.0rem" :: String)

                  StylePx "width" dialogWidth
                , StyleInt "marginLeft" 0
                , StyleInt "marginTop" 0
                , StyleInt "zIndex" 6050

                , StyleST "position" "absolute"
                ]

-- RENAME: addCommentDialogStyles
vdoc_overlay_content__add_comment :: [Style]
vdoc_overlay_content__add_comment = [mkStyle "backgroundColor" C.VDocComment] <> dialogStyles

-- is vdoc_overlay_content__comment in CSS

-- RENAME: showNoteDialogStyles
vdoc_overlay_content__note :: [Style]
vdoc_overlay_content__note = [mkStyle "backgroundColor" C.VDocNote] <> dialogStyles

-- RENAME: showDiscussionDialogStyles
vdoc_overlay_content__discussion :: [Style]
vdoc_overlay_content__discussion = [mkStyle "backgroundColor" C.VDocDiscussion] <> dialogStyles

overlayStyles :: [Style]
overlayStyles =
  [ StyleInt "zIndex" 6010
  , mkStyle "backgroundColor" C.OverlayBackdrop
  ]

data CommentDisplayProps = CommentDisplayProps
  { _commentText  :: CommentText
  , _iconStyle    :: IconDescription
  , _userName     :: JSString
  , _creationDate :: JSString
  , _contentStyle :: [Style]
  , _topOffset    :: SC.OffsetFromDocumentTop
  , _windowWidth  :: Int
  }
  deriving (Eq)

makeLenses ''CommentDisplayProps

showComment :: View '[CommentDisplayProps]
showComment = mkView "ShowComment" $ \props ->
  let extraStyles = [ StylePx "top" (props ^. topOffset . SC.unOffsetFromDocumentTop + 5)
                    , StylePx "left" (leftFor (props ^. windowWidth))
                    , StyleST "height" ""
                    , StylePx "minHeight" 100
                    ]
  in skylight_ ["isVisible" &= True
           , on "onCloseClicked"   $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentOverlay)
           , on "onOverlayClicked" $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentOverlay)
           , "dialogStyles" @= ((props ^. contentStyle) <> extraStyles)
           , "overlayStyles" @= overlayStyles
           , "closeButtonStyle" @= [StyleST "top" "", StyleInt "bottom" 0]
           , "titleStyle" @= [StyleInt "margin" 0]
           ] $ do
    -- div_ ["className" $= "c-vdoc-overlay-content c-vdoc-overlay-content--comment"] $ do

        div_ ["style" @= [StylePercentage "marginLeft" 96]] $ do             -- FIXME: How to do this properly?
          icon_ (IconProps "c-vdoc-overlay-content" False (props ^. iconStyle) XL)

        div_ ["className" $= "c-vdoc-overlay-content__copy"] $ elemText (props ^. commentText)

        -- edit/comment user meta data -->
        div_ ["className" $= "c-vdoc-overlay-meta"] $ do
            span_ ["className" $= "c-vdoc-overlay-meta__user-avatar"] $ do
                icon_ (IconProps "c-vdoc-overlay-meta" False ("icon-User", "bright") M)
            span_ ["className" $= "c-vdoc-overlay-meta__user"] $ elemCS (props ^. userName)
            span_ ["className" $= "c-vdoc-overlay-meta__date"] $ elemCS (props ^. creationDate) -- or what is this?
        -- END: edit/comment user meta data -->

        -- vote buttons -->
        div_ ["className" $= "c-vdoc-overlay-votes"] $ do

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-up"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_positive", "dark") XL)

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-down"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_negative", "dark") XL)
        -- END: vote buttons -->

        div_ ["style" @= [StylePx "marginBottom" 20]] "" -- make some space for the close button

showComment_ :: CommentDisplayProps -> ReactElementM eventHandler ()
showComment_ !props = view_ showComment "showComment_" props

showNoteProps :: M.Map (ID Note) Note -> RS.GlobalState -> ShowNoteProps
showNoteProps notes rs = case (maybeNote, maybeOffset) of
  (Just note, Just offset) -> ShowNotePropsJust note offset (rs ^. RS.gsScreenState . SC.ssWindowWidth)
  (Just note, Nothing)     -> err "note" note "offset" ShowNotePropsNothing
  (Nothing,   Just offset) -> err "offset" offset "note" ShowNotePropsNothing
  _                        -> ShowNotePropsNothing
  where
    maybeContribID = rs ^. RS.gsContributionState . RS.csDisplayedContributionID
    maybeNoteID :: Maybe (ID Note) = getNoteID =<< maybeContribID
    maybeNote = (`M.lookup` notes) =<< maybeNoteID
    maybeOffset = do
      nid <- maybeNoteID
      rs ^? RS.gsContributionState . RS.csMarkPositions . to RS._unMarkPositions
          . at (ContribIDNote nid) . _Just . RS.markPositionBottom

    err haveT haveV missT = gracefulError (unwords ["showNoteProps: we have a", haveT, show haveV, "but no", missT])


data ShowNoteProps =
    ShowNotePropsJust
      { _snpNote        :: Note
      , _snpTop         :: SC.OffsetFromDocumentTop
      , _snpWindowWidth :: Int
      }
  | ShowNotePropsNothing
  deriving (Eq)

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

data ShowDiscussionProps =
    ShowDiscussionPropsJust
      { _sdpNote        :: CompositeDiscussion
      , _sdpTop         :: SC.OffsetFromDocumentTop
      , _sdpWindowWidth :: Int
      }
    | ShowDiscussionPropsNothing
  deriving (Eq)

showDiscussionProps :: M.Map (ID Discussion) CompositeDiscussion -> RS.GlobalState -> ShowDiscussionProps
showDiscussionProps discussions rs = case (maybeDiscussion, maybeOffset) of
  (Just discussion, Just offset) -> ShowDiscussionPropsJust discussion offset (rs ^. RS.gsScreenState . SC.ssWindowWidth)
  (Just discussion, Nothing)     -> err "discussion" discussion "offset" ShowDiscussionPropsNothing
  (Nothing,         Just offset) -> err "offset" offset "discussion" ShowDiscussionPropsNothing
  _                              -> ShowDiscussionPropsNothing
  where
    maybeContribID = rs ^. RS.gsContributionState . RS.csDisplayedContributionID
    maybeDiscussionID :: Maybe (ID Discussion) = getDiscussionID =<< maybeContribID
    maybeDiscussion = (`M.lookup` discussions) =<< maybeDiscussionID
    maybeOffset = do
      did <- maybeDiscussionID
      rs ^? RS.gsContributionState . RS.csMarkPositions . to RS._unMarkPositions
          . at (ContribIDDiscussion did) . _Just . RS.markPositionBottom

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

showQuestion :: View '[Maybe CompositeQuestion]
showQuestion = mkView "ShowQuestion" $ \case
  Nothing -> mempty
  Just question ->
    let overlayStyle1 = [mkStyle "backgroundColor" C.VDocQuestion]
        commentText1  = (question ^. compositeQuestion . questionText)
        iconStyle1    = ("icon-Question", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         overlayStyle1 (SC.OffsetFromDocumentTop 0) 800)

showQuestion_ :: Maybe CompositeQuestion -> ReactElementM eventHandler ()
showQuestion_ !question = view_ showQuestion "showQuestion_" question


data AddCommentProps = AddCommentProps
  { _acpEditor      :: RS.ContributionEditorData
  , _acpCategory    :: Maybe RS.CommentCategory
  , _acpWindowWidth :: Int
  }
  deriving (Eq)

makeLenses ''AddCommentProps

data CommentInputProps = CommentInputProps
  { _cipRange       :: Maybe RS.Range
  , _cipCategory    :: Maybe RS.CommentCategory
  , _cipWindowWidth :: Int
  }
  deriving (Eq)

makeLenses ''CommentInputProps

-- was add-annotation
addComment :: Translations -> View '[CommentInputProps]
addComment __ = mkView "AddComment" $ \props ->
    let top = case props ^. cipRange of
              Nothing -> 0 -- FIXME: Invent a suitable top for the "general comment" case
              Just range -> (range ^. RS.rangeBottomOffset . SC.unOffsetFromViewportTop)
                          + (range ^. RS.rangeScrollOffset . SC.unScrollOffsetOfViewport)
        extraStyles = [ StylePx "top" (top + 5)
                      , StylePx "left" (leftFor (props ^. cipWindowWidth))
                      , StylePx "height" 560
                      ]
    in skylight_ ["isVisible" &= True
             , on "onCloseClicked"   $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentEditor)
             , on "onOverlayClicked" $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentEditor)
             , "dialogStyles" @= (vdoc_overlay_content__add_comment <> extraStyles)
             , "overlayStyles" @= overlayStyles
             , "titleStyle" @= [StyleInt "margin" 0]
             ]  $ do

      icon_ (IconProps "c-vdoc-overlay-content" False ("icon-New_Comment", "dark") XL)

      span_ [ "className" $= "c-vdoc-overlay-content__title"
            , "style" @= [ StyleRem "fontSize" 1.125
                         , StyleDouble "lineHeight" 1.15
                         , StyleRem "marginBottom" 0.875
                         , StyleRem "marginLeft" 1
                         , StyleST "fontWeight" "bold"
                         ]
            ] (elemText $ __ add_a_comment)

      hr_ []

      commentInput_ props


addComment_ :: Translations -> AddCommentProps -> ReactElementM eventHandler ()
addComment_ __ (AddCommentProps RS.EditorIsHidden _ _) = mempty
addComment_ __ (AddCommentProps (RS.EditorIsVisible range) category windowWidth1) =
  view_ (addComment __) "addComment_" (CommentInputProps range category windowWidth1)


commentInput :: View '[CommentInputProps]
commentInput = mkStatefulView "CommentInput" (RS.CommentInputState "") $ \curState props ->
    div_ $ do
      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 1: "
          span_ ["className" $= "bold"] "Select a type for your comment:"

      let checkCipKind k = if props ^. cipCategory == Just k then "RO" else "dark"

      div_ ["className" $= "c-vdoc-overlay-content__annotation-type"] $ do  -- RENAME: annotation => comment
        iconButton_ $ def
          & iconButtonPropsListKey      .~ "note"
          & iconButtonPropsIconProps . iconPropsBlockName .~ "c-vdoc-overlay-content"
          & iconButtonPropsIconProps . iconPropsDesc      .~ ("icon-Note", checkCipKind RS.Note)
          & iconButtonPropsElementName  .~ "category"
          & iconButtonPropsModuleName   .~ "comment"
          & iconButtonPropsLabel        .~ "add a node"
          & iconButtonPropsClickActions .~ [RS.ContributionAction $ RS.SetCommentCategory RS.Note]

        span_ ["style" @= [StyleRem "marginRight" 1]] ""

        iconButton_ $ def
          & iconButtonPropsListKey      .~ "discussion"
          & iconButtonPropsIconProps . iconPropsBlockName .~ "c-vdoc-overlay-content"
          & iconButtonPropsIconProps . iconPropsDesc      .~ ("icon-Discussion", checkCipKind RS.Discussion)
          & iconButtonPropsElementName  .~ "category"
          & iconButtonPropsModuleName   .~ "discussion"
          & iconButtonPropsLabel        .~ "start a discussion"
          & iconButtonPropsClickActions .~ [RS.ContributionAction $ RS.SetCommentCategory RS.Discussion]

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 2: "
          span_ ["className" $= "bold"] "enter your comment:"

      form_ [ "target" $= "#"
           , "action" $= "POST"] $ do
        textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"  -- RENAME: annotation => comment
                  , "className" $= "o-wysiwyg o-form-input__textarea"
                  , "style" @= [ StyleST "resize" "none"
                               , StyleInt "width" 600
                               , StyleInt "height" 240
                               ]
                  -- Update the current state with the current text in the textbox, sending no actions
                  , onChange $ \evt state -> ([], Just $ state { RS._commentInputStateText = target evt "value" } )
                  ] mempty

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 3: "
          span_ ["className" $= "bold"] "finish"

      let notATextOrCategory = 0 == DT.length (curState ^. RS.commentInputStateText)
                            || isNothing (props ^. cipCategory)
        in iconButton_ $ def
          & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" False ("icon-Share", "dark") L
          & iconButtonPropsElementName  .~ "submit"
          & iconButtonPropsLabel        .~ "submit"
          & iconButtonPropsDisabled     .~ notATextOrCategory
          & iconButtonPropsClickActions .~
                [ RS.ContributionAction (RS.SubmitComment (curState ^. RS.commentInputStateText) (props ^. cipCategory) (props ^. cipRange))
                , RS.ContributionAction RS.HideCommentEditor
                ]

commentInput_ :: CommentInputProps -> ReactElementM eventHandler ()
commentInput_ !props = view_ commentInput "commentInput_" props
