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

import           Control.Lens (makeLenses, (^.), (^?), at, to, _Just)
import           Data.Maybe (isNothing)
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import qualified Data.Text as DT
import qualified Data.Tree as Tree
import           React.Flux

import           Refine.Common.Types
import qualified Refine.Frontend.ErrorHandling as E
import           Refine.Frontend.ThirdPartyViews (skylight_)
import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.Contribution.Types as RS
import qualified Refine.Frontend.Colors as C
import           Refine.Frontend.TKey
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import           Refine.Frontend.UtilityWidgets

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

dialogWidth :: Int
dialogWidth = 640

leftFor :: Int -> Int
leftFor windowWidth = (windowWidth - dialogWidth) `quot` 2

dialogStyles :: [Style]
dialogStyles = [ -- Style "display" ("block" :: String)
                --, Style "minHeight" ("200px" :: String)
                 -- Style "padding" ("3rem 1.0rem 1.0rem" :: String)

                  Style "width" (show dialogWidth <> "px")
                , Style "marginLeft" ("0" :: String)
                , Style "marginTop" ("0" :: String)
                , Style "zIndex" (6050 :: Int)

                , Style "position" ("absolute" :: String)
                ]

-- RENAME: addCommentDialogStyles
vdoc_overlay_content__add_comment :: [Style]
vdoc_overlay_content__add_comment = [ Style "backgroundColor" C.vdoc_comment
                                    ] <> dialogStyles

-- is vdoc_overlay_content__comment in CSS

-- RENAME: showNoteDialogStyles
vdoc_overlay_content__note :: [Style]
vdoc_overlay_content__note = [ Style "backgroundColor" C.vdoc_comment
                              ] <> dialogStyles

-- RENAME: showDiscussionDialogStyles
vdoc_overlay_content__discussion :: [Style]
vdoc_overlay_content__discussion = [ Style "backgroundColor" C.vdoc_discussion
                                    ] <> dialogStyles

overlayStyles :: [Style]
overlayStyles =
  [ Style "zIndex" (6010 :: Int)
  , Style "backgroundColor" C.overlay_backdrop
  ]

data CommentDisplayProps = CommentDisplayProps
  { _commentText :: CommentText
  , _iconStyle :: IconDescription
  , _userName :: String
  , _creationDate :: String
  , _contentStyle :: [Style]
  , _topOffset    :: SC.OffsetFromDocumentTop
  , _windowWidth  :: Int
  }

makeLenses ''CommentDisplayProps

showComment :: ReactView CommentDisplayProps
showComment = defineView "ShowComment" $ \props ->
  let extraStyles = [ Style "top"        (show (props ^. topOffset . SC.unOffsetFromDocumentTop + 5) <> "px")
                    , Style "left" (show (leftFor (props ^. windowWidth)) <> "px")
                    , Style "height"    ("" :: String)
                    , Style "minHeight" ("100px" :: String)
                    ]
  in skylight_ ["isVisible" &= True
           , on "onCloseClicked"   $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentOverlay)
           , on "onOverlayClicked" $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentOverlay)
           , "dialogStyles" @= ((props ^. contentStyle) <> extraStyles)
           , "overlayStyles" @= overlayStyles
           , "closeButtonStyle" @= [Style "top" ("" :: String), Style "bottom" ("0" :: String)]
           , "titleStyle" @= [Style "margin" ("0" :: String)]
           ] $ do
    -- div_ ["className" $= "c-vdoc-overlay-content c-vdoc-overlay-content--comment"] $ do

        div_ ["style" @= [Style "marginLeft" ("96%" :: String)]] $ do             -- FIXME: How to do this properly?
          icon_ (IconProps "c-vdoc-overlay-content" False (props ^. iconStyle) XL)

        div_ ["className" $= "c-vdoc-overlay-content__copy"] $ elemText (props ^. commentText)

        -- edit/comment user meta data -->
        div_ ["className" $= "c-vdoc-overlay-meta"] $ do
            span_ ["className" $= "c-vdoc-overlay-meta__user-avatar"] $ do
                icon_ (IconProps "c-vdoc-overlay-meta" False ("icon-User", "bright") M)
            span_ ["className" $= "c-vdoc-overlay-meta__user"] $ elemString (props ^. userName)
            span_ ["className" $= "c-vdoc-overlay-meta__date"] $ elemString (props ^. creationDate) -- or what is this?
        -- END: edit/comment user meta data -->

        -- vote buttons -->
        div_ ["className" $= "c-vdoc-overlay-votes"] $ do

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-up"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_positive", "dark") XL)

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-down"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_negative", "dark") XL)
        -- END: vote buttons -->

        div_ ["style" @= [Style "marginBottom" ("20px" :: String)]] "" -- make some space for the close button

showComment_ :: CommentDisplayProps -> ReactElementM eventHandler ()
showComment_ props = view showComment props mempty

showNoteProps :: M.Map (ID Note) Note -> RS.GlobalState -> ShowNoteProps
showNoteProps notes rs = case (maybeNote, maybeOffset) of
  (Just note, Just offset) -> ShowNotePropsJust note offset (rs ^. RS.gsScreenState . SC.ssWindowWidth)
  (Just note, Nothing)     -> E.gracefulError ("We have a note " <> show note <> " but no offset - how can this be?")
                                              ShowNotePropsNothing
  (Nothing,   Just offset) -> E.gracefulError ("We have an offset " <> show offset <> " but no note - how can this be?")
                                              ShowNotePropsNothing
  _                        -> ShowNotePropsNothing
  where
    maybeContribID = rs ^. RS.gsContributionState . RS.csDisplayedContributionID
    maybeNoteID :: Maybe (ID Note) = getNoteID =<< maybeContribID
    maybeNote = (`M.lookup` notes) =<< maybeNoteID
    maybeOffset = do
      nid <- maybeNoteID
      rs ^? RS.gsContributionState . RS.csMarkPositions . to RS._unMarkPositions
          . at (ContribIDNote nid) . _Just . RS.markPositionBottom


data ShowNoteProps = ShowNotePropsJust
  { _snpNote        :: Note
  , _snpTop         :: SC.OffsetFromDocumentTop
  , _snpWindowWidth :: Int
  }
  | ShowNotePropsNothing

showNote :: ReactView ShowNoteProps
showNote = defineView "ShowNote" $ \case
  ShowNotePropsNothing -> mempty
  ShowNotePropsJust note top windowWidth1 ->
    let commentText1  = (note ^. noteText)
        iconStyle1    = ("icon-Note", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         vdoc_overlay_content__note top windowWidth1)

showNote_ :: ShowNoteProps -> ReactElementM eventHandler ()
showNote_ props = view showNote props mempty

data ShowDiscussionProps = ShowDiscussionPropsJust
  { _sdpNote        :: CompositeDiscussion
  , _sdpTop         :: SC.OffsetFromDocumentTop
  , _sdpWindowWidth :: Int
  }
  | ShowDiscussionPropsNothing

showDiscussionProps :: M.Map (ID Discussion) CompositeDiscussion -> RS.GlobalState -> ShowDiscussionProps
showDiscussionProps discussions rs = case (maybeDiscussion, maybeOffset) of
  (Just discussion, Just offset) -> ShowDiscussionPropsJust discussion offset (rs ^. RS.gsScreenState . SC.ssWindowWidth)
  (Just discussion, Nothing)     -> E.gracefulError ("We have a discussion " <> show discussion
                                                     <> " but no offset - how can this be?")
                                              ShowDiscussionPropsNothing
  (Nothing,         Just offset) -> E.gracefulError ("We have an offset " <> show offset
                                                     <> " but no discussion - how can this be?")
                                              ShowDiscussionPropsNothing
  _                              -> ShowDiscussionPropsNothing
  where
    maybeContribID = rs ^. RS.gsContributionState . RS.csDisplayedContributionID
    maybeDiscussionID :: Maybe (ID Discussion) = getDiscussionID =<< maybeContribID
    maybeDiscussion = (`M.lookup` discussions) =<< maybeDiscussionID
    maybeOffset = do
      did <- maybeDiscussionID
      rs ^? RS.gsContributionState . RS.csMarkPositions . to RS._unMarkPositions
          . at (ContribIDDiscussion did) . _Just . RS.markPositionBottom

showDiscussion :: ReactView ShowDiscussionProps
showDiscussion = defineView "ShowDiscussion" $ \case
  ShowDiscussionPropsNothing -> mempty
  ShowDiscussionPropsJust discussion top windowWidth1 ->
    let commentText1  = (Tree.rootLabel (discussion ^. compositeDiscussionTree) ^. statementText)
        iconStyle1    = ("icon-Discussion", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         vdoc_overlay_content__discussion top windowWidth1)

showDiscussion_ :: ShowDiscussionProps -> ReactElementM eventHandler ()
showDiscussion_ props = view showDiscussion props mempty

showQuestion :: ReactView (Maybe CompositeQuestion)
showQuestion = defineView "ShowQuestion" $ \case
  Nothing -> mempty
  Just question ->
    let overlayStyle1 = [ Style "backgroundColor" C.vdoc_question ]
        commentText1  = (question ^. compositeQuestion . questionText)
        iconStyle1    = ("icon-Question", "dark")
        userName1     = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                         overlayStyle1 (SC.OffsetFromDocumentTop 0) 800)

showQuestion_ :: Maybe CompositeQuestion -> ReactElementM eventHandler ()
showQuestion_ question = view showQuestion question mempty


data AddCommentProps = AddCommentProps
  { _acpEditor      :: RS.ContributionEditorData
  , _acpCategory    :: Maybe RS.CommentCategory
  , _acpWindowWidth :: Int
  }

makeLenses ''AddCommentProps

data CommentInputProps = CommentInputProps
  { _cipRange       :: Maybe RS.Range
  , _cipCategory    :: Maybe RS.CommentCategory
  , _cipWindowWidth :: Int
  }

makeLenses ''CommentInputProps

-- was add-annotation
addComment :: TranslationsCS -> ReactView CommentInputProps
addComment __ = defineView "AddComment" $ \props ->
    let top = case props ^. cipRange of
              Nothing -> 0 -- FIXME: Invent a suitable top for the "general comment" case
              Just range -> (range ^. RS.rangeBottomOffset . SC.unOffsetFromViewportTop)
                          + (range ^. RS.rangeScrollOffset . SC.unScrollOffsetOfViewport)
        extraStyles = [ Style "top"    (show (top + 5) <> "px")
                      , Style "left" (show (leftFor (props ^. cipWindowWidth)) <> "px")
                      , Style "height" ("560px" :: String)
                      ]
    in skylight_ ["isVisible" &= True
             , on "onCloseClicked"   $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentEditor)
             , on "onOverlayClicked" $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentEditor)
             , "dialogStyles" @= (vdoc_overlay_content__add_comment <> extraStyles)
             , "overlayStyles" @= overlayStyles
             , "titleStyle" @= [Style "margin" ("0" :: String)]
             ]  $ do

      icon_ (IconProps "c-vdoc-overlay-content" False ("icon-New_Comment", "dark") XL)

      span_ [ "className" $= "c-vdoc-overlay-content__title"
            , "style" @= [ Style "fontSize" ("1.125rem" :: String)
                         , Style "lineHeight" ("1.15" :: String)
                         , Style "marginBottom" ("0.875rem" :: String)
                         , Style "marginLeft" ("1rem" :: String)
                         , Style "fontWeight" ("bold" :: String)
                         ]
            ] (__ add_a_comment)

      hr_ []

      commentInput_ props


addComment_ :: TranslationsCS -> AddCommentProps -> ReactElementM eventHandler ()
addComment_ __ (AddCommentProps RS.EditorIsHidden _ _) = mempty
addComment_ __ (AddCommentProps (RS.EditorIsVisible range) category windowWidth1) =
  view (addComment __) (CommentInputProps range category windowWidth1) mempty


commentInput :: ReactView CommentInputProps
commentInput = defineStatefulView "CommentInput" (RS.CommentInputState "") $ \curState props ->
    div_ $ do
      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 1: "
          span_ ["className" $= "bold"] "Select a type for your comment:"

      div_ ["className" $= "c-vdoc-overlay-content__annotation-type"] $ do  -- RENAME: annotation => comment
        iconButton_ (IconButtonProps
                      (IconProps "c-vdoc-overlay-content"
                                 False
                                 ("icon-Note", if props ^. cipCategory == Just RS.Note then "RO" else "dark")
                                 L)
                      "category"
                      "comment"
                      ""
                      "add a note"
                      False
                      (\_ -> RS.dispatch . RS.ContributionAction $ RS.SetCommentCategory RS.Note)
                      []
                    )

        span_ ["style" @= [Style "marginRight" ("1rem" :: String)]] ""

        iconButton_ (IconButtonProps
                      (IconProps "c-vdoc-overlay-content"
                             False
                             ("icon-Discussion", if props ^. cipCategory == Just RS.Discussion then "RO" else "dark")
                             L)
                      "category"
                      "discussion"
                      ""
                      "start a discussion"
                      False
                      (\_ -> RS.dispatch . RS.ContributionAction $ RS.SetCommentCategory RS.Discussion)
                      []
                    )

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 2: "
          span_ ["className" $= "bold"] "enter your comment:"

      form_ [ "target" $= "#"
           , "action" $= "POST"] $ do
        textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"  -- RENAME: annotation => comment
                  , "className" $= "o-wysiwyg o-form-input__textarea"
                  , "style" @= [ Style "resize" ("none" :: String)
                               , Style "width" (600 :: Int)
                               , Style "height" (240 :: Int)
                               ]
                  -- Update the current state with the current text in the textbox, sending no actions
                  , onChange $ \evt state -> ([], Just $ state { RS._commentInputStateText = target evt "value" } )
                  ] mempty

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 3: "
          span_ ["className" $= "bold"] "finish"

      iconButton_
        (IconButtonProps
          (IconProps "c-vdoc-overlay-content" False ("icon-Share", "dark") L)
          "submit"
          ""
          ""
          "submit"
          ((0 == DT.length (curState ^. RS.commentInputStateText)) || isNothing (props ^. cipCategory)) -- no text or no category -> disable button
          (\_ -> RS.dispatch (RS.ContributionAction
                (RS.SubmitComment (curState ^. RS.commentInputStateText) (props ^. cipCategory) (props ^. cipRange)))
              <> RS.dispatch (RS.ContributionAction RS.HideCommentEditor))
          []
        )

commentInput_ :: CommentInputProps -> ReactElementM eventHandler ()
commentInput_  props = view commentInput props mempty
