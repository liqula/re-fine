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


module Refine.Frontend.Contributions.Overlay where

import           Control.Lens (makeLenses, (^.))
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>))
import qualified Data.Text as DT
import qualified Data.Tree as Tree
import           React.Flux

import           Refine.Common.Types
import           Refine.Frontend.ThirdPartyViews (overlay_)
import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.Contributions.Types as RS
import qualified Refine.Frontend.Colors as C
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import           Refine.Frontend.UtilityWidgets

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


vdoc_overlay_content :: [Style]
vdoc_overlay_content = [ Style "display" ("block" :: String)
                       , Style "minHeight" ("200px" :: String)
                       , Style "padding" ("15rem 10rem 10rem" :: String)
                       ]

vdoc_overlay_content__add_comment :: [Style]
vdoc_overlay_content__add_comment = [ Style "backgroundColor" C.vdoc_comment
                                    , Style "zIndex" (6010 :: Int)
                                    ]

-- is vdoc_overlay_content__comment in CSS
vdoc_overlay_content__note :: [Style]
vdoc_overlay_content__note = [ Style "backgroundColor" C.vdoc_comment
                              , Style "zIndex" (6010 :: Int)
                              ]

vdoc_overlay_content__discussion :: [Style]
vdoc_overlay_content__discussion = [ Style "backgroundColor" C.vdoc_discussion
                                    , Style "zIndex" (6010 :: Int)
                                    ]

overlay_styles :: [Style]
overlay_styles = [Style "zIndex" (6000 :: Int)]

data CommentDisplayProps = CommentDisplayProps
  { _commentText :: CommentText
  , _commentTitle :: String
  , _iconStyle :: IconDescription
  , _userName :: String
  , _creationDate :: String
  , _contentStyle :: [Style]
  }

makeLenses ''CommentDisplayProps

showComment :: ReactView CommentDisplayProps
showComment = defineView "ShowComment" $ \props ->
  overlay_ ["isVisible" &= True
           , on "onCloseClicked" $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentOverlay)
           , "hideOnOverlayClicked" &= True
           , "dialogStyles" @= (vdoc_overlay_content <> (props ^. contentStyle))
           , "overlayStyles" @= overlay_styles
           ] $ do
    -- div_ ["className" $= "c-vdoc-overlay-content c-vdoc-overlay-content--comment"] $ do

        icon_ (IconProps "c-vdoc-overlay-content" False (props ^. iconStyle) L) -- or XL? in question

        h4_ ["className" $= "c-vdoc-overlay-content__title"] $ elemString (props ^. commentTitle)

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

showComment_ :: CommentDisplayProps -> ReactElementM eventHandler ()
showComment_ props = view showComment props mempty

showNote :: ReactView (Maybe Note)
showNote = defineView "ShowNote" $ \case
  Nothing -> mempty
  Just note ->
    let commentText1 = (note ^. noteText)
        commentTitle1 = "Title of comment"
        iconStyle1 = ("icon-Remark", "dark")
        userName1 = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 commentTitle1 iconStyle1 userName1 creationDate1 vdoc_overlay_content__note)

showNote_ :: Maybe Note -> ReactElementM eventHandler ()
showNote_ note = view showNote note mempty

showDiscussion :: ReactView (Maybe CompositeDiscussion)
showDiscussion = defineView "ShowDiscussion" $ \case
  Nothing -> mempty
  Just discussion ->
    let commentText1 = (Tree.rootLabel (discussion ^. compositeDiscussionTree) ^. statementText)
        commentTitle1 = "Title of discussion"
        iconStyle1 = ("icon-Discussion", "dark")
        userName1 = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 commentTitle1 iconStyle1 userName1 creationDate1 vdoc_overlay_content__discussion)

showDiscussion_ :: Maybe CompositeDiscussion -> ReactElementM eventHandler ()
showDiscussion_ note = view showDiscussion note mempty

showQuestion :: ReactView (Maybe CompositeQuestion)
showQuestion = defineView "ShowQuestion" $ \case
  Nothing -> mempty
  Just question ->
    let overlayStyle1 = [ Style "backgroundColor" C.vdoc_question ]
        commentText1 = (question ^. compositeQuestion . questionText)
        commentTitle1 = "Title of question"
        iconStyle1 = ("icon-Question", "dark")
        userName1 = "meisterkaiser"
        creationDate1 = "24. 05. 2016"
    in showComment_ (CommentDisplayProps commentText1 commentTitle1 iconStyle1 userName1 creationDate1 overlayStyle1)

showQuestion_ :: Maybe CompositeQuestion -> ReactElementM eventHandler ()
showQuestion_ question = view showQuestion question mempty


-- was add-annotation
addComment :: ReactView (Bool, Maybe RS.Range, Maybe RS.CommentCategory)
addComment = defineView "AddComment" $ \(showOverlay, forRange, commentCategory) ->
  overlay_ ["isVisible" &= showOverlay
           , on "onCloseClicked" $ \_ -> RS.dispatch (RS.ContributionAction RS.HideCommentEditor)
           , "hideOnOverlayClicked" &= True
           , "dialogStyles" @= (vdoc_overlay_content <> vdoc_overlay_content__add_comment)
           , "overlayStyles" @= overlay_styles
           ]  $ do

    icon_ (IconProps "c-vdoc-overlay-content" False ("icon-Remark", "dark") XL)

    h4_ ["className" $= "c-vdoc-overlay-content__title"] "add a comment"

    commentInput_ forRange commentCategory


addComment_ :: (Bool, Maybe RS.Range) -> Maybe RS.CommentCategory -> ReactElementM eventHandler ()
addComment_ (doShow, forRange) category = view addComment (doShow, forRange, category) mempty


commentInput :: ReactView (Maybe RS.Range, Maybe RS.CommentCategory)
commentInput = defineStatefulView "CommentInput" (RS.CommentInputState "") $ \curState (forRange, category) ->
  div_ $ do
    form_ [ "target" $= "#"
         , "action" $= "POST"] $ do
      textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"  -- RENAME: annotation => comment
                , "className" $= "o-wysiwyg o-form-input__textarea"
                -- Update the current state with the current text in the textbox, sending no actions
                , onChange $ \evt state -> ([], Just $ state { RS._commentInputStateText = target evt "value" } )
                ] mempty

    div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
      p_ $ do
        elemString "Step 1: "
        span_ ["className" $= "bold"] "Select a type for your comment:"

    div_ ["className" $= "c-vdoc-overlay-content__annotation-type"] $ do  -- RENAME: annotation => comment
      iconButton_ (IconButtonProps
                    (IconProps "c-vdoc-overlay-content" True ("icon-Remark", "dark") L)
                    "category"
                    "comment"
                    ""
                    "add a note"
                    False
                    (\_ -> RS.dispatch . RS.ContributionAction $ RS.SetCommentCategory RS.Note)
                    []
                  )
      iconButton_ (IconButtonProps
                    (IconProps "c-vdoc-overlay-content" True ("icon-Discussion", "dark") L)
                    "category"
                    "discussion"
                    ""
                    "start a discussion"
                    False
                    (\_ -> RS.dispatch . RS.ContributionAction $ RS.SetCommentCategory RS.Discussion)
                    []
                  )

    div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
      p_ $ do
        elemString "Step 2: "
        span_ ["className" $= "bold"] "finish"

    iconButton_
      (IconButtonProps
        (IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") L)
        "submit"
        ""
        ""
        "submit"
        ((0 == DT.length (curState ^. RS.commentInputStateText)) || isNothing category) -- no text or no category -> disable button
        (\_ -> RS.dispatch (RS.ContributionAction (RS.SubmitComment (curState ^. RS.commentInputStateText) category forRange))
            <> RS.dispatch (RS.ContributionAction RS.HideCommentEditor))
        []
      )

commentInput_ :: Maybe RS.Range -> Maybe RS.CommentCategory -> ReactElementM eventHandler ()
commentInput_  forRange category = view commentInput (forRange, category) mempty
