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


module Refine.Frontend.Bubbles.Overlay where

import           Control.Lens ((^.))
import           Data.Maybe (isNothing)
import           Data.Monoid ((<>))
import qualified Data.Text as DT
import           React.Flux

import           Refine.Frontend.ThirdPartyViews (overlay_)
import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.Bubbles.Types as RS
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import           Refine.Frontend.UtilityWidgets

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


vdoc_overlay_content :: [Style]
vdoc_overlay_content = [ Style "display" ("block" :: String)
                       , Style "minHeight" ("200px" :: String)
                       , Style "padding" ("15rem 10rem 10rem" :: String)
                       ]

showComment :: ReactView Bool
showComment = defineView "ShowComment" $ \showOverlay ->
  let vdoc_overlay_content__comment = [ Style "backgroundColor" ("rgb(219, 204, 221)" :: String) -- vdoc-comment, lightred
                                      ]
  in overlay_ ["isVisible" &= showOverlay
           , on "onCloseClicked" $ \_ -> RS.dispatch (RS.BubblesAction RS.HideCommentOverlay)
           , "hideOnOverlayClicked" &= True
           , "dialogStyles" @= (vdoc_overlay_content <> vdoc_overlay_content__comment)
           ] $ do
    -- div_ ["className" $= "c-vdoc-overlay-content c-vdoc-overlay-content--comment"] $ do

        icon_ (IconProps "c-vdoc-overlay-content" False ("icon-Remark", "dark") L)
        {-
        div_ [className $= "c-vdoc-overlay-content__icon iconsize-l">
            div_ [className $= "icon-Remark_dark">
                <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
        -}

        h4_ ["className" $= "c-vdoc-overlay-content__title"] "Title of comment"

        div_ ["className" $= "c-vdoc-overlay-content__copy"]
            "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."

        -- edit/comment user meta data -->
        div_ ["className" $= "c-vdoc-overlay-meta"] $ do
            span_ ["className" $= "c-vdoc-overlay-meta__user-avatar"] $ do
                icon_ (IconProps "c-vdoc-overlay-meta" False ("icon-User", "bright") M)
                {-
                div_ [className $= "c-vdoc-overlay-meta__icon icon-User_bright iconsize-m">
                    <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                -}
            span_ ["className" $= "c-vdoc-overlay-meta__user"] "meisterkaiser"
            span_ ["className" $= "c-vdoc-overlay-meta__date"] "24. 05. 2016"
        -- END: edit/comment user meta data -->

        -- vote buttons -->
        div_ ["className" $= "c-vdoc-overlay-votes"] $ do

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-up"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_positive", "dark") XL)
                {-
                div_ [className $= "c-vdoc-overlay-votes__icon">
                    div_ [className $= "o-icon-highlight icon-Vote_positive_dark iconsize-xl">
                        <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                -}

            button_ ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-down"] $ do
                icon_ (IconProps "c-vdoc-overlay-votes" True ("icon-Vote_negative", "dark") XL)
                {-
                div_ [className $= "c-vdoc-overlay-votes__icon">
                    div_ [className $= "o-icon-highlight icon-Vote_negative_dark iconsize-xl">
                        <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                -}

showComment_ :: Bool -> ReactElementM eventHandler ()
showComment_ showOverlay = view showComment showOverlay mempty

-- was add-annotation
addComment :: ReactView (Bool, Maybe RS.Range, Maybe RS.CommentCategory)
addComment = defineView "AddComment" $ \(showOverlay, forRange, commentCategory) ->
  let vdoc_overlay_content__add_comment = [ Style "backgroundColor" ("rgb(219, 204, 221)" :: String) -- vdoc-comment, lightred
                                          , Style "zIndex" (6010 :: Int)
                                          ]
  in overlay_ ["isVisible" &= showOverlay
           , on "onCloseClicked" $ \_ -> RS.dispatch (RS.BubblesAction RS.HideCommentEditor)
           , "hideOnOverlayClicked" &= True
           , "dialogStyles" @= (vdoc_overlay_content <> vdoc_overlay_content__add_comment)
           , "overlayStyles" @= [Style "zIndex" (6000 :: Int)]
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
                    (\_ -> RS.dispatch . RS.BubblesAction $ RS.SetCommentCategory RS.Note)
                  )
      iconButton_ (IconButtonProps
                    (IconProps "c-vdoc-overlay-content" True ("icon-Discussion", "dark") L)
                    "category"
                    "discussion"
                    ""
                    "start a discussion"
                    False
                    (\_ -> RS.dispatch . RS.BubblesAction $ RS.SetCommentCategory RS.Discussion)
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
        (\_ -> RS.dispatch (RS.BubblesAction (RS.SubmitComment (curState ^. RS.commentInputStateText) category forRange))
            <> RS.dispatch (RS.BubblesAction RS.HideCommentEditor))
      )

commentInput_ :: Maybe RS.Range -> Maybe RS.CommentCategory -> ReactElementM eventHandler ()
commentInput_  forRange category = view commentInput (forRange, category) mempty
