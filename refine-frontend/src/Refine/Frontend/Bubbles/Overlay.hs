{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Refine.Frontend.Bubbles.Overlay where

import           Control.Lens ((^.))
import           Data.Monoid ((<>))
import           React.Flux

import           Refine.Frontend.ThirdPartyViews (overlay_)
import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.Bubbles.Types as RS
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import           Refine.Frontend.UtilityWidgets



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
           , on "onCloseClicked" $ \_ -> RS.dispatch RS.HideComment
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
addComment :: ReactView (Bool, Maybe RS.Range)
addComment = defineView "AddComment" $ \(showOverlay, forRange) ->
  let vdoc_overlay_content__add_comment = [ Style "backgroundColor" ("rgb(219, 204, 221)" :: String) -- vdoc-comment, lightred
                                          , Style "zIndex" (6010 :: Int)
                                          ]
  in overlay_ ["isVisible" &= showOverlay
           , on "onCloseClicked" $ \_ -> RS.dispatch RS.HideCommentEditor
           , "hideOnOverlayClicked" &= True
           , "dialogStyles" @= (vdoc_overlay_content <> vdoc_overlay_content__add_comment)
           , "overlayStyles" @= [Style "zIndex" (6000 :: Int)]
           ]  $ do

    icon_ (IconProps "c-vdoc-overlay-content" False ("icon-Remark", "dark") XL)

    h4_ ["className" $= "c-vdoc-overlay-content__title"] "add a comment"

    commentInput_ forRange


addComment_ :: (Bool, Maybe RS.Range) -> ReactElementM eventHandler ()
addComment_ showForRange = view addComment showForRange mempty


commentInput :: ReactView (Maybe RS.Range)
commentInput = defineStatefulView "CommentInput" (RS.CommentInputState "" "") $ \curState forRange ->
  div_ $ do
    form_ [ "target" $= "#"
         , "action" $= "POST"] $ do
      textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"
                , "className" $= "o-wysiwyg o-form-input__textarea"
                -- Update the current state with the current text in the textbox, sending no actions
                , onChange $ \evt state -> ([], Just $ state { RS._commentInputStateText = target evt "value" } )
                ] mempty

    div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
      p_ $ do
        elemString "Step 1: "
        span_ ["className" $= "bold"] "Select a type for your comment:"

    div_ ["className" $= "c-vdoc-overlay-content__annotation-type"] $ do
      iconButton_ (IconButtonProps
                    (IconProps "c-vdoc-overlay-content" True ("icon-Question", "dark") L)
                    "category"
                    "question"
                    ""
                    "ask a question"
                    (\_ _ -> [])
                  )
      iconButton_ (IconButtonProps
                    (IconProps "c-vdoc-overlay-content" True ("icon-Remark", "dark") L)
                    "category"
                    "comment"
                    ""
                    "add a note"
                    (\_ _ -> [])
                  )
      iconButton_ (IconButtonProps
                    (IconProps "c-vdoc-overlay-content" True ("icon-Discussion", "dark") L)
                    "category"
                    "discussion"
                    ""
                    "start a discussion"
                    (\_ _ -> [])
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
        (\_ _ -> RS.dispatch (RS.SubmitComment (curState ^. RS.commentInputStateText) (curState ^. RS.commentInputStateCategory) forRange)
              <> RS.dispatch RS.HideCommentEditor)
      )

commentInput_ :: Maybe RS.Range -> ReactElementM eventHandler ()
commentInput_  forRange = view commentInput forRange mempty
