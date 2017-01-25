{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Overlay where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           React.Flux

import           Refine.Frontend.ThirdPartyViews (overlay_)
import qualified Refine.Frontend.Types as RS
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import           Refine.Frontend.UtilityWidgets


quickCreate :: ReactView (String, (Maybe RS.Range, Maybe RS.DeviceOffset), Int)
quickCreate = defineView "QuickCreateButton" $ \(createType, currentSelection, headerHeight) ->
    case currentSelection of
    -- TODO unify CSS class names with those used in iconButton_ !!
        (Just range, Just deviceOffset) ->
            let offset = quickCreateOffset range deviceOffset headerHeight
            in positionedIconButton_
              (IconButtonProps
                (IconProps ("o-add-" <> createType) True ("icon-New_Comment", "bright") XXL)
                ""
                ""
                (fromString createType)
                ""
                (\_ _ -> RS.dispatch RS.ClearSelection <> RS.dispatch RS.ShowCommentEditor)
              ) offset
        _ -> div_ ""
--    // quickCreate annotation ui events
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);


quickCreateOffset :: RS.Range -> Int -> Int -> Int
quickCreateOffset range deviceOffset headerHeight =
    quickCreateSelectionTop range headerHeight +
    quickCreateSelectionPos range deviceOffset


-- | This is the offset from the bottom of the toolbar.
quickCreateSelectionTop :: RS.Range -> Int -> Int
quickCreateSelectionTop range headerHeight = RS._top range + RS._scrollOffset range - headerHeight - 80

quickCreateSelectionPos :: RS.Range -> Int -> Int
quickCreateSelectionPos range deviceOffset =
    let selectionHeight = RS._bottom range - RS._top range
        idealCenter = selectionHeight `div` 2 - 22
        useIdealCenter = selectionHeight <= 200
        closerToTop = abs (deviceOffset - RS._top range) < idealCenter
        edgePosition = if closerToTop then 0 else selectionHeight - 44
    in if useIdealCenter then idealCenter else edgePosition

-- "annotation", "modification"
quickCreate_ :: String -> (Maybe RS.Range, Maybe RS.DeviceOffset) -> Int -> ReactElementM eventHandler ()
quickCreate_ createType currentSelection headerHeight = view quickCreate (createType, currentSelection, headerHeight) mempty

vdoc_overlay_content :: [Style]
vdoc_overlay_content = [ Style "display" ("block" :: String)
                       , Style "minHeight" ("200px" :: String)
                       , Style "padding" ("15rem 10rem 10rem" :: String)
                       ]

showComment :: ReactView Bool
showComment = defineView "ShowComment" $ \showOverlay ->
  let vdoc_overlay_content__comment = [ Style "backgroundColor" ("rgb(219, 204, 221)" :: String) -- lightred
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

        -- patch/comment/etc. user meta data -->
        div_ ["className" $= "c-vdoc-overlay-meta"] $ do
            span_ ["className" $= "c-vdoc-overlay-meta__user-avatar"] $ do
                icon_ (IconProps "c-vdoc-overlay-meta" False ("icon-User", "bright") M)
                {-
                div_ [className $= "c-vdoc-overlay-meta__icon icon-User_bright iconsize-m">
                    <span class="path1"></span><span class="path2"></span><span class="path3"></span><span class="path4"></span><span class="path5"></span><span class="path6"></span><span class="path7"></span><span class="path8"></span>
                -}
            span_ ["className" $= "c-vdoc-overlay-meta__user"] "meisterkaiser"
            span_ ["className" $= "c-vdoc-overlay-meta__date"] "24. 05. 2016"
        -- END: patch/comment/etc. user meta data -->

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
addComment :: ReactView Bool
addComment = defineView "AddComment" $ \showOverlay ->
  let vdoc_overlay_content__add_annotation = [ Style "backgroundColor" ("rgb(59, 67, 87)" :: String) -- std-icon-bg, blue02
                                     ]
  in overlay_ ["isVisible" &= showOverlay
           , on "onCloseClicked" $ \_ -> RS.dispatch RS.HideCommentEditor
           , "hideOnOverlayClicked" &= True
           , "dialogStyles" @= (vdoc_overlay_content <> vdoc_overlay_content__add_annotation)
           ]  $ do
              icon_ (IconProps "c-vdoc-overlay-content" False ("icon-Remark", "dark") XL)

              h4_ ["className" $= "c-vdoc-overlay-content__title"] "add a comment"

              form_ [ "target" $= "#"
                   , "action" $= "POST"] $ do
                textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"
                         , "className" $= "o-wysiwyg o-form-input__textarea"] mempty

              div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
                p_ $ do
                  elemString "Step 1:"
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
                  elemString "Step 2:"
                  span_ ["className" $= "bold"] "finish"

              iconButton_ (IconButtonProps
                            (IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") L)
                            "submit"
                            ""
                            ""
                            "submit"
                            (\_ _ -> [])
                          )


addComment_ :: Bool -> ReactElementM eventHandler ()
addComment_ showOverlay = view addComment showOverlay mempty
