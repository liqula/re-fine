{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.RefineViews (
    refineApp
  ) where

import           React.Flux
import           Data.Monoid ((<>))
import           Data.String (fromString)
--import           Data.JSString (JSString)
import qualified Data.Tree as DT
import           Data.String.Conversions
import qualified Data.Map.Strict as M
import qualified Text.HTML.Parser as HTMLP
import           Text.HTML.Tree as HTMLT

import qualified Refine.Frontend.RefineStore as RS
import           Refine.Frontend.Loader.Component (vdocLoader_)
import           Refine.Frontend.UtilityWidgets
import           Refine.Frontend.Heading (documentHeader_, DocumentHeaderProps(..), editToolbar_, editToolbarExtension_, menuButton_, headerSizeCapture_)
import           Refine.Frontend.StickyViews (sticky_, stickyContainer_)
import           Refine.Frontend.WindowSize (windowSize_, WindowSizeProps(..))
import           Refine.Frontend.Style

import           Refine.Common.Types
import           Refine.Common.Rest


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: ReactView ()
refineApp = defineControllerView "RefineApp" RS.refineStore $ \(RS.RefineState maybeVdoc maybeVdocList headerHeight markPositions windowSize currentSelection) () ->
    case maybeVdoc of
        Nothing -> vdocLoader_ maybeVdocList
        Just vdoc -> div_ $ do
            windowSize_ (WindowSizeProps windowSize) mempty
            stickyContainer_ [] $ do
                headerSizeCapture_ $ do
                    -- the following need to be siblings because of the z-index handling
                    div_ ["className" $= "c-mainmenu__bg"] "" -- "role" $= "navigation"
                    --header_ ["role" $= "banner"] $ do
                    menuButton_
                    documentHeader_ . DocumentHeaderProps . _vdocTitle $ _compositeVDoc vdoc
                    div_ ["className" $= "c-fulltoolbar"] $ do
                        sticky_ [] $ do
                            editToolbar_
                            editToolbarExtension_

                main_ ["role" $= "main"] $ do
                    div_ ["className" $= "grid-wrapper"] $ do
                        div_ ["className" $= "row row-align-center row-align-top"] $ do
                            leftAside_ markPositions currentSelection headerHeight
                            toArticle . HTMLT.tokensToForest . HTMLP.parseTokens . cs . _unVDocVersion $ _compositeVDocVersion vdoc
                            rightAside_ markPositions


toArticle :: Either ParseTokenForestError (DT.Forest HTMLP.Token) -> ReactElementM [SomeStoreAction] ()
toArticle (Left err) = p_ (elemString (show err))
toArticle (Right forest) = article_
                 [ "id" $= "vdocValue"
                 , "className" $= "gr-20 gr-14@desktop c-article-content"
                 , onMouseUp $ \_ me -> RS.dispatch . RS.SetSelection $ mouseClientY me
                 , onTouchEnd $ \_ te -> RS.dispatch . RS.SetSelection . touchScreenY . head $ touches te
                 ] (mconcat $ map toHTML forest)


toHTML :: DT.Tree HTMLP.Token -> ReactElementM [SomeStoreAction] ()
-- br and hr need to be handled differently
toHTML (DT.Node (HTMLP.TagOpen "br" attrs) []) = br_ (toProps attrs)
toHTML (DT.Node (HTMLP.TagOpen "hr" attrs) []) = hr_ (toProps attrs)
-- just a node without children, containing some text:
toHTML (DT.Node (HTMLP.ContentText content) []) = elemText content
toHTML (DT.Node (HTMLP.ContentChar content) []) = elemText $ cs [content]
-- a comment - do we want to support them, given our HTML editor provides no means of entering them?
toHTML (DT.Node (HTMLP.Comment _) _) = mempty -- ignore comments
toHTML (DT.Node (HTMLP.TagOpen label attrs) subForest) =
    React.Flux.term (fromString (cs label)) (toProps attrs) $ toHTML `mapM_` subForest

-- the above cases cover all possibilities in the demo article, but we leave this here for discovery:
toHTML (DT.Node rootLabel []) = p_ (elemString ("root_label_wo_children " <> show rootLabel))
toHTML (DT.Node rootLabel subForest) = do
    p_ $ do
      elemString ("root_label " <> show rootLabel)
      p_ "subforest_start"
      toHTML `mapM_` subForest
      p_ "subforest_end"

-- alternatively: (needs `import Text.Show.Pretty`, package pretty-show.)
-- toHTML n@(DT.Node rootLabel subForest) = pre_ $ ppShow n


toProps :: [HTMLP.Attr] -> [PropertyOrHandler [SomeStoreAction]]
toProps = mconcat . fmap go
  where
    go :: HTMLP.Attr -> [PropertyOrHandler [SomeStoreAction]]
    go (HTMLP.Attr name value) =
        (fromString (cs name) $= fromString (cs value)) :
        []


data SnippetProps = SnippetProps
  { _dataHunkId2 :: String
  , _dataContentType2 :: String
  , _iconSide :: String
  , _iconStyle :: String
  , _markPosition :: Maybe Int
  }

snippet :: ReactView SnippetProps
snippet = defineView "snippet" $ \props ->
        case _markPosition props of
            Nothing -> div_ ""
            Just pos ->
                div_ ["data-hunk-id" $= fromString (_dataHunkId2 props)
                    , "data-content-type" $= fromString (_dataContentType2 props)
                    , "className" $= fromString ("o-snippet o-snippet--" <> _dataContentType2 props)
                    -- works equally well, we are experimenting with which to use:
                    --, nestedProperty "style" ["top" @= pos]
                    , "style" @= [Style "top" pos]
                    ] $ do
                    div_ ["className" $= fromString ("o-snippet__icon-bg o-snippet__icon-bg--" <> _iconSide props)] $ do
                        icon_ (fromString ("o-snippet__icon icon-" <> _iconStyle props <> " iconsize-m"))
                    div_ ["className" $= "o-snippet__content"] childrenPassedToView

snippet_ :: SnippetProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
snippet_ = view snippet


discussionSnippet :: ReactView (String, RS.MarkPositions)
discussionSnippet = defineView "DiscussionSnippet" $ \(dataHunkId, RS.MarkPositions markPositions) ->
    snippet_ (SnippetProps dataHunkId "discussion" "left" "Discussion_bright" (M.lookup dataHunkId markPositions)) childrenPassedToView

discussionSnippet_ :: String -> RS.MarkPositions -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
discussionSnippet_ dataHunkId markPositions = view discussionSnippet (dataHunkId, markPositions)

questionSnippet :: ReactView (String, RS.MarkPositions)
questionSnippet = defineView "QuestionSnippet" $ \(dataHunkId, RS.MarkPositions markPositions) ->
    snippet_ (SnippetProps dataHunkId "question" "left" "Question_dark" (M.lookup dataHunkId markPositions)) childrenPassedToView

questionSnippet_ :: String -> RS.MarkPositions -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
questionSnippet_ dataHunkId markPositions = view questionSnippet (dataHunkId, markPositions)

editSnippet :: ReactView (String, RS.MarkPositions)
editSnippet = defineView "EditSnippet" $ \(dataHunkId, RS.MarkPositions markPositions) ->
    snippet_ (SnippetProps dataHunkId "edit" "right" "Edit_dark" (M.lookup dataHunkId markPositions)) childrenPassedToView

editSnippet_ :: String -> RS.MarkPositions -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editSnippet_ dataHunkId markPositions = view editSnippet (dataHunkId, markPositions)


leftAside :: ReactView (RS.MarkPositions, (Maybe RS.Range, Maybe RS.DeviceOffset), Int)
leftAside = defineView "LeftAside" $ \(markPositions, currentSelection, headerHeight) ->
    aside_ ["className" $= "sidebar sidebar-annotations gr-2 gr-5@desktop hide@mobile"] $ do
        discussionSnippet_ "1" markPositions $ do
            span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."
        questionSnippet_ "3" markPositions $ do
            span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."
        quickCreate_ "annotation" currentSelection headerHeight


leftAside_ :: RS.MarkPositions -> (Maybe RS.Range, Maybe RS.DeviceOffset) -> Int -> ReactElementM eventHandler ()
leftAside_ markPositions currentSelection headerHeight = view leftAside (markPositions, currentSelection, headerHeight) mempty


quickCreate :: ReactView (String, (Maybe RS.Range, Maybe RS.DeviceOffset), Int)
quickCreate = defineView "QuickCreateButton" $ \(createType, currentSelection, headerHeight) ->
    case currentSelection of
    -- TODO unify CSS class names with those used in iconButton_ !!
        (Just range, Just _deviceOffset) ->
          let topOfSelection = RS._top range + RS._scrollOffset range - headerHeight - 80 -- offset from menu bar
              idealCenter = (RS._bottom range - RS._top range) `div` 2 - 22
              usefulCenter = idealCenter -- if abs (deviceOffset - idealCenter) > 200 then deviceOffset else idealCenter
              offset = topOfSelection + usefulCenter
          in positionedIconButton_ (IconButtonProps ("o-add-" <> createType) "" (fromString createType) True ("icon-New_Comment", "bright") "" XXL) offset
        (_, _) -> div_ ""
--    // quickCreate annotation ui events
--    ann.addEventListener('mousedown', quickCreateOverlay);
--    Hammer.on(ann, 'tap', quickCreateOverlay);


-- "annotation", "modification"
quickCreate_ :: String -> (Maybe RS.Range, Maybe RS.DeviceOffset) -> Int -> ReactElementM eventHandler ()
quickCreate_ createType currentSelection headerHeight = view quickCreate (createType, currentSelection, headerHeight) mempty


rightAside :: ReactView RS.MarkPositions
rightAside = defineView "RightAside" $ \markPositions ->
    aside_ ["className" $= "sidebar sidebar-modifications gr-2 gr-5@desktop hide@mobile"] $ do
            editSnippet_ "2" markPositions $ do
                span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."

rightAside_ :: RS.MarkPositions -> ReactElementM eventHandler ()
rightAside_ markPositions = view rightAside markPositions mempty
