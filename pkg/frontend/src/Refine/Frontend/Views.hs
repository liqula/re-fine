{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Refine.Frontend.Views
  ( refineApp
  ) where

import           Control.Lens ((^.))
import           Data.Int
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.String.Conversions
import           Data.String (fromString)
import qualified Data.Tree as DT
import           React.Flux
import qualified Text.HTML.Parser as HTMLP
import           Text.HTML.Tree as HTMLT

import           Refine.Common.Types
import           Refine.Common.VDoc.HTML.Enhance (addUIInfoToForest)
import           Refine.Frontend.Bubbles.Overlay
import           Refine.Frontend.Bubbles.QuickCreate
import           Refine.Frontend.Heading ( documentHeader_, DocumentHeaderProps(..), editToolbar_
                                         , editToolbarExtension_, menuButton_, headerSizeCapture_
                                         )
import           Refine.Frontend.Loader.Component (vdocLoader_)
import           Refine.Frontend.Mark
import           Refine.Frontend.ThirdPartyViews (sticky_, stickyContainer_)
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Style
import           Refine.Frontend.Types as RS
import           Refine.Frontend.Bubbles.Types as RS
import           Refine.Frontend.UtilityWidgets
import           Refine.Frontend.WindowSize (windowSize_, WindowSizeProps(..))


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: ReactView ()
refineApp = defineControllerView "RefineApp" RS.refineStore $ \rs () ->
    case rs ^. gsVDoc of
        Nothing -> vdocLoader_ (rs ^. gsVDocList)
        Just vdoc -> div_ $ do
            windowSize_ (WindowSizeProps (rs ^. gsWindowSize)) mempty
            stickyContainer_ [] $ do
                headerSizeCapture_ $ do
                    -- the following need to be siblings because of the z-index handling
                    div_ ["className" $= "c-mainmenu__bg"] "" -- "role" $= "navigation"
                    --header_ ["role" $= "banner"] $ do
                    menuButton_
                    documentHeader_ $ DocumentHeaderProps (vdoc ^. compositeVDoc . vdocTitle) (vdoc ^. compositeVDoc . vdocAbstract)
                    div_ ["className" $= "c-fulltoolbar"] $ do
                        sticky_ [] $ do
                            editToolbar_
                            editToolbarExtension_

                showComment_ (rs ^. gsBubblesState ^. bsCommentIsVisible)
                addComment_ (rs ^. gsBubblesState ^. bsCommentEditorIsVisible) (rs ^. gsBubblesState ^. bsCommentCategory)

                main_ ["role" $= "main"] $ do
                    div_ ["className" $= "grid-wrapper"] $ do
                        div_ ["className" $= "row row-align-center row-align-top"] $ do
                            leftAside_ $ LeftAsideProps
                                           (rs ^. gsMarkPositions)
                                           (rs ^. gsBubblesState ^. bsCurrentSelection)
                                           (rs ^. gsHeaderHeight)
                                           (vdoc ^. compositeVDocDiscussions)
                                           (vdoc ^. compositeVDocNotes)
                            article_ [ "id" $= "vdocValue"
                                     , "className" $= "gr-20 gr-14@desktop"
                                     , onMouseUp $ \_ me -> RS.dispatch . RS.TriggerUpdateSelection $ mouseClientY me
                                     , onTouchEnd $ \_ te -> RS.dispatch . RS.TriggerUpdateSelection . touchScreenY . head $ touches te
                                     ] $ do
                              -- div_ ["className" $= "c-vdoc-overlay"] $ do
                                -- div_ ["className" $= "c-vdoc-overlay__inner"] $ do
                              div_ ["className" $= "c-article-content"] $ do
                                (toArticleBody . HTMLT.tokensToForest . HTMLP.parseTokens . cs . _unVDocVersion $ _compositeVDocVersion vdoc) vdoc
                            rightAside_ (rs ^. gsMarkPositions)


toArticleBody :: Either ParseTokenForestError (DT.Forest HTMLP.Token) -> CompositeVDoc -> ReactElementM [SomeStoreAction] ()
toArticleBody (Left err) _ = p_ (elemString (show err))
toArticleBody (Right forest) vdoc = mconcat $ map (toHTML vdoc) (addUIInfoToForest forest)


toHTML :: CompositeVDoc -> DT.Tree HTMLP.Token -> ReactElementM [SomeStoreAction] ()
-- br and hr need to be handled differently
toHTML _ (DT.Node (HTMLP.TagOpen "br" attrs) []) = br_ (toProps attrs)
toHTML _ (DT.Node (HTMLP.TagOpen "hr" attrs) []) = hr_ (toProps attrs)
-- just a node without children, containing some text:
toHTML _ (DT.Node (HTMLP.ContentText content) []) = elemText content
toHTML _ (DT.Node (HTMLP.ContentChar content) []) = elemText $ cs [content]
-- a comment - do we want to support them, given our HTML editor provides no means of entering them?
toHTML _ (DT.Node (HTMLP.Comment _) _) = mempty -- ignore comments
toHTML vdoc (DT.Node (HTMLP.TagOpen "mark" attrs) subForest) =
    rfMark_ (toMarkProps attrs vdoc) $ (toHTML vdoc) `mapM_` subForest -- (toProps attrs)
toHTML vdoc (DT.Node (HTMLP.TagOpen tagname attrs) subForest) =
    React.Flux.term (fromString (cs tagname)) (toProps attrs) $ (toHTML vdoc) `mapM_` subForest

-- the above cases cover all possibilities in the demo article, but we leave this here for discovery:
toHTML _ (DT.Node rootLabel []) = p_ (elemString ("root_label_wo_children " <> show rootLabel))
toHTML vdoc (DT.Node rootLabel subForest) = do
    p_ $ do
      elemString ("root_label " <> show rootLabel)
      p_ "subforest_start"
      (toHTML vdoc) `mapM_` subForest
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


data BubbleProps = BubbleProps
  { _dataHunkId2 :: Int64
  , _dataContentType2 :: String
  , _iconSide :: String
  , _iconStyle :: IconDescription
  , _markPosition :: Maybe Int
  }

bubble :: ReactView BubbleProps
bubble = defineView "Bubble" $ \props ->
        case _markPosition props of
            Nothing -> div_ ""
            Just pos ->
                div_ ["data-chunk-id" $= fromString (show (_dataHunkId2 props))
                    , "data-content-type" $= fromString (_dataContentType2 props)
                    , "className" $= fromString ("o-snippet o-snippet--" <> _dataContentType2 props)  -- RENAME: snippet => bubble
                    , "style" @= [Style "top" pos]
                    ] $ do
                    div_ ["className" $= fromString ("o-snippet__icon-bg o-snippet__icon-bg--" <> _iconSide props)] $ do  -- RENAME: snippet => bubble
                        icon_ (IconProps "o-snippet" False (_iconStyle props) M)  -- RENAME: snippet => bubble
                    div_ ["className" $= "o-snippet__content"] childrenPassedToView  -- RENAME: snippet => bubble

bubble_ :: BubbleProps -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
bubble_ = view bubble


discussionBubble :: ReactView (Int64, Maybe Int)
discussionBubble = defineView "DiscussionBubble" $ \(dataHunkId, markPosition) ->
    bubble_ (BubbleProps dataHunkId "discussion" "left" ("icon-Discussion", "bright") markPosition) childrenPassedToView

discussionBubble_ :: Int64 -> Maybe Int -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
discussionBubble_ dataHunkId markPosition = view discussionBubble (dataHunkId, markPosition)

questionBubble :: ReactView (Int64, RS.MarkPositions)
questionBubble = defineView "QuestionBubble" $ \(dataHunkId, RS.MarkPositions markPositions) ->
    bubble_ (BubbleProps dataHunkId "question" "left" ("icon-Question", "dark") (M.lookup dataHunkId markPositions)) childrenPassedToView

questionBubble_ :: Int64 -> RS.MarkPositions -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
questionBubble_ dataHunkId markPositions = view questionBubble (dataHunkId, markPositions)

noteBubble :: ReactView (Int64, Maybe Int)
noteBubble = defineView "NoteBubble" $ \(dataHunkId, markPosition) ->
    bubble_ (BubbleProps dataHunkId "question" "left" ("icon-Question", "dark") markPosition) childrenPassedToView

noteBubble_ :: Int64 -> Maybe Int -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
noteBubble_ dataHunkId markPosition = view noteBubble (dataHunkId, markPosition)

editBubble :: ReactView (Int64, RS.MarkPositions)
editBubble = defineView "EditBubble" $ \(dataHunkId, RS.MarkPositions markPositions) ->
    bubble_ (BubbleProps dataHunkId "edit" "right" ("icon-Edit", "dark") (M.lookup dataHunkId markPositions)) childrenPassedToView

editBubble_ :: Int64 -> RS.MarkPositions -> ReactElementM eventHandler () -> ReactElementM eventHandler ()
editBubble_ dataHunkId markPositions = view editBubble (dataHunkId, markPositions)


data LeftAsideProps = LeftAsideProps
  { _leftAsideMarkPositions :: RS.MarkPositions
  , _leftAsideCurrentSelection :: Selection
  , _leftAsideHeaderHeight :: Int
  , _leftAsideDiscussions :: [CompositeDiscussion]
  , _leftAsideNotes :: [Note]
  }

leftAside :: ReactView LeftAsideProps
leftAside = defineView "LeftAside" $ \props ->
    aside_ ["className" $= "sidebar sidebar-annotations gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: annotation => comment
        let lookupPosition = \chunkId -> M.lookup chunkId . _unMarkPositions $ _leftAsideMarkPositions props
        -- TODO the map should use proper IDs as keys
        mconcat $ map (\d -> discussionBubble_ (d ^. compositeDiscussion ^. discussionID ^. unID)
                                               (lookupPosition (d ^. compositeDiscussion ^. discussionID ^. unID))
                                               (elemText (head (d ^. compositeDiscussionTree) ^. statementText))) -- we always have one stmt
                      (_leftAsideDiscussions props)
        mconcat $ map (\n -> noteBubble_ (n ^. noteID ^. unID)
                                         (lookupPosition (n ^. noteID ^. unID))
                                         (elemText (n ^. noteText)))
                      (_leftAsideNotes props)

        questionBubble_ 3 (_leftAsideMarkPositions props) $ do
            span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."
        quickCreate_ "annotation" (_leftAsideCurrentSelection props) (_leftAsideHeaderHeight props)  -- RENAME: annotation => comment


leftAside_ :: LeftAsideProps -> ReactElementM eventHandler ()
leftAside_ props = view leftAside props mempty


rightAside :: ReactView RS.MarkPositions
rightAside = defineView "RightAside" $ \markPositions ->
    aside_ ["className" $= "sidebar sidebar-modifications gr-2 gr-5@desktop hide@mobile"] $ do -- RENAME: modifications => ??
            editBubble_ 2 markPositions $ do
                span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."

rightAside_ :: RS.MarkPositions -> ReactElementM eventHandler ()
rightAside_ markPositions = view rightAside markPositions mempty
