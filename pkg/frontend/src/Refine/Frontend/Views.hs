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

module Refine.Frontend.Views
  ( refineApp
  ) where

import           Control.Lens ((^.))
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.String.Conversions
import           Data.String (fromString)
import qualified Data.Tree as DT
import           React.Flux
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Prelude (ClearTypeParameter(..))
import           Refine.Common.VDoc.HTML.Enhance (addUIInfoToForest)
import           Refine.Frontend.Bubbles.Bubble
import           Refine.Frontend.Bubbles.Mark
import           Refine.Frontend.Bubbles.Overlay
import           Refine.Frontend.Bubbles.QuickCreate
import           Refine.Frontend.Bubbles.Types as RS
import           Refine.Frontend.Heading ( documentHeader_, DocumentHeaderProps(..), editToolbar_
                                         , editToolbarExtension_, menuButton_, headerSizeCapture_
                                         )
import           Refine.Frontend.Loader.Component (vdocLoader_)
import           Refine.Frontend.ThirdPartyViews (sticky_, stickyContainer_)
import           Refine.Frontend.Screen.WindowSize (windowSize_, WindowSizeProps(..))
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Types as RS


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: ReactView ()
refineApp = defineControllerView "RefineApp" RS.refineStore $ \rs () ->
    case rs ^. gsVDoc of
        Nothing -> vdocLoader_ (rs ^. gsVDocList)
        Just vdoc -> div_ $ do
            windowSize_ (WindowSizeProps (rs ^. gsScreenState ^. SC.ssWindowSize)) mempty
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

                showNote_ $ (`M.lookup` (vdoc ^. compositeVDocNotes)) =<< (rs ^. gsBubblesState ^. bsNoteIsVisible)
                showDiscussion_ $ (`M.lookup` (vdoc ^. compositeVDocDiscussions)) =<< (rs ^. gsBubblesState ^. bsDiscussionIsVisible)
                addComment_ (rs ^. gsBubblesState ^. bsCommentEditorIsVisible) (rs ^. gsBubblesState ^. bsCommentCategory)

                main_ ["role" $= "main"] $ do
                    div_ ["className" $= "grid-wrapper"] $ do
                        div_ ["className" $= "row row-align-center row-align-top"] $ do
                            leftAside_ $ LeftAsideProps
                                           (rs ^. gsBubblesState ^. bsMarkPositions)
                                           (rs ^. gsBubblesState ^. bsCurrentSelection)
                                           (rs ^. gsScreenState)
                                           (M.elems (vdoc ^. compositeVDocDiscussions))
                                           (M.elems (vdoc ^. compositeVDocNotes))
                            article_ [ "id" $= "vdocValue"
                                     , "className" $= "gr-20 gr-14@desktop"
                                     , onMouseUp $ \_ me -> RS.dispatch . RS.TriggerUpdateSelection $ mouseClientY me
                                     , onTouchEnd $ \_ te -> RS.dispatch . RS.TriggerUpdateSelection . touchScreenY . head $ touches te
                                     ] $ do
                              -- div_ ["className" $= "c-vdoc-overlay"] $ do
                                -- div_ ["className" $= "c-vdoc-overlay__inner"] $ do
                              div_ ["className" $= "c-article-content"] $ do
                                toArticleBody . _unVDocVersion . _compositeVDocVersion $ vdoc
                            rightAside_ (rs ^. gsBubblesState ^. bsMarkPositions) (rs ^. gsScreenState)


toArticleBody :: DT.Forest HTMLP.Token -> ReactElementM [SomeStoreAction] ()
toArticleBody forest = mconcat $ map toHTML (addUIInfoToForest forest)


toHTML :: DT.Tree HTMLP.Token -> ReactElementM [SomeStoreAction] ()
-- br and hr need to be handled differently
toHTML (DT.Node (HTMLP.TagSelfClose "br" attrs) []) = br_ (toProps attrs)
toHTML (DT.Node (HTMLP.TagSelfClose "hr" attrs) []) = hr_ (toProps attrs)
-- just a node without children, containing some text:
toHTML (DT.Node (HTMLP.ContentText content) []) = elemText content
toHTML (DT.Node (HTMLP.ContentChar content) []) = elemText $ cs [content]
-- a comment - do we want to support them, given our HTML editor provides no means of entering them?
toHTML (DT.Node (HTMLP.Comment _) _) = mempty -- ignore comments
toHTML (DT.Node (HTMLP.TagOpen "mark" attrs) subForest) =
    rfMark_ (toMarkProps attrs) $ toHTML `mapM_` subForest -- (toProps attrs)
toHTML (DT.Node (HTMLP.TagOpen tagname attrs) subForest) =
    React.Flux.term (fromString (cs tagname)) (toProps attrs) $ toHTML `mapM_` subForest
toHTML (DT.Node (HTMLP.TagSelfClose tagname attrs) []) =
    React.Flux.term (fromString (cs tagname)) (toProps attrs) mempty

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


data LeftAsideProps = LeftAsideProps
  { _leftAsideMarkPositions :: RS.MarkPositions
  , _leftAsideCurrentSelection :: RS.Selection
  , _leftAsideScreenState :: SC.ScreenState
  , _leftAsideDiscussions :: [CompositeDiscussion]
  , _leftAsideNotes :: [Note]
  }

leftAside :: ReactView LeftAsideProps
leftAside = defineView "LeftAside" $ \props ->
    aside_ ["className" $= "sidebar sidebar-annotations gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: annotation => comment
        let lookupPosition chunkId = M.lookup chunkId . _unMarkPositions $ _leftAsideMarkPositions props
        -- TODO the map should use proper IDs as keys
        mconcat $ map (\d -> discussionBubble_ (SpecialBubbleProps
                                                 (d ^. compositeDiscussion ^. discussionID ^. unID)
                                                 (lookupPosition $ clearTypeParameter (d ^. compositeDiscussion ^. discussionID))
                                                 (_leftAsideScreenState props)
                                               )
                                               (elemText (DT.rootLabel (d ^. compositeDiscussionTree) ^. statementText))) -- we always have one stmt
                      (_leftAsideDiscussions props)
        mconcat $ map (\n -> noteBubble_ (SpecialBubbleProps
                                           (n ^. noteID ^. unID)
                                           (lookupPosition $ clearTypeParameter (n ^. noteID))
                                           (_leftAsideScreenState props)
                                         )
                                         (elemText (n ^. noteText)))
                      (_leftAsideNotes props)
{-
        questionBubble_ (SpecialBubbleProps 3 (_leftAsideMarkPositions props) (_leftAsideScreenState props)) $ do
            span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."
-}
        quickCreate_ "annotation" (_leftAsideCurrentSelection props) (_leftAsideScreenState props)  -- RENAME: annotation => comment


leftAside_ :: LeftAsideProps -> ReactElementM eventHandler ()
leftAside_ props = view leftAside props mempty


rightAside :: ReactView (RS.MarkPositions, SC.ScreenState)
rightAside = defineView "RightAside" $ \(_markPositions, _screenState) ->
    aside_ ["className" $= "sidebar sidebar-modifications gr-2 gr-5@desktop hide@mobile"] $ do -- RENAME: modifications => ??
      mempty
    {-
            editBubble_ 2 markPositions screenState $ do
                span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."
    -}

rightAside_ :: RS.MarkPositions -> SC.ScreenState -> ReactElementM eventHandler ()
rightAside_ markPositions screenState = view rightAside (markPositions, screenState) mempty
