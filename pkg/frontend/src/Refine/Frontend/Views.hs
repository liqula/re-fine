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

  -- for testing:
  , mainScreen_
  ) where

import           Control.Lens ((^.))
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.String.Conversions
import           Data.String (fromString)
import qualified Data.Tree as DT
import           Data.Void
import           React.Flux
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Prelude (ClearTypeParameter(..))
import           Refine.Frontend.Bubbles.Bubble
import           Refine.Frontend.Bubbles.Mark
import           Refine.Frontend.Bubbles.Overlay
import           Refine.Frontend.Bubbles.QuickCreate
import           Refine.Frontend.Bubbles.Types as RS
import           Refine.Frontend.Header.Heading ( mainHeader_ )
import           Refine.Frontend.Header.Types as HT
import           Refine.Frontend.Loader.Component (vdocLoader_)
import           Refine.Frontend.Login.Component (login_)
import           Refine.Frontend.MainMenu (mainMenu_)
import           Refine.Frontend.NotImplementedYet (notImplementedYet_)
import           Refine.Frontend.ThirdPartyViews (stickyContainer_)
import           Refine.Frontend.Screen.WindowSize (windowSize_, WindowSizeProps(..))
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Types as RS


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: ReactView ()
refineApp = defineControllerView "RefineApp" RS.refineStore $ \rs () ->
  if rs ^. gsMainMenuOpen
    then mainMenu_
    else refineAppMenuClosed_ rs

refineAppMenuClosed_ :: RS.GlobalState -> ReactElementM ViewEventHandler ()
refineAppMenuClosed_ rs =
    case rs ^. gsVDoc of
        Nothing -> do
          login_
          vdocLoader_ (rs ^. gsVDocList)
        Just _ -> mainScreen_ rs

mainScreen :: ReactView RS.GlobalState
mainScreen = defineView "MainScreen" $ \rs ->
  let vdoc = fromJust (rs ^. gsVDoc) -- TODO improve this!
  in div_
    [ onClick $ \_ _ -> RS.dispatch (RS.HeaderAction HT.CloseCommentToolbarExtension)
    ] $ do
      windowSize_ (WindowSizeProps (rs ^. gsScreenState . SC.ssWindowSize)) mempty
      stickyContainer_ [] $ do
          mainHeader_ rs

          -- components that are only temporarily visible:
          showNote_ $ (`M.lookup` (vdoc ^. compositeVDocNotes)) =<< (rs ^. gsBubblesState . bsNoteIsVisible)
          showDiscussion_ $ (`M.lookup` (vdoc ^. compositeVDocDiscussions)) =<< (rs ^. gsBubblesState . bsDiscussionIsVisible)
          addComment_ (rs ^. gsBubblesState . bsCommentEditorIsVisible) (rs ^. gsBubblesState . bsCommentCategory)
          notImplementedYet_ (rs ^. gsNotImplementedYetIsVisible)

          main_ ["role" $= "main"] $ do
              div_ ["className" $= "grid-wrapper"] $ do
                  div_ ["className" $= "row row-align-center row-align-top"] $ do
                      leftAside_ $ LeftAsideProps
                                     (rs ^. gsBubblesState . bsMarkPositions)
                                     (rs ^. gsBubblesState . bsCurrentSelection)
                                     (rs ^. gsBubblesState . bsHighlightedMarkAndBubble)
                                     (rs ^. gsScreenState)
                                     (M.elems (vdoc ^. compositeVDocDiscussions))
                                     (M.elems (vdoc ^. compositeVDocNotes))
                      article_ [ "id" $= "vdocValue"
                               , "className" $= "gr-20 gr-14@desktop"
                               , onMouseUp $ \_ me -> RS.dispatch . RS.TriggerUpdateSelection $ mouseClientY me
                               , onTouchEnd $ \_ te -> RS.dispatch . RS.TriggerUpdateSelection . touchScreenY . head $ touches te
                               ] $ do
                        -- leftover from p'2016:
                        -- div_ ["className" $= "c-vdoc-overlay"] $ do
                          -- div_ ["className" $= "c-vdoc-overlay__inner"] $ do
                        div_ ["className" $= "c-article-content"] $ do
                          toArticleBody (rs ^. gsBubblesState) (_unVDocVersion . _compositeVDocVersion $ vdoc)
                      rightAside_ (rs ^. gsBubblesState . bsMarkPositions) (rs ^. gsScreenState)

mainScreen_ :: RS.GlobalState -> ReactElementM eventHandler ()
mainScreen_ rs = view mainScreen rs mempty


toArticleBody :: BubblesState -> DT.Forest HTMLP.Token -> ReactElementM [SomeStoreAction] ()
toArticleBody state forest = mconcat $ map (toHTML state) forest


toHTML :: BubblesState -> DT.Tree HTMLP.Token -> ReactElementM [SomeStoreAction] ()
-- br and hr need to be handled differently
toHTML _ (DT.Node (HTMLP.TagSelfClose "br" attrs) []) = br_ (toProperties attrs)
toHTML _ (DT.Node (HTMLP.TagSelfClose "hr" attrs) []) = hr_ (toProperties attrs)
-- just a node without children, containing some text:
toHTML _ (DT.Node (HTMLP.ContentText content) []) = elemText content
toHTML _ (DT.Node (HTMLP.ContentChar content) []) = elemText $ cs [content]
-- a comment - do we want to support them, given our HTML editor provides no means of entering them?
toHTML _ (DT.Node (HTMLP.Comment _) _) = mempty -- ignore comments
toHTML state (DT.Node (HTMLP.TagOpen "mark" attrs) subForest) =
    rfMark_ (MarkProps attrs (state ^. bsHighlightedMarkAndBubble)) $ toHTML state `mapM_` subForest -- (toProperties attrs)
toHTML state (DT.Node (HTMLP.TagOpen tagname attrs) subForest) =
    React.Flux.term (fromString (cs tagname)) (toProperties attrs) $ toHTML state `mapM_` subForest
toHTML _ (DT.Node (HTMLP.TagSelfClose tagname attrs) []) =
    React.Flux.term (fromString (cs tagname)) (toProperties attrs) mempty

-- the above cases cover all possibilities in the demo article, but we leave this here for discovery:
toHTML _ (DT.Node rootLabel []) = p_ (elemString ("root_label_wo_children " <> show rootLabel))
toHTML state (DT.Node rootLabel subForest) = do
    p_ $ do
      elemString ("root_label " <> show rootLabel)
      p_ "subforest_start"
      toHTML state `mapM_` subForest
      p_ "subforest_end"

-- alternatively: (needs `import Text.Show.Pretty`, package pretty-show.)
-- toHTML n@(DT.Node rootLabel subForest) = pre_ $ ppShow n


data LeftAsideProps = LeftAsideProps
  { _leftAsideMarkPositions :: RS.MarkPositions
  , _leftAsideCurrentSelection :: RS.Selection
  , _leftAsideHighlightedBubble :: Maybe (ID Void)
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
                                                 (clearTypeParameter (d ^. compositeDiscussion . discussionID))
                                                 (lookupPosition $ clearTypeParameter (d ^. compositeDiscussion . discussionID))
                                                 (_leftAsideHighlightedBubble props)
                                                 (_leftAsideScreenState props)
                                               )
                                               (elemText (DT.rootLabel (d ^. compositeDiscussionTree) ^. statementText))) -- we always have one stmt
                      (_leftAsideDiscussions props)
        mconcat $ map (\n -> noteBubble_ (SpecialBubbleProps
                                           (clearTypeParameter (n ^. noteID))
                                           (lookupPosition $ clearTypeParameter (n ^. noteID))
                                           (_leftAsideHighlightedBubble props)
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
