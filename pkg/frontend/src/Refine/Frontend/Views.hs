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

import           Control.Lens ((^.), (^?))
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.String.Conversions
import qualified Data.Tree as DT
import           React.Flux
import qualified Text.HTML.Parser as HTMLP

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Mark
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.QuickCreate
import           Refine.Frontend.Contribution.Types as RS
import           Refine.Frontend.Header.Heading ( mainHeader_ )
import           Refine.Frontend.Header.Types as HT
import           Refine.Frontend.Loader.Component (vdocLoader_)
import           Refine.Frontend.Login.Types as LG
import           Refine.Frontend.MainMenu.Component (mainMenu_)
import           Refine.Frontend.MainMenu.Types (mainMenuOpenTab)
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
  case rs ^. gsVDoc of
    Nothing -> vdocLoader_ (rs ^. gsVDocList)  -- (this is just some scaffolding that will be replaced by more app once we get there.)
    Just _ -> case rs ^? gsMainMenuState . mainMenuOpenTab of
      Nothing  -> mainScreen_ rs
      Just tab -> mainMenu_ tab (rs ^. gsLoginState . lsCurrentUser)

mainScreen :: ReactView RS.GlobalState
mainScreen = defineView "MainScreen" $ \rs ->
  let vdoc = fromJust (rs ^. gsVDoc) -- FIXME: improve this!  (introduce a custom props type with a CompositeVDoc *not* wrapped in a 'Maybe')

  in div_ (case rs ^. gsHeaderState . hsToolbarExtensionStatus of
    HT.ToolbarExtensionClosed -> []
    _ -> [ onClick $ \_ _ -> RS.dispatch (RS.HeaderAction HT.CloseToolbarExtension)
         ]) $ do
      windowSize_ (WindowSizeProps (rs ^. gsScreenState . SC.ssWindowSize)) mempty
      stickyContainer_ [] $ do
          mainHeader_ rs

          -- components that are only temporarily visible:
          showNote_ $ showNoteProps (vdoc ^. compositeVDocNotes) rs
          showDiscussion_ $ showDiscussionProps (vdoc ^. compositeVDocDiscussions) rs
          addComment_ $ AddCommentProps (rs ^. RS.gsContributionState . RS.csCommentEditorIsVisible)
                                        (rs ^. RS.gsContributionState . RS.csCommentCategory)
          notImplementedYet_ (rs ^. gsNotImplementedYetIsVisible)

          main_ ["role" $= "main"] $ do
              div_ ["className" $= "grid-wrapper"] $ do
                  div_ ["className" $= "row row-align-center row-align-top"] $ do
                      let toolbarStatus = rs ^. gsHeaderState . hsToolbarExtensionStatus
                      leftAside_ $ LeftAsideProps
                                     (rs ^. gsContributionState . csMarkPositions)
                                     (rs ^. gsContributionState . csCurrentSelection)
                                     (rs ^. gsContributionState . csHighlightedMarkAndBubble)
                                     (rs ^. gsScreenState)
                                     (M.elems (vdoc ^. compositeVDocDiscussions))
                                     (M.elems (vdoc ^. compositeVDocNotes))
                                     toolbarStatus
                      article_ [ "id" $= "vdocValue"
                               , "className" $= "gr-20 gr-14@desktop"
                               , onMouseUp  $ \_ me -> RS.dispatch $
                                   RS.TriggerUpdateSelection (SC.OffsetFromDocumentTop $ mousePageY me) toolbarStatus
                                     -- <-- relative to webpage | relative to viewport -> mouseClientY me
                               , onTouchEnd $ \_ te -> RS.dispatch $
                                   RS.TriggerUpdateSelection (SC.OffsetFromDocumentTop . touchPageY . head $ touches te) toolbarStatus

                               ] $ do
                        -- leftover from p'2016:
                        -- div_ ["className" $= "c-vdoc-overlay"] $ do
                          -- div_ ["className" $= "c-vdoc-overlay__inner"] $ do
                        div_ ["className" $= "c-article-content"] $ do
                          toArticleBody (rs ^. gsContributionState) (_unVDocVersion . _compositeVDocVersion $ vdoc)
                      rightAside_ (rs ^. gsContributionState . csMarkPositions) (rs ^. gsScreenState)

mainScreen_ :: RS.GlobalState -> ReactElementM eventHandler ()
mainScreen_ rs = view mainScreen rs mempty


toArticleBody :: ContributionState -> DT.Forest HTMLP.Token -> ReactElementM [SomeStoreAction] ()
toArticleBody state forest = mconcat $ map (toHTML state) forest


toHTML :: ContributionState -> DT.Tree HTMLP.Token -> ReactElementM [SomeStoreAction] ()
-- br and hr need to be handled differently
toHTML _ (DT.Node (HTMLP.TagSelfClose "br" attrs) []) = br_ (toProperties attrs)
toHTML _ (DT.Node (HTMLP.TagSelfClose "hr" attrs) []) = hr_ (toProperties attrs)
-- just a node without children, containing some text:
toHTML _ (DT.Node (HTMLP.ContentText content) []) = elemText content
toHTML _ (DT.Node (HTMLP.ContentChar content) []) = elemText $ cs [content]
-- a comment - do we want to support them, given our HTML editor provides no means of entering them?
toHTML _ (DT.Node (HTMLP.Comment _) _) = mempty -- ignore comments
toHTML state (DT.Node (HTMLP.TagOpen "mark" attrs) subForest) =
    rfMark_ (MarkProps attrs (state ^. csHighlightedMarkAndBubble)) $ toHTML state `mapM_` subForest -- (toProperties attrs)
toHTML state (DT.Node (HTMLP.TagOpen tagname attrs) subForest) =
    React.Flux.term (cs tagname) (toProperties attrs) $ toHTML state `mapM_` subForest
toHTML _ (DT.Node (HTMLP.TagSelfClose tagname attrs) []) =
    React.Flux.term (cs tagname) (toProperties attrs) mempty

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
  { _leftAsideMarkPositions     :: RS.MarkPositions
  , _leftAsideCurrentSelection  :: RS.Selection
  , _leftAsideHighlightedBubble :: Maybe ContributionID
  , _leftAsideScreenState       :: SC.ScreenState
  , _leftAsideDiscussions       :: [CompositeDiscussion]
  , _leftAsideNotes             :: [Note]
  , _leftAsideQuickCreateInfo   :: ToolbarExtensionStatus
  }

leftAside :: ReactView LeftAsideProps
leftAside = defineView "LeftAside" $ \props ->
    aside_ ["className" $= "sidebar sidebar-annotations gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: annotation => comment
        let lookupPosition chunkId = M.lookup chunkId . _unMarkPositions $ _leftAsideMarkPositions props
        -- TODO the map should use proper IDs as keys
        mconcat $ map (\d -> discussionBubble_ (SpecialBubbleProps
                                                 (ContribIDDiscussion (d ^. compositeDiscussion . discussionID))
                                                 (lookupPosition $ ContribIDDiscussion (d ^. compositeDiscussion . discussionID))
                                                 (_leftAsideHighlightedBubble props)
                                                 (_leftAsideScreenState props)
                                               )
                                               (elemText (DT.rootLabel (d ^. compositeDiscussionTree) ^. statementText))) -- we always have one stmt
                      (_leftAsideDiscussions props)
        mconcat $ map (\n -> noteBubble_ (SpecialBubbleProps
                                           (ContribIDNote (n ^. noteID))
                                           (lookupPosition $ ContribIDNote (n ^. noteID))
                                           (_leftAsideHighlightedBubble props)
                                           (_leftAsideScreenState props)
                                         )
                                         (elemText (n ^. noteText)))
                      (_leftAsideNotes props)
{- TODO: later
        questionBubble_ (SpecialBubbleProps 3 (_leftAsideMarkPositions props) (_leftAsideScreenState props)) $ do
            span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."
-}
        quickCreate_ $ QuickCreateProps "annotation" (_leftAsideCurrentSelection props) (_leftAsideScreenState props) (_leftAsideQuickCreateInfo props)  -- RENAME: annotation => comment


leftAside_ :: LeftAsideProps -> ReactElementM eventHandler ()
leftAside_ props = view leftAside props mempty


rightAside :: ReactView (RS.MarkPositions, SC.ScreenState)
rightAside = defineView "RightAside" $ \(_markPositions, _screenState) ->
    aside_ ["className" $= "sidebar sidebar-modifications gr-2 gr-5@desktop hide@mobile"] $ do -- RENAME: modifications => ??
      mempty
    {- TODO: later
            editBubble_ 2 markPositions screenState $ do
                span_ "Ut wis is enim ad minim veniam, quis nostrud exerci tution ullam corper suscipit lobortis nisi ut aliquip ex ea commodo consequat. Duis te feugi facilisi. Duis autem dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit au gue duis dolore te feugat nulla facilisi."
    -}

rightAside_ :: RS.MarkPositions -> SC.ScreenState -> ReactElementM eventHandler ()
rightAside_ markPositions screenState = view rightAside (markPositions, screenState) mempty
