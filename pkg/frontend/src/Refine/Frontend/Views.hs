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

module Refine.Frontend.Views
  ( refineApp

  -- for testing:
  , mainScreen_
  ) where

import           Control.Lens ((^.), (^?), to)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.String.Conversions
import qualified Data.Tree as DT
import           React.Flux

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.QuickCreate
import           Refine.Frontend.Contribution.Types as RS
import           Refine.Frontend.Document.Types (DocumentProps(..))
import           Refine.Frontend.Document.Document (document_)
import           Refine.Frontend.Header.Heading ( mainHeader_ )
import           Refine.Frontend.Header.Types as HT
import           Refine.Frontend.Loader.Component (vdocLoader_)
import           Refine.Frontend.Login.Types as LG
import           Refine.Frontend.MainMenu.Component (mainMenu_)
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.NotImplementedYet (notImplementedYet_)
import           Refine.Frontend.ThirdPartyViews (stickyContainer_)
import           Refine.Frontend.Screen.WindowSize (windowSize_, WindowSizeProps(..))
import qualified Refine.Frontend.Screen.Types as SC
import qualified Refine.Frontend.Store as RS
import           Refine.Frontend.Types as RS
import           Refine.Prelude.Aeson (unNoJSONRep)


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: ReactView ()
refineApp = defineControllerView "RefineApp" RS.refineStore $ \rs () ->
  case rs ^. gsVDoc of
    Nothing -> vdocLoader_ (rs ^. gsVDocList)  -- (this is just some scaffolding that will be replaced by more app once we get there.)
    Just _ -> case rs ^? gsMainMenuState . mmState . mainMenuOpenTab of
      Nothing  -> mainScreen_ rs
      Just tab -> mainMenu_ tab
                            (rs ^. gsMainMenuState . mmErrors)
                            (rs ^. gsLoginState . lsCurrentUser)

mainScreen :: ReactView RS.GlobalState
mainScreen = defineView "MainScreen" $ \rs -> do
  let vdoc = fromJust (rs ^. gsVDoc) -- FIXME: improve this!  (introduce a custom props type with a CompositeVDoc *not* wrapped in a 'Maybe')

  div_ (case rs ^. gsHeaderState . hsToolbarExtensionStatus of
    HT.ToolbarExtensionClosed -> []
    _ -> [ onClick $ \_ _ -> RS.dispatch (RS.HeaderAction HT.CloseToolbarExtension)
         ]) $ do
      windowSize_ (WindowSizeProps (rs ^. gsScreenState . SC.ssWindowSize)) mempty
      stickyContainer_ [] $ do
          mainHeader_ rs

          -- components that are only temporarily visible:
          showNote_ $ showNoteProps (vdoc ^. compositeVDocNotes) rs
          showDiscussion_ $ showDiscussionProps (vdoc ^. compositeVDocDiscussions) rs
          addComment_
            (rs ^. RS.gsTranslations . unNoJSONRep . to (cs .))
            $ AddCommentProps (rs ^. RS.gsContributionState . RS.csCommentEditorIsVisible)
                              (rs ^. RS.gsContributionState . RS.csCommentCategory)
                              (rs ^. RS.gsScreenState . SC.ssWindowWidth)
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
                      document_ $ DocumentProps (rs ^. RS.gsDocumentState)
                                                (rs ^. RS.gsContributionState)
                                                toolbarStatus
                                                (_compositeVDocVersion vdoc)
                      rightAside_ (rs ^. gsContributionState . csMarkPositions) (rs ^. gsScreenState)

mainScreen_ :: RS.GlobalState -> ReactElementM eventHandler ()
mainScreen_ rs = view mainScreen rs mempty


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
