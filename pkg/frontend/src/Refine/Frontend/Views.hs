{-# LANGUAGE NoImplicitPrelude          #-}
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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Views
  ( refineApp

  -- for testing:
  , mainScreen_
  ) where

import Refine.Frontend.Prelude

import           Control.Lens (ix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as ST

import           Refine.Common.Types
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.QuickCreate
import           Refine.Frontend.Contribution.Types as RS
import           Refine.Frontend.Document.Document
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Heading (mainHeader_)
import           Refine.Frontend.Header.Types as HT
import           Refine.Frontend.Loader.Component
import           Refine.Frontend.Login.Types as LG
import           Refine.Frontend.MainMenu.Component (mainMenu_)
import           Refine.Frontend.MainMenu.Types
import qualified Refine.Frontend.Screen.Types as SC
import           Refine.Frontend.Screen.WindowSize (windowSize_, WindowSizeProps(..))
import           Refine.Frontend.Store as RS
import           Refine.Frontend.Store.Types as RS
import           Refine.Frontend.ThirdPartyViews (stickyContainer_)
import           Refine.Frontend.Views.Types
import qualified Refine.Frontend.Workbench


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: View '[]
refineApp = mkControllerView @'[StoreArg GlobalState] "RefineApp" $ \gs ->
  if False {- set conditional to 'True' to switch to workbench. -} then Refine.Frontend.Workbench.workbench_ gs else
  case gs ^. gsVDoc of
    Nothing -> vdocLoader_ (VDocLoaderProps $ gs ^. gsVDocList)  -- (this is just some scaffolding that will be replaced by more app once we get there.)
    Just _ -> case gs ^? gsMainMenuState . mmState . mainMenuOpenTab of
      Nothing  -> mainScreen_ gs
      Just tab -> mainMenu_ tab
                            (gs ^. gsMainMenuState . mmErrors)
                            (gs ^. gsLoginState . lsCurrentUser)

mainScreen :: View '[GlobalState]
mainScreen = mkView "MainScreen" $ \rs -> do
  let vdoc = fromJust (rs ^. gsVDoc) -- FIXME: improve this!  (introduce a custom props type with a CompositeVDoc *not* wrapped in a 'Maybe')

      __ :: Translations = rs ^. RS.gsTranslations . unTrans
                                -- FIXME: I think this could be done more nicely.

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
          addComment_ __ $ AddContributionProps
                              (rs ^. RS.gsContributionState . RS.csActiveDialog == Just ActiveDialogComment)
                              (rs ^. RS.gsContributionState . RS.csCurrentRange)
                              (rs ^. RS.gsContributionState . RS.csCommentKind)
                              (rs ^. RS.gsScreenState . SC.ssWindowWidth)
          addEdit_ $ AddContributionProps
                              (rs ^. RS.gsContributionState . RS.csActiveDialog == Just ActiveDialogEdit)
                              (rs ^. RS.gsContributionState . RS.csCurrentRange)
                              (rs ^? RS.gsDocumentState . documentStateEditKind)
                              (rs ^. RS.gsScreenState . SC.ssWindowWidth)

          main_ ["role" $= "main"] $ do
              div_ ["className" $= "grid-wrapper"] $ do
                  div_ ["className" $= "row row-align-center row-align-top"] $ do
                      let asideProps = AsideProps
                                     (rs ^. gsContributionState . csMarkPositions)
                                     (rs ^. gsContributionState . csCurrentRange)
                                     (rs ^. gsContributionState . csHighlightedMarkAndBubble)
                                     (rs ^. gsScreenState)
                                     (fltr (vdoc ^. compositeVDocDiscussions))
                                     (fltr (vdoc ^. compositeVDocNotes))
                                     (fltr (vdoc ^. compositeVDocEdits))
                                     (rs ^. gsContributionState . csQuickCreateShowState)

                          fltr :: IsContribution c => Map (ID c) b -> [b]
                          fltr = maybe Map.elems go (rs ^. gsContributionState . csBubbleFilter)
                            where
                              go allowed = fmap snd . filter ((`Set.member` allowed) . contribID . fst) . Map.toList

                      leftAside_ asideProps
                      document_ $ DocumentProps (rs ^. RS.gsDocumentState)
                                                (rs ^. RS.gsContributionState)
                                                (rs ^. gsHeaderState . hsToolbarExtensionStatus)
                                                (case rs ^. RS.gsContributionState . csDisplayedContributionID of
                                                  Just (ContribIDEdit eid) -> Just $ vdoc ^?! compositeVDocEdits . ix eid
                                                  _ -> Nothing)
                      rightAside_ asideProps

mainScreen_ :: GlobalState -> ReactElementM eventHandler ()
mainScreen_ !rs = view_ mainScreen "mainScreen_" rs


leftAside :: View '[AsideProps]
leftAside = mkView "LeftAside" $ \props ->
    aside_ ["className" $= "sidebar sidebar-annotations gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: annotation => comment
        mconcat $ map (\d -> discussionBubble_ (SpecialBubbleProps
                                                 (ContribIDDiscussion (d ^. compositeDiscussion . discussionID))
                                                 (lookupPosition props $ ContribIDDiscussion (d ^. compositeDiscussion . discussionID))
                                                 (props ^. asideHighlightedBubble)
                                                 (props ^. asideScreenState)
                                               )
                                               (elemText (ST.rootLabel (d ^. compositeDiscussionTree) ^. statementText))) -- we always have one stmt
                      (props ^. asideDiscussions)
        mconcat $ map (\n -> noteBubble_ (SpecialBubbleProps
                                           (ContribIDNote (n ^. noteID))
                                           (lookupPosition props $ ContribIDNote (n ^. noteID))
                                           (_asideHighlightedBubble props)
                                           (_asideScreenState props)
                                         )
                                         (elemText (n ^. noteText)))
                      (props ^. asideNotes)
        quickCreate_ $ QuickCreateProps QuickCreateComment
            (props ^. asideQuickCreateShow)
            (props ^. asideCurrentRange)
            (props ^. asideScreenState)

leftAside_ :: AsideProps -> ReactElementM eventHandler ()
leftAside_ !props = view_ leftAside "leftAside_" props


rightAside :: View '[AsideProps]
rightAside = mkView "RightAside" $ \props ->
  aside_ ["className" $= "sidebar sidebar-modifications gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: modifications => edit
    mconcat $ map (\e -> editBubble_ (SpecialBubbleProps
                                       (ContribIDEdit (e ^. editID))
                                       (lookupPosition props $ ContribIDEdit (e ^. editID))
                                       (props ^. asideHighlightedBubble)
                                       (props ^. asideScreenState)
                                     )
                                     (elemText (e ^. editDesc)))
                  (props ^. asideEdits)

    quickCreate_ $ QuickCreateProps QuickCreateEdit
      (props ^. asideQuickCreateShow)
      (props ^. asideCurrentRange)
      (props ^. asideScreenState)

rightAside_ :: AsideProps -> ReactElementM eventHandler ()
rightAside_ !props = view_ rightAside "rightAside_" props


-- * helpers

lookupPosition :: AsideProps -> ContributionID -> Maybe MarkPosition
lookupPosition props cid = props ^? asideMarkPositions . markPositionsMap . at cid . _Just
