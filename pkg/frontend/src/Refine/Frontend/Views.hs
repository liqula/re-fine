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

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as ST
import           Control.Lens (ix)
import           Language.Css.Syntax

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft (deleteMarksFromRawContent)
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.QuickCreate
import           Refine.Frontend.Contribution.Types as RS
import           Refine.Frontend.Document.Document
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.Heading
import           Refine.Frontend.Header.Types as HT
import           Refine.Frontend.Login.Types as LG
import           Refine.Frontend.MainMenu.Component (mainMenu_)
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types as SC
import           Refine.Frontend.Screen.WindowSize (windowSize_, WindowSizeProps(..))
import           Refine.Frontend.Store.Types as RS
import           Refine.Frontend.ThirdPartyViews (stickyContainer_)
import           Refine.Frontend.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Views.Types
import qualified Refine.Frontend.Workbench


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: HasCallStack => View '[]
refineApp = mkControllerView @'[StoreArg GlobalState] "RefineApp" $ \gs ->
  if False {- set conditional to 'True' to switch to workbench. -} then Refine.Frontend.Workbench.workbench_ gs else
  case gs ^? gsMainMenuState . mmState . mainMenuOpenTab of
      Nothing  -> mainScreen_ gs
      Just tab -> mainMenu_ $ MainMenuProps
                            (mapMainMenuTab (const groups) groupFromCache id id id tab)
                            (gs ^. gsMainMenuState . mmErrors)
                            (gs ^. gsLoginState . lsCurrentUser)
        where
          groupFromCache :: ID Group -> Group
          groupFromCache gid = fromMaybe (error "impossible") $ gs ^? gsServerCache . scGroups . ix gid
          groups :: [Group]
          groups = Map.elems $ gs ^. gsServerCache . scGroups

mainScreen :: HasCallStack => View '[GlobalState]
mainScreen = mkView "MainScreen" $ \rs -> do
  let vdoc = fromMaybe (error "mainScreen: no gsVDoc") (rs ^. gsVDoc) -- FIXME: improve this!  (introduce a custom props type with a CompositeVDoc *not* wrapped in a 'Maybe')

      __ :: Translations = rs ^. RS.gsTranslations . unTrans
                                -- FIXME: I think this could be done more nicely.

  div_ ["key" $= "maindiv" {-FIXME: seems not to work as expected, we still have a warning-}] $ do
      windowSize_ (WindowSizeProps (rs ^. gsScreenState . SC.ssWindowSize)) mempty
      stickyContainer_ [] $ do
          mainHeader_ $ mkMainHeaderProps rs

          -- components that are visible only sometimes:
          showNote_ `mapM_` showNoteProps (vdoc ^. compositeVDocApplicableNotes) rs
          case rs ^. RS.gsContributionState . RS.csActiveDialog of
            Just (ActiveDialogComment lst) -> do
              addComment_ __ $ AddContributionProps
                              (rs ^. RS.gsContributionState . RS.csCurrentSelectionWithPx)
                              lst
                              (rs ^. RS.gsScreenState . SC.ssWindowWidth)
            Just ActiveDialogEdit -> do
              addEdit_ $ AddContributionProps
                              (rs ^. RS.gsContributionState . RS.csCurrentSelectionWithPx)
                              (fromMaybe (error "rs ^? RS.gsDocumentState . documentStateEditInfo") $
                               rs ^? RS.gsDocumentState . documentStateEditInfo)
                              (rs ^. RS.gsScreenState . SC.ssWindowWidth)
            Nothing -> mempty

          main_ ["role" $= "main", "key" $= "main"] $ do
              div_ ["className" $= "grid-wrapper"] $ do
                  div_ ["className" $= "row row-align-center row-align-top"] $ do
                      let asideProps = AsideProps
                                     (rs ^. gsContributionState . csAllVerticalSpanBounds)
                                     (OffsetFromDocumentTop $ rs ^. gsScreenState . ssHeaderHeight + fixedHeaderHeight + 15)
                                     (rs ^. gsContributionState . csCurrentSelectionWithPx)
                                     (rs ^. gsContributionState . csHighlightedMarkAndBubble)
                                     (rs ^. gsScreenState)
                                     (fltr (vdoc ^. compositeVDocApplicableDiscussions))
                                     (fltr (vdoc ^. compositeVDocApplicableNotes))
                                     (fltr . fltrThisEdit $ vdoc ^. compositeVDocApplicableEdits)
                                     (case rs ^. gsDocumentState of
                                        DocumentStateDiff{} -> BubblePositioningEvenlySpaced
                                        _ -> rs ^. gsContributionState . csBubblePositioning)
                                     (rs ^. gsContributionState . csQuickCreateShowState)

                          fltrThisEdit = case rs ^. gsDocumentState of
                            DocumentStateDiff _ _ _ eid _ _ -> Map.filter $ (/= eid) . (^. editID)
                            DocumentStateEdit{} -> const mempty
                            _ -> id

                          fltr :: IsContribution c => Map (ID c) b -> [b]
                          fltr = if rs ^. gsHeaderState . hsReadOnly
                              then const mempty
                              else maybe Map.elems go (rs ^. gsContributionState . csBubbleFilter)
                            where
                              go allowed = fmap snd . filter ((`Set.member` allowed) . contribID . fst) . Map.toList

                      leftAside_ asideProps
                      document_ $ DocumentProps ((if rs ^. gsHeaderState . hsReadOnly
                                                  then mapDocumentState id deleteMarksFromRawContent id
                                                  else id)
                                                 $ rs ^. to RS.getDocumentState)
                                                (rs ^. RS.gsContributionState)
                      rightAside_ asideProps

          -- append an empty page to the botton.  (helps with legitimate attempts to scroll beyond
          -- the end of the document, e.g. when moving dialogs into the center of the screen before
          -- they have been rendered.)
          div_ ["style" @@= [decl "margin-bottom" (Px 800)]] $ pure ()

mainScreen_ :: HasCallStack => GlobalState -> ReactElementM eventHandler ()
mainScreen_ = view_ mainScreen "mainScreen_"


leftAside :: HasCallStack => View '[AsideProps]
leftAside = mkView "LeftAside" $ \props ->
  aside_ ["className" $= "sidebar sidebar-annotations gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: annotation => comment
    let protos = maybeStackProtoBubbles (props ^. asideBubblePositioning)
               $ (noteToProtoBubble props <$> (props ^. asideNotes))
              <> (discussionToProtoBubble props <$> (props ^. asideDiscussions))
    stackBubble BubbleLeft props `mapM_` protos

    quickCreate_ $ QuickCreateProps QuickCreateComment
        (props ^. asideQuickCreateShow)
        (props ^. asideCurrentRange)
        (props ^. asideScreenState)

leftAside_ :: HasCallStack => AsideProps -> ReactElementM eventHandler ()
leftAside_ = view_ leftAside "leftAside_"


rightAside :: HasCallStack => View '[AsideProps]
rightAside = mkView "RightAside" $ \props ->
  aside_ ["className" $= "sidebar sidebar-modifications gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: modifications => edit
    let protos = maybeStackProtoBubbles (props ^. asideBubblePositioning)
                  (editToProtoBubbles props =<< (props ^. asideEdits))
    stackBubble BubbleRight props `mapM_` protos

    quickCreate_ $ QuickCreateProps QuickCreateEdit
      (props ^. asideQuickCreateShow)
      (props ^. asideCurrentRange)
      (props ^. asideScreenState)

rightAside_ :: HasCallStack => AsideProps -> ReactElementM eventHandler ()
rightAside_ = view_ rightAside "rightAside_"


-- * helpers

-- | All contributions need to be positioned.  The default is '0' (beginning of the article).
lookupPosition :: HasCallStack => AsideProps -> MarkID -> VerticalSpanBounds
lookupPosition props cid = fromMaybe (VerticalSpanBounds (props ^. asideMinimumSpanYPos) constantBubbleHeight)
                         $ props ^? asideAllVerticalSpanBounds . allVerticalSpanBounds . at cid . _Just

lookupPositions :: HasCallStack => AsideProps -> ContributionID -> [(VerticalSpanBounds, Int)]
lookupPositions props cid = case
  [ (p, i)
  | (MarkContribution cid' i, p) <- Map.toList $ props ^. asideAllVerticalSpanBounds . allVerticalSpanBounds
  , cid' == cid
  ] of
    [] -> [(VerticalSpanBounds (props ^. asideMinimumSpanYPos) constantBubbleHeight, 0)]
    ps -> map snd . filter fst . computeDistance
        $ sortBy (compare `on` (^. verticalSpanBoundsTop) . fst) ps
  where
    computeDistance xs
      = zip (True: zipWith (\top prevtop -> top - prevtop > 2 * constantBubbleHeight + 20{-pixel-})
                           (tail tops)
                           tops
            ) xs
      where
        tops = (^. verticalSpanBoundsTop) . fst <$> xs

editToProtoBubbles :: HasCallStack => AsideProps -> Edit -> [ProtoBubble]
editToProtoBubbles aprops e
  = [ ProtoBubble (cid, i) pos $ elemText (e ^. editDesc)
    | (pos, i) <- lookupPositions aprops cid
    ]
  where cid = contribID $ e ^. editID

noteToProtoBubble :: HasCallStack => AsideProps -> Note -> ProtoBubble
noteToProtoBubble aprops n = ProtoBubble cid (lookupPosition aprops $ uncurry MarkContribution cid) (elemText (n ^. noteText))
  where cid = (contribID $ n ^. noteID, 0)

discussionToProtoBubble :: HasCallStack => AsideProps -> Discussion -> ProtoBubble
discussionToProtoBubble aprops d = ProtoBubble cid (lookupPosition aprops $ uncurry MarkContribution cid) child
  where
    cid = (contribID $ d ^. discussionID, 0)
    child = elemText (ST.rootLabel (d ^. discussionTree) ^. statementText)

stackBubble :: HasCallStack => BubbleSide -> AsideProps -> StackOrNot ProtoBubble -> ReactElementM 'EventHandlerCode ()
stackBubble bubbleSide aprops bstack = bubble_ props children
  where
    bstack' :: StackOrNot (ContributionID, Int)
    bstack' = view protoBubbleContributionID <$> bstack

    props = BubbleProps
      { _bubblePropsContributionIds   = bstack'
      , _bubblePropsIconSide          = bubbleSide
      , _bubblePropsVerticalOffset    = voffset
      , _bubblePropsHighlight         = highlight
      , _bubblePropsScreenState       = aprops ^. asideScreenState
      }

    voffset = if aprops ^. asideBubblePositioning == BubblePositioningAbsolute
                then Just $ stackToHead bstack ^. protoBubbleVerticalSpanBounds . verticalSpanBoundsTop
                else Nothing

    highlight = not . Set.null $ Set.intersection shots hits
      where
        hits  = Set.fromList [cid | MarkContribution cid _ <- aprops ^. asideHighlighteds]
        shots = Set.fromList (fst <$> stackToList bstack')

    children = stackToHead bstack ^. protoBubbleChild
