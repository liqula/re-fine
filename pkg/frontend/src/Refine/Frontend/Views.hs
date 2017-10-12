{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Views
  ( refineApp
  , wholeScreen_
  ) where
#include "import_frontend.hs"

import           Data.Maybe (mapMaybe)
import           Language.Css.Syntax

import           Refine.Common.Types
import           Refine.Common.VDoc.Draft (deleteMarksFromRawContent)
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.QuickCreate
import           Refine.Frontend.Contribution.Types as RS
import           Refine.Frontend.Document.Document
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.EditToolbar
import           Refine.Frontend.Header.Heading
import           Refine.Frontend.Header.Types as HT
import           Refine.Frontend.Login.Types as LG
import           Refine.Frontend.MainMenu.Component (mainMenu)
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types as SC
import           Refine.Frontend.Screen.WindowSize (trackWindowSize)
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Debug
import           Refine.Frontend.Types
import           Refine.Frontend.Util
import           Refine.Frontend.Views.Types
import           Refine.Frontend.Store (flushCacheMisses)


-- | The controller view and also the top level of the Refine app.  This controller view registers
-- with the store and will be re-rendered whenever the store changes.
refineApp :: HasCallStack => View '[]
refineApp = mkControllerView @'[StoreArg AccessState, StoreArg GlobalState] "RefineApp" wholeScreen_

wholeScreen_ :: HasCallStack => AccessState -> GlobalState -> ReactElementM eventHandler ()
wholeScreen_ accessState props = React.viewWithSKey wholeScreen "wholeScreen" (props, accessState) mempty

wholeScreen :: React.ReactView (GlobalState, AccessState)
wholeScreen = React.defineLifecycleView "WholeScreen" () React.lifecycleConfig
  { React.lRender = \() (gs, as) ->
     case gs ^. gsPageState of
      PageStateVDoc pst
        -> view_' mainScreen "mainScreen" (gs, pst, as)
      PageStateMainMenu (mainmenustate :: MainMenuState) _
        -> view_ mainMenu "mainMenu" $ MainMenuProps
                            (mapMainMenuTab
                              (const (GroupsProps groups vdocs users))
                              groupFromCache
                              (flip (,) users)
                              id
                              id
                              (first $ cacheLookup' gs)
                              (mainmenustate ^. mmState :: MainMenuTabState))
                            (mainmenustate ^. mmErrors :: MainMenuErrors)
                            (cacheLookup' gs <$> (as ^. accLoginState . lsCurrentUser))
        where
          groupFromCache :: ID Group -> GroupProps
          groupFromCache gid = GroupProps (cacheLookup gs gid) vdocs users

          groups :: [Group]
          groups = mapMaybe (cacheLookup gs) . Set.elems
                 . fromMaybe (cacheMiss CacheKeyGroupIds mempty mainmenustate)
                 $ gs ^. gsServerCache . scGroupIds

          vdocs :: Map (ID VDoc) VDoc
          vdocs = gs ^. gsServerCache . scVDocs

          users :: Map (ID User) User
          users = Map.fromList . mapMaybe (\i -> (,) i <$> cacheLookup gs i) . Set.elems
                . fromMaybe (cacheMiss CacheKeyUserIds mempty mainmenustate)
                $ gs ^. gsServerCache . scUserIds

  , React.lComponentDidMount = Just $ \this _ _ -> didMountOrUpdate this
  , React.lComponentDidUpdate = Just $ \this _ _ _ _ -> didMountOrUpdate this
  }
  where
    didMountOrUpdate :: HasCallStack => React.LPropsAndState (GlobalState, AccessState) () -> IO ()
    didMountOrUpdate _getPropsAndState = flushCacheMisses

mainScreen :: HasCallStack => View' '[(GlobalState, ProcessState DocumentState, AccessState)]
mainScreen = mkView' "MainScreen" $ \(gst, procst, as) ->
  let cachest  = gst ^. gsServerCache
      screenst = gst ^. gsScreenState
      gstwiped = wipeDocumentState as gst
      contrst  = procst ^. psContributionState
      docst    = procst ^. psDocumentState

      asideProps :: [contrib] -> AsideProps contrib
      asideProps = mkAsideProps gst procst bubblepos
        where
          bubblepos = case procst ^. psDocumentState of
                        DocumentStateDiff{} -> BubblePositioningEvenlySpaced
                        _ -> contrst ^. csBubblePositioning

  in div_ ["key" $= "maindiv" {-FIXME: seems not to work as expected, we still have a warning-}] $ do
    React.view trackWindowSize (screenst ^. SC.ssWindowSize) mempty
    div_ ["className" $= "c_bg_blue_dark"] $ mainHeader_ (mkMainHeaderProps as gstwiped)

    case (procst ^. psVDocID, cachest) ^. getCompositeVDoc of
      Nothing -> hourglass
      Just vdoc -> do
          -- components that are visible only sometimes:
          case contrst ^. csActiveDialog of
              Just (ActiveDialogComment lst) -> do
                addComment_ $ AddContributionProps
                                (contrst ^. csCurrentSelectionWithPx)
                                lst
                                (screenst ^. SC.ssWindowWidth)
              Just (ActiveDialogEdit estate) -> do
                let Just (localst :: EditInfo (Maybe EditKind)) = docst ^? documentStateEditInfo
                addEdit_ $ AddContributionProps
                                (contrst ^. csCurrentSelectionWithPx)
                                (localst, estate)
                                (screenst ^. SC.ssWindowWidth)
              Nothing -> mempty

          main_ ["role" $= "main", "key" $= "main"] $ do
              mainHeaderToolbar_ (mkMainHeaderToolbarProps gstwiped)
              div_ ["className" $= "grid-wrapper"] $ do
                  div_ ["className" $= "row row-align-center row-align-top"] $ do
                      view_ leftAside "leftAside_" (asideProps $ filterDiscussions procst (contrst ^. csBubbleFilter) vdoc)
                      document_ $ DocumentProps ((if procst ^. psHeaderState . hsReadOnly
                                                  then mapDocumentState id deleteMarksFromRawContent id id
                                                  else id)
                                                 $ getDocumentStateProps as gst)
                                                contrst
                      view_ rightAside "rightAside_" (asideProps $ filterEdits procst (contrst ^. csBubbleFilter) vdoc)

          -- append an empty page to the botton.  (helps with legitimate attempts to scroll beyond
          -- the end of the document, e.g. when moving dialogs into the center of the screen before
          -- they have been rendered.)
          div_ ["style" @@= [decl "marginBottom" (Px 800)]] $ pure ()


leftAside :: HasCallStack => View '[AsideProps Discussion]
leftAside = mkView "LeftAside" $ \props ->
  aside_ ["className" $= "sidebar sidebar-annotations gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: annotation => comment
    let protos = maybeStackProtoBubbles (props ^. asideBubblePositioning)
               $ (noteToProtoBubble props <$> filter (^. discussionIsNote) (props ^. asideContributions))
              <> (discussionToProtoBubble props <$> filter (not . (^. discussionIsNote)) (props ^. asideContributions))
    stackBubble BubbleLeft props `mapM_` protos

    view_ quickCreate "quickCreate" $ QuickCreateProps QuickCreateComment
        (props ^. asideQuickCreateShow)
        (props ^. asideCurrentRange)
        (props ^. asideScreenState)


rightAside :: HasCallStack => View '[AsideProps Edit]
rightAside = mkView "RightAside" $ \props ->
  aside_ ["className" $= "sidebar sidebar-modifications gr-2 gr-5@desktop hide@mobile"] $ do  -- RENAME: modifications => edit
    let protos = maybeStackProtoBubbles (props ^. asideBubblePositioning)
                  (editToProtoBubbles props =<< (props ^. asideContributions))
    stackBubble BubbleRight props `mapM_` protos

    view_ quickCreate "quickCreate" $ QuickCreateProps QuickCreateEdit
      (props ^. asideQuickCreateShow)
      (props ^. asideCurrentRange)
      (props ^. asideScreenState)


-- * helpers

mkAsideProps :: GlobalState -> ProcessState DocumentState -> BubblePositioning -> [contrib] -> AsideProps contrib
mkAsideProps (view gsScreenState -> screenst) (view psContributionState -> contrst) bubblepos contribs =
  AsideProps
    (contrst ^. csAllVerticalSpanBounds)
    (OffsetFromDocumentTop $ screenst ^. ssHeaderHeight + fixedHeaderHeight + 15)
    (contrst ^. csCurrentSelectionWithPx)
    (contrst ^. csHighlightedMarkAndBubble)
    screenst
    contribs
    bubblepos
    (contrst ^. csQuickCreateShowState)


filterEdits :: ProcessState DocumentState -> Maybe (Set ContributionID) -> CompositeVDoc -> [Edit]
filterEdits procst bfilter vdoc =
  filterContributions procst bfilter ((:[]) . ContribIDEdit) (fltrThisEdit $ vdoc ^. compositeVDocApplicableEdits)
  where
    fltrThisEdit = case procst ^. psDocumentState of
                     DocumentStateDiff _ _ eid _ _ -> Map.filter $ (/= eid) . (^. editID)
                     DocumentStateEdit{} -> const mempty
                     _ -> id

filterContributions :: ProcessState DocumentState -> Maybe (Set ContributionID) -> (ID c -> [ContributionID]) -> Map (ID c) b -> [b]
filterContributions procst bfilter mkCId = if procst ^. psHeaderState . hsReadOnly
                              then const mempty
                              else maybe Map.elems go bfilter
                            where
                              go allowed = fmap snd . filter (any (`Set.member` allowed) . mkCId . fst) . Map.toList

filterDiscussions :: ProcessState DocumentState -> Maybe (Set ContributionID) -> CompositeVDoc -> [Discussion]
filterDiscussions procst bfilter vdoc =
  filterContributions procst bfilter
    (\i -> ContribIDDiscussion <$> [True, False] <*> pure i)
    (fmap snd $ vdoc ^. compositeVDocApplicableDiscussions)


-- | All contributions need to be positioned.  The default is '0' (beginning of the article).
lookupPosition :: HasCallStack => AsideProps a -> MarkID -> VerticalSpanBounds
lookupPosition props cid = fromMaybe (VerticalSpanBounds (props ^. asideMinimumSpanYPos) constantBubbleHeight)
                         $ props ^? asideAllVerticalSpanBounds . allVerticalSpanBounds . at cid . _Just

lookupPositions :: HasCallStack => AsideProps a -> ContributionID -> [(VerticalSpanBounds, Int)]
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

editToProtoBubbles :: HasCallStack => AsideProps Edit -> Edit -> [ProtoBubble]
editToProtoBubbles aprops e
  = [ ProtoBubble (cid, i) pos $ elemText (e ^. editDesc)
    | (pos, i) <- lookupPositions aprops cid
    ]
  where cid = ContribIDEdit $ e ^. editID

noteToProtoBubble :: HasCallStack => AsideProps Discussion -> Discussion -> ProtoBubble
noteToProtoBubble aprops n = ProtoBubble cid (lookupPosition aprops $ uncurry MarkContribution cid) (elemText (n ^. noteText))
  where cid = (ContribIDDiscussion True $ n ^. discussionID, 0)

-- TODO: merge with 'noteToProtoBubble'
discussionToProtoBubble :: HasCallStack => AsideProps Discussion -> Discussion -> ProtoBubble
discussionToProtoBubble aprops d = ProtoBubble cid (lookupPosition aprops $ uncurry MarkContribution cid) child
  where
    cid = (ContribIDDiscussion False $ d ^. discussionID, 0)
    child = elemText (Tree.rootLabel (d ^. discussionTree) ^. statementText)

stackBubble :: HasCallStack => BubbleSide -> AsideProps a -> StackOrNot ProtoBubble -> ReactElementM 'EventHandlerCode ()
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
