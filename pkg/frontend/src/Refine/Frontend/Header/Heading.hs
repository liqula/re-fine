{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Header.Heading
  ( TopMenuBarProps(..)
  , mkMainHeaderProps, mkMainHeaderToolbarProps
  , mainHeader_
  , mainHeaderToolbar_
  ) where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           Refine.Common.Types
import           Refine.Frontend.Access
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.DiffToolbar (diffToolbar_)
import           Refine.Frontend.Header.DiscussionToolbar
import           Refine.Frontend.Header.DocumentHeader
import           Refine.Frontend.Header.EditToolbar (editToolbar_, wipeDocumentState)
import           Refine.Frontend.Header.Toolbar ( CommentToolbarExtensionProps(..), EditToolbarExtensionProps(..),
                                                  toolbar_, commentToolbarExtension_, editToolbarExtension_, indexToolbarExtension_ )
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon.Svg
import           Refine.Frontend.Login.Status
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.MainMenu.Component (topMenuBar)
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Util


mainHeader :: HasCallStack => React.ReactView MainHeaderProps
mainHeader = React.defineLifecycleView "MainHeader" () React.lifecycleConfig
     -- the render function inside a Lifecycle view does not update the children passed to it when the state changes
     -- (see react-flux issue #29), therefore we move everything inside the Lifecylce view.
   { React.lRender = mainHeaderRender
   , React.lComponentDidMount = Just $ \_ dom _ -> calcHeaderHeight dom
   , React.lComponentDidUpdate = Just $ \_ dom _ _ _ -> calcHeaderHeight dom
   }

mainHeaderRender :: HasCallStack => () -> MainHeaderProps -> ReactElementM eventHandler ()
mainHeaderRender () (title, abstract, topmenuprops) = do
  div_ ["className" $= "c-fullheader"] $ do
    view_ topMenuBar "topMenuBar" topmenuprops
    documentHeader_ $ DocumentHeaderProps title abstract

mainHeader_ :: HasCallStack => MainHeaderProps -> ReactElementM eventHandler ()
mainHeader_ props = React.viewWithSKey mainHeader "mainHeader" props mempty


-- | FIXME: make this a component, not just an element.
mainHeaderToolbar_ :: HasCallStack => MainHeaderToolbarProps -> ReactElementM eventHandler ()
mainHeaderToolbar_ props = do
  case props ^. mainHeaderToolbarPropsDocumentState of
            WipedDocumentStateView -> toolbar_ $ props ^. mainHeaderToolbarPropsVDoc

            WipedDocumentStateDiff i edit collapsed editable -> diffToolbar_ $ DiffToolbarProps
              (edit ^. editID)
              i
              (edit ^. editKind)
              (edit ^. editVotes . to votesToCount)
              collapsed
              editable

            WipedDocumentStateEdit eprops -> editToolbar_ eprops

            WipedDocumentStateDiscussion dprops -> discussionToolbar_ dprops

  indexToolbarExtension_ $ props ^. mainHeaderToolbarPropsIndexToolbarProps
  commentToolbarExtension_ $ CommentToolbarExtensionProps (props ^. mainHeaderToolbarPropsExtStatus)
  editToolbarExtension_ $ EditToolbarExtensionProps (props ^. mainHeaderToolbarPropsExtStatus)


headerDepth :: BlockType -> Maybe Int
headerDepth = \case
  Header1 -> Just 1
  Header2 -> Just 2
  Header3 -> Just 3
  _ -> Nothing

mkMainHeaderProps :: AccessState -> GlobalState_ WipedDocumentState -> MainHeaderProps
mkMainHeaderProps as wiped = (title, abstract, props)
  where
    cvdoc = fromMaybe (error "mkMainHeaderProps: no vdoc!") $ wiped ^? gsCompositeVDoc . _Just . _Just
    props = TopMenuBarProps Nothing (cacheLookup' wiped <$> (as ^. accLoginState . lsCurrentUser))

    title = cvdoc ^. compositeVDoc . vdocTitle
    abstract = cvdoc ^. compositeVDoc . vdocAbstract

mkMainHeaderToolbarProps :: GlobalState_ WipedDocumentState -> MainHeaderToolbarProps
mkMainHeaderToolbarProps wiped = MainHeaderToolbarProps ds vdoc indexprops extprops
  where
    Just ds = wiped ^. gsDocumentState
    Just vdoc = wiped ^? gsCompositeVDoc . _Just . _Just . compositeVDoc
    indexprops = mkIndexToolbarProps wiped
    Just extprops = wiped ^? gsHeaderState . _Just . hsToolbarExtensionStatus

mkIndexToolbarProps :: GlobalState_ WipedDocumentState -> IndexToolbarProps
mkIndexToolbarProps rs
  | rs ^? gsHeaderState . _Just . hsToolbarExtensionStatus == Just IndexToolbarExtension
  && fromMaybe True (not <$> rs ^? gsDocumentState . _Just . wipedDocumentStateDiffCollapsed)
  = mkIndex . _editVDocVersion <$> join (rs ^. gsEdit)
  | otherwise = Nothing
  where
    mkIndex (RawContent bs _) =
      [ IndexItem (b ^. blockKey) (b ^. blockText) depth
      | b <- NEL.toList bs, depth <- maybeToList . headerDepth $ b ^. blockType
      ]


calcHeaderHeight :: HasCallStack => React.LDOM -> IO ()
calcHeaderHeight ldom = do
   this <- React.lThis ldom
   h <- js_getHeaderHeight this
   when (h /= (-1)) . dispatchAndExec . ScreenAction . AddHeaderHeight $ h + 80

#ifdef __GHCJS__

foreign import javascript safe
  "refine$getHeaderHeight($1)"
  js_getHeaderHeight :: JSVal -> IO Int

#else

{-# ANN js_getHeaderHeight ("HLint: ignore Use camelCase" :: String) #-}
js_getHeaderHeight :: JSVal -> IO Int
js_getHeaderHeight = error "javascript FFI not available in GHC"

#endif
