{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
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
{-# LANGUAGE NoImplicitPrelude          #-}
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

module Refine.Frontend.Header.Heading
  ( TopMenuBarProps(..)
  , topMenuBar, topMenuBar_
  , mainHeader, mainHeader_
  , toolbarWrapper_
  , mkMainHeaderProps
  ) where

import Refine.Frontend.Prelude

import qualified Data.List.NonEmpty as NEL
import           Language.Css.Syntax
import           React.Flux as RF
import           React.Flux.Internal as RF
import           React.Flux.Outdated as RF

import           Refine.Common.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.DocumentHeader
import           Refine.Frontend.Header.DiffToolbar ( diffToolbar_ )
import           Refine.Frontend.Header.EditToolbar ( editToolbar_, wipeDocumentState )
import           Refine.Frontend.Header.DiscussionToolbar
import           Refine.Frontend.Header.Toolbar ( CommentToolbarExtensionProps(..), EditToolbarExtensionProps(..),
                                                  toolbar_, commentToolbarExtension_, editToolbarExtension_, indexToolbarExtension_ )
import           Refine.Frontend.Access
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Status
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (sticky_)
import           Refine.Frontend.Util


topMenuBar :: HasCallStack => View '[TopMenuBarProps]
topMenuBar = mkView "TopMenuBar" $ \props ->
  div_ [ classNamesAny [("c-mainmenu", True), ("c-mainmenu--toolbar-combined", props ^. isSticky)]
       , "style" @@= [decl "pointerEvents" (Ident "none")]
       ] $ do
    topMenuBarLeft_ props
    topMenuBarRight_ props

topMenuBar_ :: HasCallStack => TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBar_ = view_ topMenuBar "TopMenuBar_"

topMenuBarLeft :: View '[TopMenuBarProps]
topMenuBarLeft = mkView "TopMenuBarLeft" $ \(TopMenuBarProps sticky _currentUser) -> do
    button_ [ "aria-controls" $= "bs-navbar"
            , "aria-expanded" $= "false"
            , "className" $= "c-mainmenu__menu-button"
            , "type" $= "button"
            , "style" @@= [decl "pointerEvents" (Ident "all")]
            , onClick $ \_ _ -> simpleHandler . dispatch . MainMenuAction $ MainMenuActionOpen defaultMainMenuTab
            ] $ do
      span_ ["className" $= "sr-only"] "Navigation an/aus"
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
      span_ ["className" $= "c-mainmenu__icon-bar"] ""
    unless sticky $
      span_ ["className" $= "c-mainmenu__menu-button-label"] "MENU"

topMenuBarLeft_ :: TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBarLeft_ = view_ topMenuBarLeft "TopMenuBarLeft_"

topMenuBarRight_ :: TopMenuBarProps -> ReactElementM eventHandler ()
topMenuBarRight_ (TopMenuBarProps sticky cu) = do
    loginStatusButton_ (ibDarkBackground .~ not sticky) cu


-- | Note that if @toolbarItems_@ is a component ('View') rather than a 'ReactElementM', css styling
-- mysteriously breaks.
toolbarWrapper_ :: ReactElementM eventHandler () -> ReactElementM eventHandler ()
toolbarWrapper_ toolbarItems_ = do
  header_ ["className" $= "row row-align-middle c-vdoc-toolbar"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar__content"] $ do
          toolbarItems_


-- | extract the new state from event.
currentToolbarStickyState :: HasCallStack => Event -> Bool
currentToolbarStickyState (evtHandlerArg -> RF.HandlerArg j) = pFromJSVal j

mainHeader :: HasCallStack => RF.ReactView MainHeaderProps
mainHeader = RF.defineLifecycleView "HeaderSizeCapture" () RF.lifecycleConfig
     -- the render function inside a Lifecycle view does not update the children passed to it when the state changes
     -- (see react-flux issue #29), therefore we move everything inside the Lifecylce view.
   { RF.lRender = mainHeaderRender
   , RF.lComponentDidMount = Just $ \_ dom _ -> calcHeaderHeight dom
   , RF.lComponentDidUpdate = Just $ \_ dom _ _ _ -> calcHeaderHeight dom
   }

mainHeaderRender :: HasCallStack => () -> MainHeaderProps -> ReactElementM ('StatefulEventHandlerCode ()) ()
mainHeaderRender () (rs, as) = do
  let vdoc = fromMaybe (error "mainHeader: no vdoc!") $ rs ^? gsVDoc . _Just . _Just
      props = TopMenuBarProps (rs ^. gsToolbarSticky) (as ^. accLoginState . lsCurrentUser)

      mainMenuPart_ = do
        -- in the past, the following needed to be siblings because of the z-index handling.  not sure that's still the case.
        div_ ["className" $= "c-mainmenu__bg" {-, "role" $= "navigation" -}] mempty
        {- header_ ["role" $= "banner"] $ do -}
        topMenuBar_ props

      headerPart_
        = documentHeader_
        . DocumentHeaderProps (vdoc ^. compositeVDoc . vdocTitle)
        $ case rs ^. gsDocumentState of
          WipedDocumentStateDiff _ eid _ _ -> editDescToAbstract vdoc . ContribIDEdit $ eid ^. editID
          _ -> vdoc ^. compositeVDoc . vdocAbstract

      toolbarPart_ = div_ ["className" $= "c-fulltoolbar"] $ do
        sticky_ [RF.on "onStickyStateChange" $ \e -> simpleHandler $ \() ->
                    (dispatch . ToolbarStickyStateChange $ currentToolbarStickyState e, Nothing)] $ do
          toolbarWrapper_ $ case rs ^. gsDocumentState of

            WipedDocumentStateView -> liftViewToStateHandler
                                    . toolbar_ . fromJust $ rs ^? gsVDoc . _Just . _Just . compositeVDoc

            WipedDocumentStateDiff i edit collapsed editable -> diffToolbar_ $ DiffToolbarProps
              (edit ^. editID)
              i
              (edit ^. editKind)
              (edit ^. editVotes . to votesToCount)
              collapsed
              editable

            WipedDocumentStateEdit eprops -> editToolbar_ eprops

            WipedDocumentStateDiscussion dprops -> discussionToolbar_ dprops

          indexToolbarExtension_ $ mkIndexToolbarProps rs
          commentToolbarExtension_ $ CommentToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)
          editToolbarExtension_ $ EditToolbarExtensionProps (rs ^. gsHeaderState . hsToolbarExtensionStatus)

  div_ ["className" $= "c-fullheader"] $ do
      mainMenuPart_
      headerPart_
      toolbarPart_

mkIndexToolbarProps :: GlobalState_ WipedDocumentState -> IndexToolbarProps
mkIndexToolbarProps rs
  | rs ^. gsHeaderState . hsToolbarExtensionStatus == IndexToolbarExtension
  && fromMaybe True (not <$> rs ^? gsDocumentState . wipedDocumentStateDiffCollapsed)
  = mkIndex . _editVDocVersion <$> join (gsEdit rs)
  | otherwise = Nothing
  where
    mkIndex (RawContent bs _) =
      [ IndexItem (b ^. blockKey) (b ^. blockText) depth
      | b <- NEL.toList bs, depth <- maybeToList . headerDepth $ b ^. blockType
      ]

headerDepth :: BlockType -> Maybe Int
headerDepth = \case
  Header1 -> Just 1
  Header2 -> Just 2
  Header3 -> Just 3
  _ -> Nothing

mkMainHeaderProps :: AccessState -> GlobalState -> MainHeaderProps
mkMainHeaderProps as gs = (fmap (const $ wipeDocumentState as gs) gs, as)

mainHeader_ :: HasCallStack => MainHeaderProps -> ReactElementM eventHandler ()
mainHeader_ props = RF.viewWithSKey mainHeader "mainHeader" props mempty

calcHeaderHeight :: HasCallStack => RF.LDOM -> IO ()
calcHeaderHeight ldom = do
   this <- RF.lThis ldom
   h <- js_getHeaderHeight this
   when (h /= (-1)) . dispatchAndExec . ScreenAction $ AddHeaderHeight h

#ifdef __GHCJS__

foreign import javascript safe
  "refine$getHeaderHeight($1)"
  js_getHeaderHeight :: JSVal -> IO Int

#else

{-# ANN js_getHeaderHeight ("HLint: ignore Use camelCase" :: String) #-}
js_getHeaderHeight :: JSVal -> IO Int
js_getHeaderHeight = error "javascript FFI not available in GHC"

#endif
