{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -Wno-orphans #-}

module Refine.Frontend.Test.Arbitrary where
#include "import_frontend.hs"

import Test.QuickCheck
import Generics.SOP as SOP
import GHC.IORef
import System.IO.Unsafe
import GHCJS.Types

import React.Flux.Missing
import Refine.Common.Error
import Refine.Common.Route
import Refine.Common.Test.Arbitrary
import Refine.Common.Types
import Refine.Common.OT
import Refine.Frontend.Access
import Refine.Frontend.Contribution.Types
import Refine.Frontend.Document.FFI
import Refine.Frontend.Document.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Icon.Svg
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Screen.Types
import Refine.Frontend.Login.Types
import Refine.Frontend.Store.Types
import Refine.Frontend.Types


{-# NOINLINE genIORef #-}
genIORef :: a -> IORef a
genIORef = unsafePerformIO . newIORef


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f) => Arbitrary (MainMenuTab a b c d e f) where arbitrary = garbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (FormActionWith a b) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (CommentInfo a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (CurrentUser a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (EditInfo a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (IORef a) where arbitrary = genIORef <$> arbitrary  -- FIXME: not sure genIORef works this way.
instance Arbitrary a => Arbitrary (LocalStateRef a) where arbitrary = garbitrary
instance Arbitrary a => Arbitrary (ProcessState a) where arbitrary = garbitrary
instance Arbitrary AccessState where arbitrary = garbitrary
instance Arbitrary ActiveDialog where arbitrary = garbitrary
instance Arbitrary AllVerticalSpanBounds where arbitrary = garbitrary
instance Arbitrary BubblePositioning where arbitrary = garbitrary
instance Arbitrary ButtonPressed where arbitrary = garbitrary
instance Arbitrary ButtonRollOver where arbitrary = garbitrary
instance Arbitrary ButtonState where arbitrary = garbitrary
instance Arbitrary CacheAction where arbitrary = garbitrary
instance Arbitrary CommentInputState where arbitrary = garbitrary
instance Arbitrary CommentKind where arbitrary = garbitrary
instance Arbitrary ContributionAction where arbitrary = garbitrary
instance Arbitrary ContributionState where arbitrary = garbitrary
instance Arbitrary DevState where arbitrary = garbitrary
instance Arbitrary DocumentAction where arbitrary = garbitrary
instance Arbitrary DocumentState where arbitrary = garbitrary
instance Arbitrary EditIndex where arbitrary = garbitrary
instance Arbitrary EditInputState where arbitrary = garbitrary
instance Arbitrary EditIsInitial where arbitrary = garbitrary
instance Arbitrary EditorState where arbitrary = createWithRawContent <$> garbitrary
instance Arbitrary File where arbitrary = pure $ File nullRef
instance Arbitrary GlobalAction where arbitrary = garbitrary
instance Arbitrary GlobalState where arbitrary = garbitrary
instance Arbitrary HeaderAction where arbitrary = garbitrary
instance Arbitrary HeaderState where arbitrary = garbitrary
instance Arbitrary LoginState where arbitrary = garbitrary
instance Arbitrary MainMenuAction where arbitrary = garbitrary
instance Arbitrary MainMenuErrors where arbitrary = garbitrary
instance Arbitrary MainMenuGroup where arbitrary = garbitrary
instance Arbitrary MainMenuState where arbitrary = garbitrary
instance Arbitrary MainMenuSubTabLogin where arbitrary = garbitrary
instance Arbitrary OffsetFromDocumentTop where arbitrary = garbitrary
instance Arbitrary OffsetFromViewportTop where arbitrary = garbitrary
instance Arbitrary (PageState DocumentState) where arbitrary = garbitrary
instance Arbitrary QuickCreateShowState where arbitrary = garbitrary
instance Arbitrary Route where arbitrary = garbitrary
instance Arbitrary ScreenAction where arbitrary = garbitrary
instance Arbitrary ScreenState where arbitrary = garbitrary
instance Arbitrary ScrollOffsetOfViewport where arbitrary = garbitrary
instance Arbitrary SelectionStateWithPx where arbitrary = garbitrary
instance Arbitrary StatementEditorProps where arbitrary = garbitrary
instance Arbitrary ToolbarExtensionStatus where arbitrary = garbitrary
instance Arbitrary VerticalSpanBounds where arbitrary = garbitrary
instance Arbitrary WindowSize where arbitrary = garbitrary
