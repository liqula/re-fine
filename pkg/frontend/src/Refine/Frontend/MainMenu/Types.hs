{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.MainMenu.Types where
#include "import_frontend.hs"

import React.Flux.Missing
import Refine.Common.Types
import Refine.Common.Rest (ApiError)
import Refine.Common.Types.Prelude (Username)
import Refine.Frontend.Types
import Refine.Frontend.Login.Types
import Control.DeepSeq


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTabAction
  | MainMenuActionLoginError        Username
  | MainMenuActionRegistrationError ApiError
  | MainMenuActionClearErrors
  deriving (Eq, Show, Generic)

data MainMenuErrors = MainMenuErrors
  { _mmeLogin        :: FormError
  , _mmeRegistration :: FormError
  }
  deriving (Eq, Show, Generic)

defaultMainMenuErrors :: HasCallStack => MainMenuErrors
defaultMainMenuErrors = MainMenuErrors
  { _mmeLogin        = Nothing
  , _mmeRegistration = Nothing
  }

data MainMenuState = MainMenuState
  { _mmState  :: MainMenu
  , _mmErrors :: MainMenuErrors
  }
  deriving (Eq, Show, Generic)

emptyMainMenuState :: HasCallStack => MainMenuState
emptyMainMenuState = MainMenuState
  { _mmState  = MainMenuOpen $ MainMenuLogin MainMenuSubTabLogin
  , _mmErrors = defaultMainMenuErrors
  }

data MainMenu
  = MainMenuClosed
  | MainMenuOpen { _mainMenuOpenTab :: MainMenuTabState }
  deriving (Eq, Show, Generic)

type MainMenuTabState = (MainMenuTab
      ()
      (ID Group)
      (LocalStateRef (CreateGroup_ [(User, Bool)]))
      (LocalStateRef CreateVDoc)
      (LocalStateRef UpdateVDoc)
      ImageUpload
      :: *)
type MainMenuTabAction = (MainMenuTab
      ()
      (ID Group)
      (FormAction (CreateGroup_ [(User, Bool)]))
      (FormAction CreateVDoc)
      (FormAction UpdateVDoc)
      ImageUpload
      :: *)
type MainMenuTabProps = (MainMenuTab
      GroupsProps
      GroupProps
      (LocalStateRef (CreateGroup_ [(User, Bool)]), Map (ID User) User)
      (LocalStateRef CreateVDoc)
      (LocalStateRef UpdateVDoc)
      ImageUpload
      :: *)

type ImageUpload = Maybe (NoJSONRep File, Maybe Image)

newtype File = File JSVal deriving (FromJSVal)
instance Eq File where _ == _ = False
instance NFData File where rnf _ = ()

type GroupProps = (Maybe Group, Map (ID VDoc) VDoc, Map (ID User) User)
type GroupsProps = ([Group], Map (ID VDoc) VDoc, Map (ID User) User)

-- | FUTUREWORK: it may be nicer after all to have different types for action, state, and props
-- here.  but for now it should work.
data MainMenuTab gids group cgroup cprocess uprocess profile
  = MainMenuGroups gids
  | MainMenuGroup MainMenuGroup group
  | MainMenuCreateOrUpdateGroup (Maybe (ID Group)) cgroup
  | MainMenuCreateProcess cprocess
  | MainMenuUpdateProcess (ID VDoc) uprocess
  | MainMenuHelp
  | MainMenuLogin MainMenuSubTabLogin
  | MainMenuProfile profile
  deriving (Eq, Show, Generic)

data MainMenuGroup
  = MainMenuGroupProcesses
  | MainMenuGroupMembers
  deriving (Eq, Show, Generic)

mapMainMenuTab :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> (e -> e') -> (f -> f') -> MainMenuTab a b c d e f -> MainMenuTab a' b' c' d' e' f'
mapMainMenuTab fa fb fc fd fe ff = \case
  MainMenuGroups a -> MainMenuGroups (fa a)
  MainMenuGroup g b  -> MainMenuGroup g (fb b)
  MainMenuCreateOrUpdateGroup u c -> MainMenuCreateOrUpdateGroup u (fc c)
  MainMenuCreateProcess d -> MainMenuCreateProcess (fd d)
  MainMenuUpdateProcess pid e -> MainMenuUpdateProcess pid (fe e)
  MainMenuHelp     -> MainMenuHelp
  MainMenuLogin l  -> MainMenuLogin l
  MainMenuProfile f -> MainMenuProfile $ ff f

defaultMainMenuTab :: MainMenuTabAction
defaultMainMenuTab = MainMenuGroups ()

data MainMenuSubTabLogin = MainMenuSubTabLogin | MainMenuSubTabRegistration
  deriving (Eq, Show, Generic)

data MainMenuProps tab = MainMenuProps
  { _mmpMainMenuTab    :: tab
  , _mmpMainMenuErrors :: MainMenuErrors
  , _mmpCurrentUser    :: CurrentUser
  }
  deriving (Eq)

data TopMenuBarInMainMenuProps = TopMenuBarInMainMenuProps
  { _tmbimmpMainMenuTab    :: MainMenuTabProps
  , _tmbimmpCurrentUser    :: CurrentUser
  }
  deriving (Eq)

data MainMenuProcessShortProps = MainMenuProcessShortProps
  { _mmprocShrtID          :: ID VDoc
  , _mmprocShrtIcon        :: ()  -- FIXME
  , _mmprocShrtTitle       :: Title
  , _mmprocShrtNumComments :: Int
  , _mmprocShrtNumEdits    :: Int
  , _mmprocShrtNumUsers    :: Int
  }
  deriving (Eq, Show, Generic)


makeRefineTypes [ ''MainMenuAction, ''MainMenuErrors, ''MainMenuState
                , ''MainMenu, ''MainMenuTab, ''MainMenuGroup, ''MainMenuSubTabLogin, ''MainMenuProcessShortProps
                ]
