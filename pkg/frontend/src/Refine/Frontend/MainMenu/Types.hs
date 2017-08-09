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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.MainMenu.Types where

import Refine.Frontend.Prelude

import GHC.Generics (Generic)

import React.Flux.Missing
import Refine.Common.Types
import Refine.Common.Rest (ApiErrorCreateUser)
import Refine.Common.Types.Prelude (Username)
import Refine.Frontend.Types
import Refine.Frontend.Login.Types


data MainMenuAction
  = MainMenuActionClose
  | MainMenuActionOpen MainMenuTabAction
  | MainMenuActionLoginError        Username
  | MainMenuActionRegistrationError ApiErrorCreateUser
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
      (LocalStateRef CreateGroup)
      (LocalStateRef CreateVDoc)
      (LocalStateRef UpdateVDoc)
      :: *)
type MainMenuTabAction = (MainMenuTab
      (AjaxAction () [ID Group])
      (ID Group)
      (FormAction CreateGroup)
      (FormAction CreateVDoc)
      (FormAction UpdateVDoc)
      :: *)
type MainMenuTabProps = (MainMenuTab
      GroupsProps
      GroupProps
      (LocalStateRef CreateGroup)
      (LocalStateRef CreateVDoc)
      (LocalStateRef UpdateVDoc)
      :: *)

type GroupProps = (Group, Map (ID VDoc) VDoc)
type GroupsProps = ([Group], Map (ID VDoc) VDoc)

-- | FUTUREWORK: it may be nicer after all to have different types for action, state, and props
-- here.  but for now it should work.
data MainMenuTab gids group cgroup cprocess uprocess
  = MainMenuGroups gids
  | MainMenuGroup group
  | MainMenuCreateOrUpdateGroup (Maybe (ID Group)) cgroup
  | MainMenuCreateProcess cprocess
  | MainMenuUpdateProcess (ID VDoc) uprocess
  | MainMenuHelp
  | MainMenuLogin MainMenuSubTabLogin
  deriving (Eq, Show, Generic)

mapMainMenuTab :: (a -> a') -> (b -> b') -> (c -> c') -> (d -> d') -> (e -> e') -> MainMenuTab a b c d e -> MainMenuTab a' b' c' d' e'
mapMainMenuTab fa fb fc fd fe = \case
  MainMenuGroups a -> MainMenuGroups (fa a)
  MainMenuGroup b  -> MainMenuGroup (fb b)
  MainMenuCreateOrUpdateGroup u c -> MainMenuCreateOrUpdateGroup u (fc c)
  MainMenuCreateProcess d -> MainMenuCreateProcess (fd d)
  MainMenuUpdateProcess pid e -> MainMenuUpdateProcess pid (fe e)
  MainMenuHelp     -> MainMenuHelp
  MainMenuLogin l  -> MainMenuLogin l

defaultMainMenuTab :: MainMenuTabAction
defaultMainMenuTab = MainMenuGroups $ BeforeAjax ()

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
                , ''MainMenu, ''MainMenuTab, ''MainMenuSubTabLogin, ''MainMenuProcessShortProps
                ]
