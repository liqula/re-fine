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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.Types where

import Refine.Frontend.Prelude

import GHC.Generics (Generic)

import Refine.Common.Types
import Refine.Frontend.Login.Types


data HeaderAction =
    ToggleCommentToolbarExtension
  | StartTextSpecificComment
  | StartEdit
  | CloseToolbarExtension
  | ToggleReadOnly
  | ScrollToPageTop
  -- ScrollToDocumentTop
  | OpenEditToolbarLinkEditor ST
  deriving (Show, Eq, Generic)

data ToolbarExtensionStatus =
    ToolbarExtensionClosed
  | CommentToolbarExtensionWithoutRange
  | CommentToolbarExtensionWithRange
  | EditToolbarLinkEditor ST
  deriving (Show, Eq, Generic)

data HeaderState = HeaderState
  { _hsReadOnly               :: Bool
  , _hsToolbarExtensionStatus :: ToolbarExtensionStatus
  } deriving (Show, Eq, Generic)

emptyHeaderState :: HasCallStack => HeaderState
emptyHeaderState = HeaderState False ToolbarExtensionClosed

newtype AddLinkFormState = AddLinkFormState
  { _addLinkFormState :: ST
  } deriving (Show, Eq, Generic)


data TopMenuBarProps = TopMenuBarProps
 { _isSticky    :: Bool
 , _currentUser :: CurrentUser
 } deriving (Eq, Generic)

instance UnoverlapAllEq TopMenuBarProps


data DiffToolbarProps = DiffToolbarProps
  { _diffToolbarPropsEditID :: ID Edit
  , _diffToolbarPropsVotes  :: VoteCount
  } deriving (Show, Eq, Generic)

instance UnoverlapAllEq DiffToolbarProps


data EditToolbarProps
    = LinkButtonDisabled
    | LinkButtonDeletes
    | LinkButtonAdds ST
  deriving (Eq, Show)


makeRefineTypes [''HeaderAction, ''ToolbarExtensionStatus, ''HeaderState, ''AddLinkFormState, ''DiffToolbarProps, ''TopMenuBarProps]
