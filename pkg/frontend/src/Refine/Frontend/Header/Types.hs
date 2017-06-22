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
import Refine.Prelude.TH (makeRefineType)


data HeaderAction =
    ToggleCommentToolbarExtension
  | StartTextSpecificComment
  | StartEdit EditKind
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


data DiffToolbarProps = DiffToolbarProps
  { _diffToolbarPropsEditID :: ID Edit
  , _diffToolbarPropsVotes  :: VoteCount
  } deriving (Show, Eq, Generic)

instance UnoverlapAllEq DiffToolbarProps


makeRefineType ''HeaderAction
makeRefineType ''ToolbarExtensionStatus
makeRefineType ''HeaderState
makeRefineType ''AddLinkFormState
makeRefineType ''DiffToolbarProps
