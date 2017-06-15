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

import Refine.Common.Types (EditKind)
import Refine.Prelude.TH (makeRefineType)


data HeaderAction =
    ToggleCommentToolbarExtension
  | StartTextSpecificComment
  | ToggleEditToolbarExtension
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
  | EditToolbarExtension
  | EditToolbarLinkEditor ST
  deriving (Show, Eq, Generic)

data HeaderState = HeaderState
  { _hsReadOnly               :: Bool
  , _hsToolbarExtensionStatus :: ToolbarExtensionStatus
  } deriving (Show, Eq, Generic)

emptyHeaderState :: HeaderState
emptyHeaderState = HeaderState False ToolbarExtensionClosed

newtype AddLinkFormState = AddLinkFormState
  { _addLinkFormState :: ST
  } deriving (Show, Eq, Generic)


makeRefineType ''HeaderAction
makeRefineType ''ToolbarExtensionStatus
makeRefineType ''HeaderState
makeRefineType ''AddLinkFormState
