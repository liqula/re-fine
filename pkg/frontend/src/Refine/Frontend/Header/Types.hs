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

import GHC.Generics (Generic)

import Refine.Common.Types (EditKind)
import Refine.Prelude.TH (makeRefineType)


data HeaderAction =
    ToggleCommentToolbarExtension
  | StartTextSpecificComment
  | ToggleEditToolbarExtension
  | StartEdit EditKind
  | CloseToolbarExtension
  deriving (Show, Generic)

data ToolbarExtensionStatus =
    ToolbarExtensionClosed
  | CommentToolbarExtensionWithButtons
  | CommentToolbarExtensionWithSelection
  | EditToolbarExtension
  deriving (Show, Generic, Eq)

newtype HeaderState = HeaderState
  { _hsToolbarExtensionStatus     :: ToolbarExtensionStatus
  } deriving (Show, Generic)

emptyHeaderState :: HeaderState
emptyHeaderState = HeaderState ToolbarExtensionClosed


makeRefineType ''HeaderAction
makeRefineType ''ToolbarExtensionStatus
makeRefineType ''HeaderState
