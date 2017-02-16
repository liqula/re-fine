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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Header.Types where

import GHC.Generics (Generic)

import Refine.Prelude.TH (makeRefineType)


data HeaderAction =
    ToggleCommentToolbarExtension
  | StartTextSpecificComment
  | CloseCommentToolbarExtension
  | ToggleEditToolbarExtension
  deriving (Show, Generic)

data CommentToolbarExtensionStatus =
    CommentToolbarExtensionClosed
  | CommentToolbarExtensionWithButtons
  | CommentToolbarExtensionWithSelection
  deriving (Show, Generic, Eq)

data HeaderState = HeaderState
  { _hsCommentToolbarExtensionStatus     :: CommentToolbarExtensionStatus
  , _hsEditToolbarExtensionIsVisible     :: Bool  -- FIXME: since comment and edit are mutually
                                                  -- exclusive, it shouldn't be possible to set the
                                                  -- two independently.  make this two constructors
                                                  -- `HeaderStateComment`, `HeaderStateEdit`.
  } deriving (Show, Generic)

emptyHeaderState :: HeaderState
emptyHeaderState = HeaderState CommentToolbarExtensionClosed False


makeRefineType ''HeaderAction
makeRefineType ''CommentToolbarExtensionStatus
makeRefineType ''HeaderState
