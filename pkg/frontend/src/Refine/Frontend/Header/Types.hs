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
  | ToggleEditToolbarExtension
  | StartTextSpecificComment
  | FinishTextSpecificComment
  deriving (Show, Generic)

data TextSpecificComment =
    TextSpecificCommentInProgress
  | TextSpecificCommentInactive
  deriving (Show, Generic)

data HeaderState = HeaderState
  { _hsCommentToolbarExtensionIsVisible     :: Bool
  , _hsEditToolbarExtensionIsVisible        :: Bool
  , _hsTextSpecificComment                  :: TextSpecificComment
  } deriving (Show, Generic)

emptyHeaderState :: HeaderState
emptyHeaderState = HeaderState False False TextSpecificCommentInactive


makeRefineType ''HeaderAction
makeRefineType ''TextSpecificComment
makeRefineType ''HeaderState
