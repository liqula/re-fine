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


module Refine.Frontend.Header.Store
  ( headerStateUpdate
  ) where

import Refine.Frontend.Prelude

import Control.Lens ((&), (%~))

import Refine.Frontend.Header.Types
import Refine.Frontend.Store.Types


headerStateUpdate :: GlobalAction -> HeaderState -> HeaderState
headerStateUpdate action st = st
  & hsToolbarExtensionStatus %~ toolbarExtensionUpdate action


toolbarExtensionUpdate :: GlobalAction -> ToolbarExtensionStatus -> ToolbarExtensionStatus
toolbarExtensionUpdate action st = case (st, action) of
    (ToolbarExtensionClosed,               HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithoutRange
    (CommentToolbarExtensionWithoutRange,  HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithRange,     HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (EditToolbarExtension,                 HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithoutRange

    (CommentToolbarExtensionWithoutRange,  HeaderAction StartTextSpecificComment)      -> CommentToolbarExtensionWithRange
    (_,                                    HeaderAction StartTextSpecificComment)      -> error "text-specific comment cannot start when toolbar extension is closed or in selection mode"

    (ToolbarExtensionClosed,               HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (CommentToolbarExtensionWithoutRange,  HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (CommentToolbarExtensionWithRange,     HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (EditToolbarExtension,                 HeaderAction ToggleEditToolbarExtension)    -> ToolbarExtensionClosed

    (EditToolbarExtension,                 HeaderAction (StartEdit _))                 -> ToolbarExtensionClosed
    (_,                                    HeaderAction (StartEdit _))                 -> error "edit cannot start when toolbar extension is closed or in comment mode"

    (_,                                    HeaderAction CloseToolbarExtension)         -> ToolbarExtensionClosed

    _ -> st
