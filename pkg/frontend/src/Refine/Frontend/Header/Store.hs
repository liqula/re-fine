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
  & hsReadOnly               %~ readOnlyUpdate action
  & hsToolbarExtensionStatus %~ toolbarExtensionUpdate action

readOnlyUpdate :: GlobalAction -> Bool -> Bool
readOnlyUpdate (HeaderAction ToggleReadOnly) = not
readOnlyUpdate _                             = id

toolbarExtensionUpdate :: GlobalAction -> ToolbarExtensionStatus -> ToolbarExtensionStatus
toolbarExtensionUpdate action st = case (st, action) of
    (ToolbarExtensionClosed,               HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithoutRange
    (CommentToolbarExtensionWithoutRange,  HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithRange,     HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (EditToolbarExtension,                 HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithoutRange

    (CommentToolbarExtensionWithoutRange,  HeaderAction StartTextSpecificComment)      -> CommentToolbarExtensionWithRange
    (_,                                    HeaderAction StartTextSpecificComment)      -> bad1

    (ToolbarExtensionClosed,               HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (CommentToolbarExtensionWithoutRange,  HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (CommentToolbarExtensionWithRange,     HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (EditToolbarExtension,                 HeaderAction ToggleEditToolbarExtension)    -> ToolbarExtensionClosed

    (EditToolbarExtension,                 HeaderAction (StartEdit _))                 -> ToolbarExtensionClosed
    (_,                                    HeaderAction (StartEdit _))                 -> bad2

    (ToolbarExtensionClosed,               HeaderAction (OpenEditToolbarLinkEditor l)) -> EditToolbarLinkEditor l
    (_,                                    HeaderAction (OpenEditToolbarLinkEditor _)) -> bad3

    (_,                                    HeaderAction CloseToolbarExtension)         -> ToolbarExtensionClosed

    _ -> st
  where
    bad1 = error "text-specific comment cannot start when toolbar extension is closed or in selection mode"
    bad2 = error "edit cannot start when toolbar extension is closed or in comment mode"
    bad3 = error "link editor can be opened only in edit toolbar extension closed mode"
