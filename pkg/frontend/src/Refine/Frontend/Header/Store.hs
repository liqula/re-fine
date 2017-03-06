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

import           Control.Lens ((&), (%~))

import Refine.Frontend.Header.Types
import Refine.Frontend.Types

headerStateUpdate :: RefineAction -> HeaderState -> HeaderState
headerStateUpdate action state =
  let newState = state
                  & hsToolbarExtensionStatus     %~ toolbarExtensionUpdate action
  in newState

---------------------------------------------------------------------------

toolbarExtensionUpdate :: RefineAction -> ToolbarExtensionStatus -> ToolbarExtensionStatus
toolbarExtensionUpdate action state = case (state, action) of
    (ToolbarExtensionClosed,               HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithButtons
    (CommentToolbarExtensionWithButtons,   HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithSelection, HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (EditToolbarExtension,                 HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithButtons

    (CommentToolbarExtensionWithButtons,   HeaderAction StartTextSpecificComment)      -> CommentToolbarExtensionWithSelection
    (_,                                    HeaderAction StartTextSpecificComment)      -> error "text-specific comment cannot start when toolbar extension is closed or in selection mode"

    (ToolbarExtensionClosed,               HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (CommentToolbarExtensionWithButtons,   HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (CommentToolbarExtensionWithSelection, HeaderAction ToggleEditToolbarExtension)    -> EditToolbarExtension
    (EditToolbarExtension,                 HeaderAction ToggleEditToolbarExtension)    -> ToolbarExtensionClosed

    (EditToolbarExtension,                 HeaderAction (StartEdit _))                 -> ToolbarExtensionClosed
    (_,                                    HeaderAction (StartEdit _))                 -> error "edit cannot start when toolbar extension is closed or in comment mode"

    (_,                                    HeaderAction CloseToolbarExtension)         -> ToolbarExtensionClosed

    _ -> state
