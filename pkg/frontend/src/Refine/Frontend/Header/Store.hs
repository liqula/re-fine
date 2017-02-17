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
    (CommentToolbarExtensionClosed, HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithButtons
    (_,                             HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionClosed
    (_,                             HeaderAction CloseCommentToolbarExtension)  -> CommentToolbarExtensionClosed
    (CommentToolbarExtensionWithButtons, HeaderAction StartTextSpecificComment) -> CommentToolbarExtensionWithSelection
    (_,                                  HeaderAction StartTextSpecificComment) -> error "text-specific comment cannot start when toolbar extension is closed or in selection mode"
    _ -> state
