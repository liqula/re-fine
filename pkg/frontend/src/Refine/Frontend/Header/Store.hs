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

import Refine.Frontend.Contribution.Types
import Refine.Frontend.Header.Types
import Refine.Frontend.Store.Types


headerStateUpdate :: HasCallStack => GlobalAction -> HeaderState -> HeaderState
headerStateUpdate action st = st
  & hsReadOnly               %~ readOnlyUpdate action
  & hsToolbarExtensionStatus %~ toolbarExtensionUpdate action

readOnlyUpdate :: HasCallStack => GlobalAction -> Bool -> Bool
readOnlyUpdate (HeaderAction ToggleReadOnly) = not
readOnlyUpdate _                             = id

toolbarExtensionUpdate :: HasCallStack => GlobalAction -> ToolbarExtensionStatus -> ToolbarExtensionStatus
toolbarExtensionUpdate action st = case (st, action) of
    (ToolbarExtensionClosed,               HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithoutRange
    (CommentToolbarExtensionWithoutRange,  HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithRange,     ContributionAction ShowCommentEditor)       -> ToolbarExtensionClosed
    (CommentToolbarExtensionWithRange,     HeaderAction ToggleCommentToolbarExtension) -> ToolbarExtensionClosed

    (CommentToolbarExtensionWithoutRange,  HeaderAction StartTextSpecificComment)      -> CommentToolbarExtensionWithRange
    (_,                                    HeaderAction StartTextSpecificComment)      -> bad1

    (ToolbarExtensionClosed,               HeaderAction (OpenEditToolbarLinkEditor l)) -> EditToolbarLinkEditor l
    (_,                                    HeaderAction (OpenEditToolbarLinkEditor _)) -> ToolbarExtensionClosed

    (_,                                    HeaderAction CloseToolbarExtension)         -> ToolbarExtensionClosed

    _ -> st
  where
    bad1 = error $ "text-specific comment cannot start when toolbar extension is closed or in selection mode: " <> show (action, st)
