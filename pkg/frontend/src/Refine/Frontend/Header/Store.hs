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
headerStateUpdate act st = st
  & hsReadOnly               %~ readOnlyUpdate act
  & hsToolbarExtensionStatus %~ toolbarExtensionUpdate act

readOnlyUpdate :: HasCallStack => GlobalAction -> Bool -> Bool
readOnlyUpdate (HeaderAction ToggleReadOnly) = not
readOnlyUpdate _                             = id

toolbarExtensionUpdate :: HasCallStack => GlobalAction -> ToolbarExtensionStatus -> ToolbarExtensionStatus
toolbarExtensionUpdate act st = case (st, act) of
    (ToolbarExtensionClosed,               HeaderAction ToggleIndexToolbarExtension)   -> IndexToolbarExtension
    (IndexToolbarExtension,                HeaderAction ToggleIndexToolbarExtension)   -> ToolbarExtensionClosed
    (_,                                    HeaderAction ScrollToBlockKey{})            -> ToolbarExtensionClosed

    (ToolbarExtensionClosed,               HeaderAction ToggleCommentToolbarExtension) -> CommentToolbarExtensionWithoutRange
    (CommentToolbarExtensionWithoutRange,  ContributionAction ShowCommentEditor)       -> ToolbarExtensionClosed
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
    bad1 = error $ "text-specific comment cannot start when toolbar extension is closed or in selection mode: " <> show (act, st)
