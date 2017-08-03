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
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-imports -fno-warn-unused-binds #-}  -- TODO

module Refine.Frontend.Contribution.Discussion (discussion_) where

import Refine.Frontend.Prelude

import qualified Data.Map.Strict as M
import qualified Data.Text as ST
import qualified Data.Tree as Tree
import           Language.Css.Syntax
import qualified React.Flux as RF

import           Refine.Common.Types hiding (Style)
import           React.Flux.Missing
import qualified Refine.Frontend.Colors as C
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Icon
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (editor_)
import           Refine.Frontend.TKey
import           Refine.Frontend.Types
import           Refine.Frontend.Util


discussion :: HasCallStack => View '[DiscussionProps]
discussion = mkView "Discussion" $ \props -> do
  aboutText_ ( props ^. discPropsAboutText
             , ContribIDDiscussion $ props ^. discPropsDiscussion . discussionMetaID . miID
             )
  statementForest_ (0, [props ^. discPropsDiscussion . discussionTree])

discussion_ :: HasCallStack => DiscussionProps -> ReactElementM eventHandler ()
discussion_ = view_ discussion "discussion_"

aboutText :: HasCallStack => View '[(RawContent, ContributionID)]
aboutText = mkView "AboutText" $ \(rc, did) -> do
  h1_ "Related Text:"
  editor_
    [ "editorState" &= createWithRawContent rc
    , "customStyleMap" &= mkDocumentStyleMap [MarkContribution did 0] (Just rc)
    , "readOnly" &= True
    ] mempty

aboutText_ :: HasCallStack => (RawContent, ContributionID) -> ReactElementM eventHandler ()
aboutText_ = view_ aboutText "aboutText_"

statementForest :: HasCallStack => View '[(Int, Tree.Forest Statement)]
statementForest = mkView "statementForest" $ \(depth, frst) -> do
  forM_ frst $ \(Tree.Node stmnt chldrn) -> do
    statement_ (depth, stmnt)
    statementForest_ (depth + 1, chldrn)

statementForest_ :: HasCallStack => (Int, Tree.Forest Statement) -> ReactElementM eventHandler ()
statementForest_ = view_ statementForest "statementForest_"

statement :: HasCallStack => View '[(Int, Statement)]
statement = mkView "statement" $ \(depth, stmnt) -> do
  elemString (show (depth, stmnt))

statement_ :: HasCallStack => (Int, Statement) -> ReactElementM eventHandler ()
statement_ = view_ statement "statement_"
