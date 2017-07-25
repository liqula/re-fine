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
  aboutText_ (props ^. discPropsAboutText, props ^. discPropsDiscussion . discussionRange)
  statementTree_ (props ^. discPropsDiscussion . discussionTree)

discussion_ :: HasCallStack => DiscussionProps -> ReactElementM eventHandler ()
discussion_ = view_ discussion "discussion_"

aboutText :: HasCallStack => View '[(RawContent, Range Position)]
aboutText = mkView "AboutText" $ \(_raw, _range) -> do
  h1_ "Related Text:"
  editor_
    [ "editorState" &= True  -- TODO
--     , "customStyleMap" &= documentStyleMap  -- TODO
    , "readOnly" &= True
    ] mempty

aboutText_ :: HasCallStack => (RawContent, Range Position) -> ReactElementM eventHandler ()
aboutText_ = view_ aboutText "aboutText_"

statementTree :: HasCallStack => View '[Tree.Tree Statement]
statementTree = mkView "statementTree" $ \_tree -> do
  undefined  -- TODO

statementTree_ :: HasCallStack => Tree.Tree Statement -> ReactElementM eventHandler ()
statementTree_ = view_ statementTree "statementTree_"

statement :: HasCallStack => View '[Statement]
statement = mkView "statement" $ \_statement -> do
  undefined  -- TODO

statement_ :: HasCallStack => Statement -> ReactElementM eventHandler ()
statement_ = view_ statement "statement_"
