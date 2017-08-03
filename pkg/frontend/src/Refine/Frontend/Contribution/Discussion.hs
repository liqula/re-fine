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
import           Refine.Frontend.Contribution.Dialog
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
  statementForest_ 0 (0, [props ^. discPropsDiscussion . discussionTree], props ^. discPropsEditor, props ^. discPropsUserNames)

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

type StatementForestProps = (Int, Tree.Forest Statement, Maybe StatementEditorProps, Map (ID User) Username)

statementForest :: HasCallStack => View '[StatementForestProps]
statementForest = mkView "StatementForest" $ \(depth, frst, seditor, names) -> do
  forM_ (zip [0..] frst) $ \(i', Tree.Node stmnt chldrn) -> do
    statement_ i' (depth, stmnt, seditor, names)
    br_ []
    statementForest_ i' (depth + 1, chldrn, seditor, names)

statementForest_ :: HasCallStack => Int -> StatementForestProps -> ReactElementM eventHandler ()
statementForest_ i = view_ statementForest $ "statementForest-" <> cs (show i)

statementEditor :: Int -> StatementEditorProps -> View '[]
statementEditor depth (sid, r) = mkPersistentStatefulView "statementEditor" r $ \txt -> do
    elemString $ "depth " <> show depth <> " statement reply"
    br_ []
    contributionDialogTextFormInner 400 20 createStatementText txt
    br_ []
    ibutton_
      $ emptyIbuttonProps "Reply"
          [ DocumentAction . ReplyStatement sid $ FormComplete txt
          , AddStatement sid $ BeforeAjax txt
          ]
      & ibLabel .~ "Reply"
      & ibSize .~ XXLarge

type StatementProps = (Int, Statement, Maybe StatementEditorProps, Map (ID User) Username)

statement :: HasCallStack => View '[StatementProps]
statement = mkView "statement" $ \(depth, stmnt, meditor, names) -> div_ $ do
  elemString $ "depth " <> show depth <> " statement"
  br_ []
  elemText $ stmnt ^. statementText
  br_ []
  elemText $ "by " <> showUser names (stmnt ^. statementMetaID . miMeta . metaCreatedBy)
  br_ []
  elemString $ "created at " <> show (stmnt ^. statementMetaID . miMeta . metaCreatedAt)
  br_ []
  elemString $ "modified at " <> show (stmnt ^. statementMetaID . miMeta . metaChangedAt)
  br_ []
  case meditor of
    Just e | fst e == stmnt ^. statementID
           -> view_ (statementEditor depth e) "statementEditor_"
    _ -> ibutton_
        $ emptyIbuttonProps "Reply"
            [ LoginGuardStash
              [ DocumentAction . ReplyStatement (stmnt ^. statementID) . FormOngoing
              $ newLocalStateRef (CreateStatement "") stmnt]
            ]
        & ibLabel .~ "Reply"
        & ibSize .~ XXLarge
  where
    showUser names = \case
      UserID i  -> fromMaybe (error "user is not in the cache") $ M.lookup i names
      UserIP ip -> ip
      Anonymous -> "anonymous"

statement_ :: HasCallStack => Int -> StatementProps -> ReactElementM eventHandler ()
statement_ i = view_ statement $ "statement-" <> cs (show i)
