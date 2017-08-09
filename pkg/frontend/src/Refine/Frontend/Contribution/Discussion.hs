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
import           Data.Foldable (toList)
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
  if props ^. discPropsFlatView
    then statementList props
    else statementForest_ 0 (0, [props ^. discPropsDiscussion . discussionTree], props ^. discPropsDetails)
  where
    statementList props =
      forM_ (zip [0..] list) $ \(i', stmnt) -> do
        statement_ i' (0, stmnt, props ^. discPropsDetails)
        br_ []
      where
        list = sortBy (flip compare `on` creationtime) . toList $ props ^. discPropsDiscussion . discussionTree
        creationtime = (^. statementMetaID . miMeta . metaCreatedAt)

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

type StatementForestProps = (Int, Tree.Forest Statement, StatementPropDetails)
type StatementProps = (Int, Statement, StatementPropDetails)

statementForest :: HasCallStack => View '[StatementForestProps]
statementForest = mkView "StatementForest" $ \(depth, frst, details) -> do
  forM_ (zip [0..] frst) $ \(i', Tree.Node stmnt chldrn) -> do
    statement_ i' (depth, stmnt, details)
    br_ []
    statementForest_ i' (depth + 1, chldrn, details)

statementForest_ :: HasCallStack => Int -> StatementForestProps -> ReactElementM eventHandler ()
statementForest_ i = view_ statementForest $ "statementForest-" <> cs (show i)

statementEditor :: StatementEditorProps -> View '[]
statementEditor (StatementEditorProps sid r modif) = mkPersistentStatefulView "statementEditor" r $ \txt -> do
    div_ ["style" @@= [ decl "border" (Ident "solid 1px black"),
                        decl "clear" (Ident "both")]] $
      contributionDialogTextFormInner 400 20 createStatementText txt
    br_ []
    ibutton_
      $ emptyIbuttonProps label
          [ DocumentAction . ReplyStatement modif sid $ FormComplete txt
          , AddStatement modif sid $ BeforeAjax txt
          ]
      & ibLabel .~ label
      & ibSize .~ Medium
      & ibListKey .~ "0"
    ibutton_
      $ emptyIbuttonProps "Cancel"
          [ DocumentAction $ ReplyStatement modif sid FormCancel
          ]
      & ibLabel .~ "Cancel"
      & ibSize .~ Medium
      & ibListKey .~ "1"
  where
    label = if modif then "Edit" else "Reply"

statement :: HasCallStack => View '[StatementProps]
statement = mkView "statement" $ \(depth, stmnt, StatementPropDetails meditor currentuser names) -> do

  let thisuser = getUser (stmnt ^. statementMetaID . miMeta . metaCreatedBy) == currentuser
      createtime = stmnt ^. statementMetaID . miMeta . metaCreatedAt
      modtime = stmnt ^. statementMetaID . miMeta . metaChangedAt

  div_ ["style" @@= [ decl "marginLeft" . Px $ 20 * depth ]] $ do

    div_ ["style" @@= [ decl "border" (Ident "solid 2px black"),
                        decl "clear" (Ident "both")]] $ do
      case meditor of
        Just e | e ^. sepStatementID == stmnt ^. statementID && e ^. sepUpdate
               -> view_ (statementEditor e) "statementEditor_"
        _ -> elemText $ stmnt ^. statementText
      br_ []
      div_ ["style" @@= [ decl "clear" (Ident "both") ]] .
        elemText $ "by " <> showUser names (stmnt ^. statementMetaID . miMeta . metaCreatedBy)
                <> " at " <> showTime createtime
                <> if createtime == modtime then "" else ", last edited at " <> showTime modtime
    br_ []
    case meditor of
      Just e
        | e ^. sepStatementID == stmnt ^. statementID && not (e ^. sepUpdate)
             -> view_ (statementEditor e) "statementEditor_"
        | otherwise -> mempty
      _ -> do
        when thisuser . ibutton_
          $ emptyIbuttonProps "Edit"
              [ LoginGuardStash
                [ DocumentAction . ReplyStatement True (stmnt ^. statementID) . FormBegin
                $ newLocalStateRef (CreateStatement $ stmnt ^. statementText) stmnt]
              ]
          & ibLabel .~ "Edit"
          & ibSize .~ Medium
          & ibListKey .~ "0"
        ibutton_
          $ emptyIbuttonProps "Reply"
              [ LoginGuardStash
                [ DocumentAction . ReplyStatement False (stmnt ^. statementID) . FormBegin
                $ newLocalStateRef (CreateStatement "") stmnt]
              ]
          & ibLabel .~ "Reply"
          & ibSize .~ Medium
          & ibListKey .~ "1"
  where
    getUser = \case
      UserID i -> Just i
      _ -> Nothing

    showUser names = \case
      UserID i  -> fromMaybe (cs . show $ cacheMiss CacheKeyUser i) $ M.lookup i names
      UserIP ip -> ip
      Anonymous -> "anonymous"

    showTime (Timestamp t) = cs $ formatTime defaultTimeLocale "%F %T" t

statement_ :: HasCallStack => Int -> StatementProps -> ReactElementM eventHandler ()
statement_ i = view_ statement $ "statement-" <> cs (show i)
