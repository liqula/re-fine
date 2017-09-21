{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.Discussion (discussion_) where
#include "import_frontend.hs"

import           Data.Foldable (toList)
import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Store.Types
import           Refine.Frontend.ThirdPartyViews (draftEditor_)
import           Refine.Frontend.Types
import           Refine.Frontend.Util


discussion :: HasCallStack => View '[DiscussionProps]
discussion = mkView "Discussion" $ \props -> case props ^. discPropsDiscussion of
  Left _ -> hourglass
  Right (_range, disc) -> do
    aboutText_ ( props ^. discPropsAboutText
               , ContribIDDiscussion False $ disc ^. discussionMetaID . miID
               )
    if props ^. discPropsFlatView
      then statementList
      else statementForest_ 0 (0, [disc ^. discussionTree], props ^. discPropsDetails)
   where
    statementList =
      forM_ (zip [0..] list) $ \(i', stmnt) -> do
        statement_ i' (0, stmnt, props ^. discPropsDetails)
        br_ []
      where
        list = sortBy (flip compare `on` creationtime) . toList $ disc ^. discussionTree
        creationtime = (^. statementMetaID . miMeta . metaCreatedAt)

discussion_ :: HasCallStack => DiscussionProps -> ReactElementM eventHandler ()
discussion_ = view_ discussion "discussion_"

aboutText :: HasCallStack => View '[(RawContent, ContributionID)]
aboutText = mkView "AboutText" $ \(rc, did) -> do
  h1_ "Related Text:"
  draftEditor_
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
statementEditor (StatementEditorProps sid r modif) = mkPersistentStatefulView "statementEditor" r Nothing $ \txt -> do
    div_ ["style" @@= [ decl "border" (Ident "solid 1px black"),
                        decl "clear" (Ident "both")]] $
      contributionDialogTextFormInner 400 20 createStatementText txt
    br_ []
    ibutton_
      $ emptyIbuttonProps (ButtonImageIcon (if modif then Svg.Edit else Svg.Reply) ColorSchemaBright)
          [ DocumentAction . ReplyStatement modif sid $ FormComplete txt
          , AddStatement modif sid txt
          ]
      & ibListKey .~ "0"
      & ibSize .~ Medium
    ibutton_
      $ emptyIbuttonProps (ButtonImageIcon Svg.Close ColorSchemaBright)
          [ DocumentAction $ ReplyStatement modif sid FormCancel
          ]
      & ibListKey .~ "1"
      & ibSize .~ Medium

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
          $ emptyIbuttonProps (ButtonImageIcon Svg.Edit ColorSchemaBright)
              [ LoginGuardStash
                [ DocumentAction . ReplyStatement True (stmnt ^. statementID) . FormBegin
                $ newLocalStateRef (CreateStatement $ stmnt ^. statementText) stmnt]
              ]
          & ibSize .~ Medium
          & ibListKey .~ "0"
        ibutton_
          $ emptyIbuttonProps (ButtonImageIcon Svg.Reply ColorSchemaBright)
              [ LoginGuardStash
                [ DocumentAction . ReplyStatement False (stmnt ^. statementID) . FormBegin
                $ newLocalStateRef (CreateStatement "") stmnt]
              ]
          & ibSize .~ Medium
          & ibListKey .~ "1"
  where
    getUser = \case
      UserID i -> Just i
      _ -> Nothing

    showUser names = \case
      UserID i  -> fromMaybe (cs . show $ cacheMiss (CacheKeyUser i) i i) $ Map.lookup i names
      UserIP ip -> ip
      Anonymous -> "anonymous"

    showTime (Timestamp t) = cs $ formatTime defaultTimeLocale "%F %T" t

statement_ :: HasCallStack => Int -> StatementProps -> ReactElementM eventHandler ()
statement_ i = view_ statement $ "statement-" <> cs (show i)
