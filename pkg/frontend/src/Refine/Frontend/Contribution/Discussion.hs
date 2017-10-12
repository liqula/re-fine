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
  Right (_range, disc) -> div_ ["className" $= "discussion-thread-container"] $ do
    view_ aboutText "aboutText_"
      ( props ^. discPropsAboutText
      , ContribIDDiscussion False $ disc ^. discussionMetaID . miID
      )

    let treeprops = (disc ^. discussionTree, props ^. discPropsDetails)
        component = if props ^. discPropsFlatView then statementList else statementRoot
    xview_ component treeprops

discussion_ :: HasCallStack => DiscussionProps -> ReactElementM eventHandler ()
discussion_ = view_ discussion "discussion_"


-- * what the discussion references

aboutText :: HasCallStack => View '[(RawContent, ContributionID)]
aboutText = mkView "AboutText" $ \(rc, did) -> div_ ["className" $= "discussion-thread-container__text"] $ do
  draftEditor_
    [ "editorState" &= createWithRawContent rc
    , "editorKey" $= "dt2h6"  -- (this makes the styleguide generation deterministic.)
    , "customStyleMap" &= mkDocumentStyleMap [MarkContribution did 0] (Just rc)
    , "readOnly" &= True
    ] mempty


-- * flat discussion view

-- FIXME: this should be "notes".  not implemented yet (neither in styleguide-new nor here).
statementList :: HasCallStack => View '[(Tree.Tree Statement, StatementPropDetails)]
statementList = mkView "StatementList" $ \(tree, details) ->
  div_ ["className" $= "discussion-thread-container__tree"] $ do
    let list = sortBy (flip compare `on` creationtime) (toList tree)
        creationtime = (^. statementMetaID . miMeta . metaCreatedAt)
    (xview_ statementViewer . (, details)) `mapM_` list


-- * tree-shaped discussion view

statementRoot :: HasCallStack => View '[(Tree.Tree Statement, StatementPropDetails)]
statementRoot = mkView "StatementForest" $ \(tree, details) ->
  div_ ["className" $= "discussion-thread-container__tree"] .
    div_ ["className" $= "discussion-thread-root"] $
      statementSubtree_ details tree

statementChild :: HasCallStack => View '[(Tree.Tree Statement, StatementPropDetails)]
statementChild = mkView "StatementForest" $ \(tree, details) ->
  div_ ["className" $= "discussion-thread-child"] $ do
    div_ ["className" $= "discussion-thread-child__icon-column"] $ do
      ibutton_
        $ emptyIbuttonProps (ButtonImageIcon Svg.DiscussionTreeChild ColorSchemaDiscussion)
          ([] :: [GlobalAction])
          & ibListKey .~ "0"
          & ibSize .~ Large
    div_ ["className" $= "discussion-thread-child__node-column"] $ do
      statementSubtree_ details tree

-- | (This is a list of more than one DOM sibling, so it can't be a 'View'.)
statementSubtree_ :: HasCallStack => StatementPropDetails -> Tree.Tree Statement -> ReactElementM 'EventHandlerCode ()
statementSubtree_ details (Tree.Node stmnt chldrn) = do
  case details ^. spdEditorProps of
          Just e | e ^. sepStatementID == stmnt ^. statementID && e ^. sepUpdate
            -> xview_ (statementEditor e)
          _ -> xview_ statementViewer (stmnt, details)
  (xview_ statementChild . (, details)) `mapM_` chldrn


-- * item (statement editor or viewer)

statementEditor :: StatementEditorProps -> View '[]
statementEditor (StatementEditorProps sid r modif) =
  mkPersistentStatefulView "statementEditor" r Nothing $ \txt -> do
    --div_ ["className" $= "content-box c_bg_note_bubble"] $ do
      div_ ["className" $= "content-box__section"] $ do
        div_ ["className" $= "left-column"] $ do
          div_ ["className" $= "inner-column-1"] $ do
            ibutton_
              $ emptyIbuttonProps (ButtonImageIcon Svg.Close ColorSchemaBright)
                  [DocumentAction $ ReplyToOrUpdateStatement modif sid FormCancel]
              & ibListKey .~ "0"
              & ibSize .~ Medium
          div_ ["className" $= "inner-column-1"] $ do
            ibutton_
              $ emptyIbuttonProps (ButtonImageIcon Svg.Help ColorSchemaBright)
                  [ShowNotImplementedYet]
              & ibListKey .~ "1"
              & ibSize .~ Medium

      div_ ["className" $= "content-box__hr"] $ pure ()
      div_ ["className" $= "content-box__form-div"] $ do
        div_ ["className" $= "content-box__form-ibutton-div"] $ do
          ibutton_
            $ emptyIbuttonProps (ButtonImageIcon Svg.Save ColorSchemaBright)
                [ DocumentAction . ReplyToOrUpdateStatement modif sid $ FormComplete txt
                , AddStatement modif sid txt
                ]
                & ibSize .~ XLarge
        textarea_ [ "className" $= "content-box__form-textarea"
                  , onChange $ \evt ->
                      -- Update the current state with the current text in the textbox, sending no actions
                      simpleHandler $ \txt' -> ([], Just $ txt' & createStatementText .~ target evt "value")
                  , "value" $= cs (txt ^. createStatementText)
                  ] $ do
          pure ()

type StatementViewerProps = (Statement, StatementPropDetails)

statementViewer :: View '[StatementViewerProps]
statementViewer = mkView "statementViewer" $ \(stmnt, StatementPropDetails meditor names) ->
  div_ ["className" $= "content-box c_bg_note_bubble"] $ do
    div_ ["className" $= "content-box__header"] $ do
      div_ ["className" $= "left-column"] $ do
        div_ ["className" $= "inner-column-1"] $ do
          ibutton_
            $ emptyIbuttonProps (ButtonImageIcon Svg.Discussion ColorSchemaDiscussion)
            ([] :: [GlobalAction])
            & ibListKey .~ "0"
            & ibSize .~ Medium

        div_ ["className" $= "inner-column-1"] $ do
          ibutton_  -- FIXME: get user image if available.  make that a little component?
            $ emptyIbuttonProps (ButtonImageIcon Svg.User ColorSchemaDiscussion)
            ([] :: [GlobalAction])
            & ibListKey .~ "1"
            & ibSize .~ Medium

        div_ ["className" $= "inner-column-1 content-box__header__label"] $ do
          authorInfo (stmnt ^. statementMetaID . miMeta) names

      --div_ ["className" $= "discussion-thread-item__header-inner fisx-css-toolbar-flex c-vdoc-toolbar"] $ do
      div_ ["className" $= "right-column"] $ do
          let expanded = True  -- FIXME: implement partial thread collapse.
                               -- FIXME: more general name for Svg.DiffExpand, Svg.DiffCollapse.
          if expanded
            then do
              div_ ["className" $= "inner-column-1 content-box__header__label"] $ do
                "collapse thread"
              div_ ["className" $= "inner-column-1"] $ do
                ibutton_
                  $ emptyIbuttonProps (ButtonImageIcon Svg.DiffCollapse ColorSchemaDiscussion)
                  [ShowNotImplementedYet]
                  & ibListKey .~ "0"
                  & ibSize .~ Medium
            else do
              div_ ["className" $= "inner-column-1 content-box__header__label"] $ do
                "expand thread"
              div_ ["className" $= "inner-column-1"] $ do
                ibutton_
                  $ emptyIbuttonProps (ButtonImageIcon Svg.DiffExpand ColorSchemaDiscussion)
                  [ShowNotImplementedYet]
                  & ibListKey .~ "0"
                  & ibSize .~ Medium

    div_ ["className" $= "content-box__body"] $ do
      elemText $ stmnt ^. statementText

    let editing e = e ^. sepStatementID == stmnt ^. statementID && not (e ^. sepUpdate)
    case meditor of
      Just e | editing e -> do
        div_ ["className" $= "content-box__section"] $ do
          --div_ ["style" @@= [decl "height" (Px 6)]] $ do
            pure ()
        xview_ (statementEditor e)
      _ -> do
        div_ ["className" $= "content-box__section"] $ do
          div_ ["className" $= "left-column"] $ do
            -- FIXME: @AP.createOrUpdateComment $ stmnt ^. statementVDoc@, but now we need to look up
            -- the vdoc under the id :(.  so it seems like the access policy terms need to be with less
            -- data.  e.g., replace vdoc by the group it's in.  that should be sufficient, and then the
            -- information is already in global access state.
            replyButton_ stmnt
            -- FIXME: comment in #358: @guard (AP.updateStatement stmnt) $ ...@
            updateButton_ stmnt

          div_ ["className" $= "right-column"] $ do
            voteUpDownReportButtons

  where
    replyButton_ :: Statement -> ReactElementM 'EventHandlerCode ()
    replyButton_ stmnt = btn stmnt Svg.Reply False "" "reply"

    updateButton_ :: Statement -> ReactElementM 'EventHandlerCode ()
    updateButton_ stmnt = btn stmnt Svg.Edit True (stmnt ^. statementText) "update"

    btn :: Statement -> Svg.Icon -> Bool -> CommentText -> ReactElementM 'EventHandlerCode ()
        -> ReactElementM 'EventHandlerCode ()
    btn stmnt svg isupdate contents label = do
      let props = emptyIbuttonProps (ButtonImageIcon svg ColorSchemaDark) act & ibSize .~ Large
          act = [ DocumentAction . ReplyToOrUpdateStatement isupdate (stmnt ^. statementID) . FormBegin
                  $ newLocalStateRef (CreateStatement contents) stmnt
                ]
      div_ ["className" $= "inner-column-1"] $ do
        xview_ (ibutton (props ^. ibPressed)) props
      div_ ["className" $= "inner-column-1"] $ do
        div_ ["className" $= "content-box__section__label"] $ do
          label

    voteUpDownReportButtons = do
      div_ ["className" $= "inner-column-1"] $ do
        ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VotePositive ColorSchemaDiscussion) [ShowNotImplementedYet]
          & ibListKey .~ "voteup"
          -- & ibIndexNum .~ Just 0
          & ibSize .~ Large

      div_ ["className" $= "inner-column-1"] $ do
        ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.VoteNegative ColorSchemaDiscussion) [ShowNotImplementedYet]
          & ibListKey .~ "votedown"
          -- & ibIndexNum .~ Just 0
          & ibSize .~ Large

      --div_ ["className" $= "content-box__hr"] $ pure ()

      div_ ["className" $= "inner-column-1"] $ do
        ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Report ColorSchemaDiscussion) [ShowNotImplementedYet]
          & ibListKey .~ "report"
          & ibSize .~ Large

authorInfo :: MetaInfo -> Map (ID User) Username -> ReactElementM h ()
authorInfo metaInfo names = elemText . mconcat $
  [ "author: " <> showUser (metaInfo ^. metaCreatedBy)
  , ", created: " <> showTime createtime
  ] <>
  [ ", last changed: " <> showTime modtime | createtime /= modtime ]
  where
    showUser = \case
      UserID i  -> fromMaybe (cs . show $ cacheMiss (CacheKeyUser i) i i) $ Map.lookup i names
      UserIP ip -> ip
      Anonymous -> "anonymous"

    showTime (Timestamp t) = cs $ formatTime defaultTimeLocale "%F %T" t

    createtime = metaInfo ^. metaCreatedAt
    modtime = metaInfo ^. metaChangedAt
