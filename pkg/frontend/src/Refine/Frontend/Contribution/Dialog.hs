{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Contribution.Dialog where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Frontend.Test.Console (gracefulError)
import           Refine.Frontend.ThirdPartyViews (skylight_)
import           Refine.Frontend.Contribution.Types
import qualified Refine.Frontend.Colors as C
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.TKey
import           Refine.Frontend.Types
import           Refine.Frontend.Util


-- * styles

dialogWidth :: HasCallStack => Int
dialogWidth = 640

leftFor :: HasCallStack => Int -> Int
leftFor windowWidth = (windowWidth - dialogWidth) `quot` 2

dialogStyles :: HasCallStack => [Decl]
dialogStyles = [ -- Style "display" ("block" :: String)
                 -- Style "minHeight" ("200px" :: String)
                 -- Style "padding" ("3rem 1.0rem 1.0rem" :: String)

                  decl "width" (Px dialogWidth)
                , decl "marginLeft" (Px 0)
                , decl "marginTop" (Px 0)
                , zindex ZIxDialog

                , decl "position" (Ident "absolute")
                ]

addCommentDialogStyles :: HasCallStack => [Decl]
addCommentDialogStyles = [decl "backgroundColor" C.VDocComment] <> dialogStyles

-- is vdoc_overlay_content__comment in CSS

showNoteDialogStyles :: HasCallStack => [Decl]
showNoteDialogStyles = [decl "backgroundColor" C.VDocNote] <> dialogStyles

showDiscussionDialogStyles :: HasCallStack => [Decl]
showDiscussionDialogStyles = [decl "backgroundColor" C.VDocDiscussion] <> dialogStyles

overlayStyles :: HasCallStack => [Decl]
overlayStyles =
  [ zindex ZIxOverlay
  , decl "backgroundColor" C.OverlayBackdrop
  ]


-- * elements / components used for all contribution kinds

addContributionDialogFrame
    :: ST -> Maybe SelectionStateWithPx -> Int
    -> ReactElementM 'EventHandlerCode () -> ReactElementM 'EventHandlerCode ()
addContributionDialogFrame title mrange windowWidth child =
    let top = case mrange of
              Nothing -> 30
              Just range -> (range ^. sstBottomOffset . unOffsetFromViewportTop)
                          + (range ^. sstScrollOffset . unScrollOffsetOfViewport)
        extraStyles = [ decl "top" (Px $ top + 5)
                      , decl "left" (Px $ leftFor windowWidth)
                      , decl "height" (Px 560)
                      ]
    in skylight_ ["isVisible" &= True
             , React.on "onCloseClicked"   $ \_ -> simpleHandler $ dispatch (ContributionAction HideCommentEditor)
             , React.on "onOverlayClicked" $ \_ -> simpleHandler $ dispatch (ContributionAction HideCommentEditor)
             , "dialogStyles" @@= (addCommentDialogStyles <> extraStyles)
             , "overlayStyles" @@= overlayStyles
             , "titleStyle" @@= [decl "margin" (Px 0)]
             ]  $ do

      icon_ (IconProps "c-vdoc-overlay-content" False ("icon-New_Comment", "dark") XLarge)

      span_ [ "className" $= "c-vdoc-overlay-content__title"
            , "style" @@=
                    [ decl "fontSize" (Rem 1.125)
                    , decl "lineHeight" (Mm 1.15)
                    , decl "marginBottom" (Rem 0.875)
                    , decl "marginLeft" (Rem 1)
                    , decl "fontWeight" (Ident "bold")
                    ]
            ] (elemText title)

      hr_ []

      child

contributionDialogTextForm :: HasCallStack => Lens' st ST -> st -> Int -> ST -> ReactElementM ('StatefulEventHandlerCode st) ()
contributionDialogTextForm stateLens st' stepNumber promptText = do
  div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
    p_ $ do
      elemString $ "Step " <> show stepNumber <> ": "
      span_ ["className" $= "bold"] $ do
        elemText promptText

  contributionDialogTextFormInner 600 240 stateLens st'

contributionDialogTextFormInner :: HasCallStack => Int -> Int -> Lens' st ST -> st -> ReactElementM ('StatefulEventHandlerCode st) ()
contributionDialogTextFormInner width height stateLens st' = do
  form_ [ "target" $= "#"
        , "action" $= "POST"] $ do
    textarea_ [ "id" $= "o-vdoc-overlay-content__textarea-annotation"  -- RENAME: annotation => comment
              , "className" $= "o-form-input__textarea"
              , "style" @@=
                      [ decl "resize" (Ident "none")
                      , decl "width" (Px width)
                      , decl "height" (Px height)
                      ]
              -- Update the current state with the current text in the textbox, sending no actions
              , onChange $ \evt -> simpleHandler $ \st -> ([], Just $ st & stateLens .~ target evt "value")
              , "value" $= cs (st' ^. stateLens)
              ]
      mempty


-- * comments

showComment :: HasCallStack => View '[CommentDisplayProps]
showComment = mkView "ShowComment" $ \props ->
  let extraStyles = [ decl "top" (Px $ props ^. cdpTopOffset . unOffsetFromDocumentTop + 5)
                    , decl "left" (Px . leftFor $ props ^. cdpWindowWidth)
                    , decl "height" (Px 0)
                    , decl "minHeight" (Px 100)
                    ]
  in skylight_ ["isVisible" &= True
           , React.on "onCloseClicked"   $ \_ -> simpleHandler $ dispatch (ContributionAction HideContributionDialog)
           , React.on "onOverlayClicked" $ \_ -> simpleHandler $ dispatch (ContributionAction HideContributionDialog)
           , "dialogStyles" @@= ((props ^. cdpContentStyle) <> extraStyles)
           , "overlayStyles" @@= overlayStyles
           , "closeButtonStyle" @@= [decl "top" (Px 0), decl "bottom" (Px 0)]
           , "titleStyle" @@= [decl "margin" (Px 0)]
           ] $ do
    -- div_ ["className" $= "c-vdoc-overlay-content c-vdoc-overlay-content--comment"] $ do

        div_ ["style" @@= [decl "marginLeft" (Percentage 96)]] $ do             -- FIXME: How to do this properly?
          icon_ (IconProps "c-vdoc-overlay-content" False (props ^. cdpIconStyle) XLarge)

        div_ ["className" $= "c-vdoc-overlay-content__copy"] $ elemText (props ^. cdpCommentText)

        -- edit/comment user meta data -->
        div_ ["className" $= "c-vdoc-overlay-meta"] $ do
            span_ ["className" $= "c-vdoc-overlay-meta__user-avatar"] $ do
                icon_ (IconProps "c-vdoc-overlay-meta" False ("icon-User", "bright") Medium)
            span_ ["className" $= "c-vdoc-overlay-meta__user"] $ elemCS (props ^. cdpUserName)
            span_ ["className" $= "c-vdoc-overlay-meta__date"] $ elemCS (props ^. cdpCreationDate) -- or what is this?
        -- END: edit/comment user meta data -->

        -- vote buttons -->
        div_ ["className" $= "c-vdoc-overlay-votes"] $ do

            ibutton_ $ emptyIbuttonProps "Vote_positive"
                         [ContributionAction $ ToggleVoteOnContribution (ContribIDDiscussion True $ props ^. cdpNoteId) Yeay]
                     & ibListKey .~ "vote_up"
                     & ibSize .~ XLarge
                     & ibLabel .~ cs (show . fromMaybe 0 . Map.lookup Yeay $ props ^. cdpVotes)
                     -- ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-up"]
                     -- IconProps "c-vdoc-overlay-votes"

            ibutton_ $ emptyIbuttonProps "Vote_negative"
                         [ContributionAction $ ToggleVoteOnContribution (ContribIDDiscussion True $ props ^. cdpNoteId) Nay]
                     & ibListKey .~ "vote_down"
                     & ibSize .~ XLarge
                     & ibLabel .~ cs (show . fromMaybe 0 . Map.lookup Nay $ props ^. cdpVotes)
                     -- ["className" $= "c-vdoc-overlay-votes__button c-vdoc-overlay-votes__btn-vote-down"]
                     -- IconProps "c-vdoc-overlay-votes"
        -- END: vote buttons -->

        div_ ["style" @@= [decl "marginBottom" (Px 20)]] "" -- make some space for the close button

showComment_ :: HasCallStack => CommentDisplayProps -> ReactElementM eventHandler ()
showComment_ = view_ showComment "showComment_"


showNoteProps :: HasCallStack => Map.Map (ID Discussion) Discussion -> GlobalState -> Maybe ShowNoteProps
showNoteProps notes rs = case (maybeNote, maybeOffset) of
  (Just note, Just offset) -> Just $ ShowNoteProps note offset
                                     (rs ^. gsScreenState . ssWindowWidth)
                                     ((^. userName) <$> (rs ^. gsServerCache . scUsers))

  (Just note, Nothing)     -> err "note" note "offset" Nothing
  (Nothing,   Just offset) -> err "offset" offset "note" Nothing
  _                        -> Nothing
  where
    maybeContribID = rs ^. gsContributionState . csDisplayedContributionID
    maybeNoteID :: Maybe (ID Discussion) = getDiscussionID =<< maybeContribID
    maybeNote = (`Map.lookup` notes) =<< maybeNoteID
    maybeOffset = do
      nid <- maybeNoteID
      rs ^? gsContributionState . csAllVerticalSpanBounds . allVerticalSpanBounds
          . at (MarkContribution (ContribIDDiscussion True nid) 0) . _Just . verticalSpanBoundsBottom

    err haveT haveV missT = gracefulError (unwords ["showNoteProps: we have a", haveT, show haveV, "but no", missT])


showNote :: HasCallStack => View '[ShowNoteProps]
showNote = mkView "ShowNote" $ \case
  ShowNoteProps note top windowWidth1 usernames ->
    let commentText1  = (note ^. noteText)
        iconStyle1    = ("icon-Note", "dark")
        user          = note ^. discussionMetaID . miMeta . metaCreatedBy
        userName1 = case user of
          UserID i -> cs . fromMaybe (cacheMiss (CacheKeyUser i) (cs $ show i) i) $ Map.lookup i usernames
          _ -> cs $ show user
        votes = votesToCount $ note ^. discussionVotes
        creationDate1 = showTime $ note ^. discussionMetaID . miMeta . metaCreatedAt
    in showComment_ $ CommentDisplayProps commentText1 iconStyle1 userName1 creationDate1
                                          showNoteDialogStyles top windowWidth1
                                          (note ^. discussionID)
                                          votes
  where
    showTime (Timestamp t) = cs $ formatTime defaultTimeLocale "%F %T" t

showNote_ :: HasCallStack => ShowNoteProps -> ReactElementM eventHandler ()
showNote_ = view_ showNote "showNote_"


addComment :: HasCallStack => Translations -> View '[AddContributionProps (LocalStateRef CommentInputState)]
addComment __ = mkView "AddComment" $ \props -> addContributionDialogFrame
  (__ add_a_comment)
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  (commentInput_ $ props ^. acpLocalState)

addComment_ :: HasCallStack => Translations -> AddContributionProps (LocalStateRef CommentInputState) -> ReactElementM eventHandler ()
addComment_ __ = view_ (addComment __) "addComment_"


commentInput :: HasCallStack => LocalStateRef CommentInputState -> View '[]
commentInput lst = mkPersistentStatefulView "CommentInput" lst Nothing $ \st ->
  do
    let smkind = st ^? commentInputStateData . commentInfoKind . _Just
    let stext  = st ^. commentInputStateData . commentInfoDesc

    div_ $ do
      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 1: "
          span_ ["className" $= "bold"] "Select a type for your comment:"

      div_ ["className" $= "c-vdoc-overlay-content__annotation-type"] $ do  -- RENAME: annotation => comment
        let props :: CommentKind -> IbuttonProps CommentKind
            props ckind = emptyIbuttonProps (img ckind) ckind
              & ibListKey       .~ l ckind
              & ibHighlightWhen .~ highlightWhen ckind
              & ibLabel         .~ l ckind
              & ibEnabled       .~ True
              & ibSize          .~ Large
              where
                l CommentKindNote       = "note"
                l CommentKindDiscussion = "discussion"

                img CommentKindNote       = "Note"
                img CommentKindDiscussion = "Discussion"

                highlightWhen k
                  | smkind == Just k = HighlightAlways
                  | otherwise        = HighlightOnMouseOver

        sibutton_ commentInputStateMouseOverNote       st $ props CommentKindNote
        sibutton_ commentInputStateMouseOverDiscussion st $ props CommentKindDiscussion

      hr_ []

      contributionDialogTextForm (commentInputStateData . commentInfoDesc) st 2 "enter your comment:"

      hr_ []

      div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
        p_ $ do
          elemString "Step 3: "
          span_ ["className" $= "bold"] "finish"

      let enableOrDisable props = if ST.null stext || isNothing smkind
            then props
              & iconButtonPropsDisabled     .~ True
            else props
              & iconButtonPropsDisabled     .~ False
              & iconButtonPropsOnClick      .~
                    [ ContributionAction $ SubmitComment (CommentInfo stext (fromJust smkind))
                    , ContributionAction ClearRange
                    , ContributionAction HideCommentEditor
                    ]

      iconButton_ $ defaultIconButtonProps @[GlobalAction]
          & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" False ("icon-Share", "dark") Large
          & iconButtonPropsElementName  .~ "submit"
          & iconButtonPropsLabel        .~ "submit"
          & enableOrDisable

commentInput_ :: HasCallStack => LocalStateRef CommentInputState -> ReactElementM eventHandler ()
commentInput_ lst = view_ (commentInput lst) "commentInput_"


-- * edits

-- FIXME: is it a good thing that the local state is lost whenever the
-- props change?  (something about people trying to compute the
-- initial state from props, perhaps?)
-- to put it in another way: often it would be nice if we could change
-- the props value via a global action and keep going with the local
-- state.

addEdit :: HasCallStack => View '[AddContributionProps (EditInfo (Maybe EditKind))]
addEdit = mkView "AddEdit" $ \props -> addContributionDialogFrame
  "add an edit"
  (props ^. acpRange)
  (props ^. acpWindowWidth)
  (editInput_ (props ^. acpLocalState))

addEdit_ :: HasCallStack => AddContributionProps (EditInfo (Maybe EditKind)) -> ReactElementM eventHandler ()
addEdit_ = view_ addEdit "addEdit_"


-- | FUTUREWORK: there is *some* code sharing between 'editInput_' and 'commentInput_', but there may be
-- room for more.
--
-- FUTUREWORK: kind change is a nice example of local signals between two components.  how is this
-- handled in react?  should we have a second global store here that is just shared between
-- 'editInput' and and 'editKindForm'?
editInput :: HasCallStack => EditInfo (Maybe EditKind) -> View '[]
editInput einfo = mkPersistentStatefulView "EditInput" (einfo ^. editInfoLocalStateRef) Nothing $
  \st@(EditInputState (EditInfo desc mkind rst) _) -> do
    div_ $ do
      elemString "Step 1: "
      span_ ["className" $= "bold"] "Type of this edit:"
      div_ $ editKindForm_ st

    hr_ []

    contributionDialogTextForm (editInputStateData . editInfoDesc) st 2 "describe your motivation for this edit:"

    hr_ []

    let enableOrDisable props = if ST.null desc || isNothing mkind
          then props
            & iconButtonPropsDisabled     .~ True
          else props
            & iconButtonPropsDisabled     .~ False
            & iconButtonPropsOnClick      .~ [ DocumentAction . DocumentSave . FormComplete $ EditInfo desc (fromJust mkind) rst
                                             , DocumentAction UpdateDocumentStateView
                                             , ContributionAction ClearRange
                                             ]

    -- FIXME: make new button, like in 'commentInput_' above.  we
    -- don't have to save this in global state until the 'editInput_'
    -- dialog is closed again without save or cancel.
    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "bright") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & enableOrDisable

editInput_ :: HasCallStack => EditInfo (Maybe EditKind) -> ReactElementM eventHandler ()
editInput_ st = view_ (editInput st) "editInput_"


-- | (FUTUREWORK: somewhere in here, there is a radio button widget
-- that works with any Bounded/Enum type...)
editKindForm_ :: HasCallStack => EditInputState -> ReactElementM ('StatefulEventHandlerCode EditInputState) ()
editKindForm_ st = do
  div_ ["className" $= "row row-align-middle c-vdoc-toolbar-extension"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      div_ ["className" $= "gr-23 gr-20@tablet gr-14@desktop gr-centered"] $ do
        div_ ["className" $= "c-vdoc-toolbar-extension__pointer"] $ do
          pure ()
        div_ ["className" $= "c-vdoc-toolbar-extension__modification c-vdoc-toolbar-extension--expanded"] $ do  -- (RENAME: Edit)
          forM_ [Grammar, Phrasing, Meaning] $ \ekind -> do
            sibutton_ (mouseIsOver ekind) st (props ekind)
            div_ ["className" $= "c-vdoc-toolbar__separator"] ""
  where
    mouseIsOver :: EditKind -> Lens' EditInputState Bool
    mouseIsOver ekind f (wide :: EditInputState) = outof <$> f into
      where
        outof :: Bool -> EditInputState
        outof True  = wide & editInputStateMouseOver .~ Just ekind
        outof False = wide & editInputStateMouseOver .~ Nothing

        into :: Bool
        into = wide ^. editInputStateMouseOver == Just ekind

    props :: EditKind -> IbuttonProps EditKind
    props ekind = emptyIbuttonProps "New_Edit" ekind
      & ibListKey       .~ cs (l ekind)
      & ibHighlightWhen .~ highlightWhen
      & ibLabel         .~ cs (l ekind)
      & ibEnabled       .~ True
      & ibSize          .~ Large
      where
        l = fmap toLower . show

        highlightWhen :: HighlightWhen
        highlightWhen = if st ^. editInputStateData . editInfoKind == Just ekind
          then HighlightAlways
          else HighlightOnMouseOver
