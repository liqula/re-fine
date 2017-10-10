{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.MainMenu.Component
  ( topMenuBar
  , mainMenu
  , mainMenuGroups
  , mainMenuGroup
  ) where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Dialog (contributionDialogTextForm)
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.ImageUpload
import           Refine.Frontend.Login.Component
import           Refine.Frontend.Login.Status
import           Refine.Frontend.Login.Types
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Console
import           Refine.Frontend.Types
import           Refine.Frontend.Util
import qualified Refine.Prelude.BuildInfo as BuildInfo

import System.IO.Unsafe


topMenuBar :: HasCallStack => View '[TopMenuBarProps]
topMenuBar = mkView "TopMenuBarInMainMenu" $ \(TopMenuBarProps mCurrentTab currentUser) ->
  let burgerOrCloseButton = case mCurrentTab of
        Nothing -> button_ [ "aria-controls" $= "bs-navbar"
                           , "aria-expanded" $= "false"
                           , "className" $= "c-mainmenu__menu-button"
                           , "type" $= "button"
                           , "style" @@= [decl "pointerEvents" (Ident "all")]
                           , onClick $ \_ _ -> simpleHandler . dispatch . MainMenuAction $ MainMenuActionOpen defaultMainMenuTab
                           ] $ do
                     span_ ["className" $= "sr-only"] "main menu"
                     span_ ["className" $= "c-mainmenu__icon-bar"] ""
                     span_ ["className" $= "c-mainmenu__icon-bar"] ""
                     span_ ["className" $= "c-mainmenu__icon-bar"] ""
        Just _ -> ibutton_
          $ emptyIbuttonProps (ButtonImageIcon Svg.Close ColorSchemaBright) (onLoginClick currentUser)
          & ibListKey .~ "1"
          & ibSize .~ XXLarge

      profilePageButton = ibutton_
        $ emptyIbuttonProps (ButtonImageIcon Svg.UserProfile ColorSchemaBright)
            (case currentUser of
               UserLoggedIn user -> [MainMenuAction . MainMenuActionOpen
                                     $ MainMenuProfile ( either id (^. userID) user
                                                       , FormBegin $ newLocalStateRef (Nothing, Nothing) user)]
               UserLoggedOut -> [])
        & ibListKey .~ "2"
        & ibSize .~ XXLarge

      groupsButton = ibutton_
        $ emptyIbuttonProps (ButtonImageIcon Svg.Group ColorSchemaBright)
            [MainMenuAction . MainMenuActionOpen $ MainMenuGroups ()]
        & ibListKey .~ "3"
        & ibPressed .~ Just (case mCurrentTab of Just MainMenuGroup{} -> True; _ -> False)
        & ibSize .~ XXLarge

      helpButton = ibutton_
        $ emptyIbuttonProps (ButtonImageIcon Svg.Help ColorSchemaBright)
            [MainMenuAction $ MainMenuActionOpen MainMenuHelp]
        & ibListKey .~ "4"
        & ibPressed .~ Just (mCurrentTab == Just MainMenuHelp)
        & ibSize .~ XXLarge

      dumpPageButton = when weAreInDevMode . ibutton_
        $ emptyIbuttonProps (ButtonImageIcon Svg.Help ColorSchemaDark) [DumpAllState]

      loginButton = loginStatusButton_ ColorSchemaBright (has _MainMenuLogin <$> mCurrentTab) currentUser

      buttonBox_ = div_ ["className" $= "inner-column-1"]

      -- in order the platform title to be centered, the left and right need to contain equally many
      -- button boxes.  some of them may be empty.
      emptyButtonBox_ = buttonBox_ . div_ ["className" $= "ibutton_xxlarge"] $ pure ()

  in div_ ["className" $= "topmenubar"] $ do
       div_ ["className" $= "left-column m-t-1 m-l-1 m-r-1 m-b-1"] $ do
         buttonBox_ burgerOrCloseButton
         buttonBox_ profilePageButton
         buttonBox_ groupsButton
         buttonBox_ helpButton

       div_ ["className" $= "platform-title"] $ pure ()

       div_ ["className" $= "right-column m-t-1 m-l-1 m-r-1 m-b-1"] $ do
         emptyButtonBox_
         emptyButtonBox_
         buttonBox_ dumpPageButton
         buttonBox_ loginButton

mainMenu :: HasCallStack => View '[MainMenuProps MainMenuTabProps]
mainMenu = mkView "MainMenu" $ \(MainMenuProps currentTab menuErrors currentUser) -> do
  div_ ["className" $= "body-container c_bg_blue_dark"] $ do
      view_ topMenuBar "topMenuBar" (TopMenuBarProps (Just currentTab) currentUser)
      case currentTab of
          MainMenuGroups groups               -> mainMenuGroups_ groups
          MainMenuGroup sub group             -> mainMenuGroup_ (sub, group)
          MainMenuCreateOrUpdateGroup mid lst -> mainMenuCreateGroup_ mid lst
          MainMenuCreateProcess lst           -> mainMenuCreateProcess_ lst
          MainMenuUpdateProcess pid lst       -> mainMenuUpdateProcess_ pid lst
          MainMenuProfile (uid, lst)          -> mainMenuProfile_ editable uid lst
            where
              editable = case currentUser of
                UserLoggedIn u -> either id (^. userID) u == either id (^. userID) uid
                _ -> False

          MainMenuHelp -> pre_ $ do
            elemString $ "commit hash: " <> show BuildInfo.gitCommitHash
            "\n"
            elemString $ "build timestamp: " <> show BuildInfo.gitBuildTimestamp
            "\n"

          MainMenuLogin subtab -> mainMenuLoginTab_ (MainMenuProps subtab menuErrors currentUser)


mainMenuGroups :: View '[GroupsProps]
mainMenuGroups = mkView "MainMenuGroups" $ \groups -> do
  div_ ["className" $= "menupage-container c_bg_blue_light"] $ do
    div_ ["className" $= "menupage-header"] $ do
      let mkCreateGroupAction :: GlobalAction
          mkCreateGroupAction = MainMenuAction . MainMenuActionOpen . MainMenuCreateOrUpdateGroup Nothing . FormBegin
                              $ newLocalStateRef
                                  (CreateGroup "" "" [] []
                                    (flip (,) False <$> Map.elems (groups ^. groupsPropsAllMembers))
                                    Nothing)
                                  groups

      div_ ["className" $= "left-column"] $ do
        div_ ["className" $= "inner-column-1"] $ do
          ibutton_ $ emptyIbuttonProps (ButtonImageIcon GroupNew ColorSchemaDark) [mkCreateGroupAction]
            & ibListKey .~ "create_group"
            & ibSize .~ XLarge

    div_ ["className" $= "menupage-hr"] $ do
      pure ()

    div_ ["className" $= "group-tile-container"] $ do
      let mainMenuGroupShort_ :: HasCallStack => Group -> ReactElementM eventHandler ()
          mainMenuGroupShort_ group = view_ mainMenuGroupShort listKey group
            where listKey = "mainMenuGroupShort-" <> (cs . show $ group ^. groupID . unID)
      mainMenuGroupShort_ `mapM_` sortBy (compare `on` view groupTitle) (groups ^. groupsPropsGroups)

mainMenuGroups_ :: HasCallStack => GroupsProps -> ReactElementM eventHandler ()
mainMenuGroups_ = view_ mainMenuGroups "mainMenuGroups"

mainMenuGroupShort :: HasCallStack => View '[Group]
mainMenuGroupShort = mkView "MainMenuGroupShort" $ \group -> do
  div_ [ "className" $= "group-tile c_bg_blue_dawn"
       , onClick $ \_ _ -> simpleHandler . dispatch
                         . MainMenuAction . MainMenuActionOpen
                         . MainMenuGroup MainMenuGroupProcesses $ group ^. groupID
       ] $ do

    case group ^. groupImage of
      Nothing -> do
        div_ ["className" $= "group-tile__svg-div"] $ do
          Svg.render ColorSchemaBright def Svg.Group
      Just (ImageInline img) -> do
        div_ ["className" $= "group-tile__image-div"] $ do
          img_ ["className" $= "group-tile__image-img", "src" $= cs img, "alt" $= "[group logo]"] $ pure ()

    div_ ["className" $= "group-tile__groupname"] $ do
      elemText $ group ^. groupTitle

    -- buttons for subgroups, processes, users (FUTUREWORK: make those clickable in addition to the entire
    -- tile.  clicking on 'members' button should get you to the members tab directly.)
    div_ ["className" $= "group-tile__ibuttons-div"] $ do
      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Group ColorSchemaBright) ([] :: [GlobalAction])
        & ibListKey .~ "subgroups"
        & ibIndexNum .~ Nothing
        & ibSize .~ XXXLarge

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Process ColorSchemaBright) ([] :: [GlobalAction])
        & ibListKey .~ "processes"
        & ibIndexNum .~ Just (length $ group ^. groupVDocs)
        & ibSize .~ XXXLarge

      ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.User ColorSchemaBright) ([] :: [GlobalAction])
        & ibListKey .~ "members"
        & ibIndexNum .~ Just (length $ group ^. groupMembers)
        & ibSize .~ XXXLarge

mainMenuGroup :: View '[(MainMenuGroup, GroupProps)]
mainMenuGroup = mkView "mainMenuGroup" $ \case
 (_, GroupProps Nothing _ _) -> hourglass
 (sub, GroupProps (Just group) vdocs users) -> do
    div_ ["className" $= "menupage-container c_bg_blue_light"] $ do
      div_ ["className" $= "menupage-header"] $ do
        div_ ["className" $= "left-column"] $ do
          div_ ["className" $= "inner-column-1"] $ do

            ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.ProcessNew ColorSchemaDark)
                [ MainMenuAction . MainMenuActionOpen . MainMenuCreateProcess . FormBegin
                  $ newLocalStateRef (CreateVDoc (Title "[no title]") (Abstract "[no abstract]") emptyRawContent (group ^. groupID) Nothing) group
                ]
              & ibListKey .~ "process_add"
              & ibSize .~ XXLarge

            ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.GroupUpdate ColorSchemaDark)
                [ MainMenuAction . MainMenuActionOpen . MainMenuCreateOrUpdateGroup (Just $ group ^. groupID) . FormBegin
                  $ newLocalStateRef
                      (CreateGroup
                        (group ^. groupTitle)
                        (group ^. groupDesc)
                        []
                        []
                        [(u, (u ^. userID) `elem` (group ^. groupMembers)) | u <- Map.elems users]
                        Nothing)
                      group
                ]
              & ibListKey .~ "group_update"
              & ibSize .~ XXLarge

      div_ ["className" $= "menupage-hr m-b-2"] $ do
        pure ()

      div_ ["className" $= "group-details"] $ do
      -- image(s)
        case group ^. groupImage of
          Nothing -> do
            div_ ["className" $= "group-details__svg-div"] $ do
              Svg.render ColorSchemaBright def Svg.Group
          Just (ImageInline img) -> do
            img_ ["className" $= "group-details__profile-img", "src" $= cs img, "alt" $= "[group logo]"] $ pure ()

        -- title, abstract
        div_ ["className" $= "group-details__description"] $ do
          div_ ["className" $= "group-details__description-headline"] $ do
            elemText $ group ^. groupTitle
          div_ ["className" $= "group-details__description-text"] $ do
            elemText $ group ^. groupDesc

        div_ ["className" $= "menupage-hr-inner m-b-2"] $ pure ()

        -- sub-sub-tab buttons (users, processes)
        div_ ["className" $= "menupage-iconlist"] $ do
          ibutton_ $ emptyIbuttonProps
              (ButtonImageIcon Svg.Group ColorSchemaBright)
              [ShowNotImplementedYet]
            & ibListKey .~ "subgroups"
            & ibIndexNum .~ Just 3
            & ibSize .~ XXXLarge

          ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Process ColorSchemaDark)
              [ MainMenuAction . MainMenuActionOpen
                               . MainMenuGroup MainMenuGroupProcesses $ group ^. groupID ]
            & ibListKey .~ "process"
            & ibSize .~ XXXLarge
            & ibPressed .~ Just (has _MainMenuGroupMembers sub)

          ibutton_ $ emptyIbuttonProps
              (ButtonImageIcon Svg.User ColorSchemaDark)
              [ MainMenuAction . MainMenuActionOpen
                               . MainMenuGroup MainMenuGroupMembers $ group ^. groupID ]
            & ibListKey .~ "user"
            & ibSize .~ XXXLarge
            & ibPressed .~ Just (has _MainMenuGroupProcesses sub)

        -- sub-sub-tab buttons (users, processes)
        case sub of
          MainMenuGroupProcesses -> do
            let procs :: [MainMenuProcessShortProps]
                procs = sortBy (compare `on` view mmprocShrtTitle) (mkProcProps <$> group ^. groupVDocs)

                mkProcProps vid = MainMenuProcessShortProps
                  { _mmprocShrtID          = vid
                  , _mmprocShrtIcon        = ()
                  , _mmprocShrtTitle       = title
                  , _mmprocShrtAbstract    = abstract
                  , _mmprocShrtNumComments = stats ^. editStatsComments
                  , _mmprocShrtNumEdits    = stats ^. editStatsEdits
                  , _mmprocShrtNumUsers    = stats ^. editStatsUsers
                  }
                  where
                    ((title, abstract), stats) = maybe
                      (cacheMiss (CacheKeyVDoc vid) ((Title hourglass, Abstract mempty), mempty) vid)
                      (((^. vdocTitle) &&& (^. vdocAbstract)) &&& (^. vdocStats))
                      $ Map.lookup vid vdocs

            div_ [ "className" $= "document-tile-container"] $
              mainMenuProcessShort_ `mapM_` procs

          MainMenuGroupMembers -> do
            let members = fmap mkMemberProps $ group ^. groupMembers
                mkMemberProps uid = Map.lookup uid users
            div_ [ "className" $= "document-tile-container"] $
              mainMenuMemberShort_ `mapM_` members

mainMenuGroup_ :: HasCallStack => (MainMenuGroup, GroupProps) -> ReactElementM eventHandler ()
mainMenuGroup_ = view_ mainMenuGroup "mainMenuGroup"


mainMenuMemberShort :: HasCallStack => View '[User]
mainMenuMemberShort = mkView "MainMenuProcessShort" $ \props -> do
  div_ [ onClick $ \_ _ -> simpleHandler . dispatch
                         . MainMenuAction . MainMenuActionOpen
                         $ MainMenuProfile (props ^. userID, FormBegin $ newLocalStateRef (Nothing, Nothing) props)
       ] $ do

    -- image
    br_ []
    let img = maybe (ButtonImageIcon Svg.User ColorSchemaDark) ButtonImageInline $ props ^. userAvatar
     in ibutton_ $ emptyIbuttonProps img ([] :: [GlobalAction])
          & ibListKey .~ "user"
          & ibSize .~ XXLarge

    -- title
    br_ []
    div_ [] $ do
      elemText $ props ^. userName

mainMenuMemberShort_ :: HasCallStack => Maybe User -> ReactElementM 'EventHandlerCode ()
mainMenuMemberShort_ Nothing = hourglass
mainMenuMemberShort_ (Just props) = view_ mainMenuMemberShort listKey props
  where
    listKey = "mainMenuMemberShort-" <> (cs . show $ props ^. userID . unID)


mainMenuProcessShort :: HasCallStack => View '[MainMenuProcessShortProps]
mainMenuProcessShort = mkView "MainMenuProcessShort" $ \props -> do
  div_ [ "className" $= "document-tile"
       , onClick $ \_ _ -> simpleHandler . dispatch . LoadVDoc $ props ^. mmprocShrtID
       ] $ do

    -- image
    div_ ["className" $= "document-tile__column1"] $ do
      div_ ["className" $= "document-tile__column1-svg"] $ do
        ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Process ColorSchemaBright) ([] :: [GlobalAction])
          & ibListKey .~ "process"
          & ibSize .~ XXLarge

    -- title & description
    div_ ["className" $= "document-tile__column2"] $ do
      div_ ["className" $= "document-tile__column2-headline"] $ do
        elemText $ props ^. mmprocShrtTitle . unTitle
      div_ ["className" $= "document-tile__column2-body"] $ do
        elemText $ props ^. mmprocShrtAbstract . unAbstract

    div_ ["className" $= "document-tile__column3"] $ do
      div_ ["className" $= "document-tile__column3-vertical-line"] $ pure ()

    div_ ["className" $= "document-tile__column4"] $ do
      -- comments
      div_ ["className" $= "document-tile__column4-item"] $ do
        div_ ["className" $= "document-tile__column4-item-icon"] $ do
          ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Comment ColorSchemaBright) ([] :: [GlobalAction])
            & ibSize .~ XXLarge
        div_ ["className" $= "document-tile__column4-item-text"] $ do
          elemString . show $ props ^. mmprocShrtNumComments

        -- FIXME: can we easily compute how many comments / edits are made by the browsing user?
        -- that would be the next item-icon, item-text pair here.

      -- edits
      div_ ["className" $= "document-tile__column4-item"] $ do
        div_ ["className" $= "document-tile__column4-item-icon"] $ do
          ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Edit ColorSchemaBright) ([] :: [GlobalAction])
            & ibSize .~ XXLarge
        div_ ["className" $= "document-tile__column4-item-text"] $ do
          elemString . show $ props ^. mmprocShrtNumEdits

      -- participants
      div_ ["className" $= "document-tile__column4-item"] $ do
        div_ ["className" $= "document-tile__column4-item-icon"] $ do
          ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.User ColorSchemaBright) ([] :: [GlobalAction])
            & ibSize .~ XXLarge
        div_ ["className" $= "document-tile__column4-item-text"] $ do
          elemString . show $ props ^. mmprocShrtNumUsers

mainMenuProcessShort_ :: HasCallStack => MainMenuProcessShortProps -> ReactElementM 'EventHandlerCode ()
mainMenuProcessShort_ props = view_ mainMenuProcessShort listKey props
  where
    listKey = "mainMenuProcessShort-" <> (cs . show $ props ^. mmprocShrtID . unID)


-- | FUTUREWORK: should this be @View '[LocalStateRef CreateGroup]@ or @View '[Maybe (ID Group),
-- LocalStateRef CreateGroup]@?  (same with 'mainMenuCreateProcess'.)
mainMenuCreateGroup :: HasCallStack => Maybe (ID Group)
                    -> (LocalStateRef (CreateGroup_ [(User, Bool)]), Map (ID User) User)
                    -> View '[]
mainMenuCreateGroup mid (lst, allusers)
  = mkPersistentStatefulView "MainMenuCreateGroup" lst (Just addUsers) rndr
  where
    addUsers :: CreateGroup_ [(User, Bool)] -> CreateGroup_ [(User, Bool)]
    addUsers cg = cg & createGroupMembers %~ flip (foldr f) (Map.elems allusers)
      where
        f u asc = case lookup u asc of
          Nothing -> asc <> [(u, False)]
          _ -> asc

    rndr :: CreateGroup_ [(User, Bool)]
         -> ReactElementM_ (StatefulViewEventHandler (CreateGroup_ [(User, Bool)])) ()
    rndr st@(CreateGroup title desc _ _ users _) =
      div_ ["className" $= "menupage-container c_bg_blue_light"] $ do
        div_ ["className" $= "menupage-header"] $ do
          div_ ["className" $= "right-column"] $ do
            div_ ["className" $= "inner-column-1"] $ do
              ibutton_ $ emptyIbuttonProps
                (ButtonImageIcon Svg.Close ColorSchemaDark)
                [ MainMenuAction . MainMenuActionOpen $
                    maybe (MainMenuGroups ()) (MainMenuGroup MainMenuGroupProcesses) mid
                ]
                & ibListKey .~ "2"
                & ibSize .~ Large

        div_ ["className" $= "menupage-hr"] $ pure ()

        div_ ["className" $= "menu-form m-t-1"] $ do
          div_ ["className" $= "menu-form-header"] .
            div_ ["className" $= "left-column"] .
              div_ ["className" $= "inner-column-1"] .
                div_ ["className" $= "menu-form-header__label"] $
                  "Create / Update Group"

          div_ ["className" $= "menu-form__input-div m-b-1"] $
            contributionDialogTextForm createGroupTitle st 1 "group title"

          div_ ["className" $= "menu-form__input-div m-b-1"] $
            contributionDialogTextForm createGroupDesc st 2 "group description"

          div_ ["className" $= "menu-form__input-div m-b-1"] $ do
            div_ ["className" $= "c-vdoc-overlay-content__step-indicator"] $ do
              p_ $ do
                elemString "Step 3: "
                span_ ["className" $= "bold"] "Invite Members"

            forM_ (zip [0..] users) $ \(i, (user, cv)) -> do
              br_ []
              div_ ["key" @= ("u-" <> show (user ^. userID . unID))] $ do
                let inputFieldStyles =
                      [ decl "borderRadius" (Px 5)
                      , decl "margin" (Px 12)
                      , decl "backgroundColor" (Ident "rgba(246, 247, 249, 1)")
                      ]

                input_ $
                 ["checked" $= "checked" | cv] <>
                 [ "style" @@= inputFieldStyles
                 , "type" $= "checkbox"
                 , onChange $ \_evt -> simpleHandler $ \st' -> ([], Just (st' & createGroupMembers . ix i . _2 %~ not))
                 ]
                elemText $ user ^. userName <> " <" <> (user ^. userEmail) <> ">"

          div_ ["className" $= "menu-form__input-div m-b-1"] $ do
            ibutton_ $ emptyIbuttonProps
              (ButtonImageIcon Svg.Save ColorSchemaDark)
              [MainMenuAction . MainMenuActionOpen . MainMenuCreateOrUpdateGroup mid $ FormComplete st]
              & ibListKey .~ "1"
              & ibSize .~ XXLarge
              & ibEnabled .~ not (ST.null desc || ST.null title)


mainMenuCreateGroup_ :: HasCallStack
                     => Maybe (ID Group)
                     -> (LocalStateRef (CreateGroup_ [(User, Bool)]), Map (ID User) User)
                     -> ReactElementM eventHandler ()
mainMenuCreateGroup_ mid lst = view_ (mainMenuCreateGroup mid lst) "mainMenuCreateGroup"

mainMenuCreateProcess :: HasCallStack => LocalStateRef CreateVDoc -> View '[]
mainMenuCreateProcess lst = mkPersistentStatefulView "MainMenuCreateProcess" lst Nothing $ \st -> do
  renderCreateOrUpdateProcess
    createVDocTitle createVDocAbstract
    (MainMenuAction . MainMenuActionOpen . MainMenuCreateProcess)
    (MainMenuAction . MainMenuActionOpen . MainMenuGroup MainMenuGroupProcesses $ st ^. createVDocGroup)
    st

mainMenuCreateProcess_ :: HasCallStack => LocalStateRef CreateVDoc -> ReactElementM eventHandler ()
mainMenuCreateProcess_ lst = view_ (mainMenuCreateProcess lst) "mainMenuCreateProcess"

{- TODO:img edit or discard this comment?

- mkPersistentStatefulView is needed because of a text form
- with mkPersistentStatefulView, the local state is updated without emitting any actions
- in persistentStatefulView it is still possible to emit the action which is needed to upload the icon to the browser
- the upload is done in transform function in Store.hs
- when the upload is ready, we have to update the local state without forgetting the details there (like half-filled form)
- this is possible with local state update function

-}
mainMenuProfile :: HasCallStack => Bool -> Lookup User -> LocalStateRef ProfileLocalState -> View '[]
mainMenuProfile editable user lst = mkPersistentStatefulView "MainMenuProfile" lst Nothing $ \st -> case user of
  Left _uid -> hourglass
  Right u -> div_ ["style" @@= [decl "margin" (Px 30), decl "border" (Ident "red, dashed, 1px")]] $ do
      div_ ["className" $= "menupage-container c_bg_blue_light"] $ do
        div_ ["className" $= "menupage-header"] $ do
          div_ ["className" $= "left-column"] $ do
            div_ ["className" $= "inner-column-1"] $ do
              pure ()
            div_ ["className" $= "inner-column-1"] $ do
              pure ()

        div_ [] $ do
          div_ ["style" @@= [decl "backgroundColor" (Ident "yellow"), decl "border" (Ident "dashed black 1px")]] $ do
            ibutton_ $ emptyIbuttonProps (ButtonImageIcon Svg.Login ColorSchemaDark) [Logout]

          div_ ["style" @@= [decl "backgroundColor" (Ident "orange"), decl "border" (Ident "dashed black 1px")]] $ do
            elemText $ u ^. userName

          div_ ["style" @@= [decl "backgroundColor" (Ident "yellow"), decl "border" (Ident "dashed black 1px")]] $ do
            imageUpload_ editable u lst st

          div_ ["style" @@= [decl "backgroundColor" (Ident "orange"), decl "border" (Ident "dashed black 1px")]] $ do
           case snd st of
            Nothing -> do
              br_ []
              p_ "description:"
              p_ . elemText $ u ^. userDescription

              when editable $ do
                br_ []
                button_
                  [ onClick $ \_evt _ -> simpleHandler @_ $
                    \st' -> ([], Just $ st' & _2 .~ Just (u ^. userDescription))
                  ] $ elemText "edit"

            _ -> do
                br_ []
                contributionDialogTextForm (_2 . iso (fromJustNote "mainMenuProfile: desc") Just) st 1 "Description"
                br_ []

                button_
                  [ onClick $ \_evt _ -> simpleHandler @_ $
                    \st' -> ( [action @GlobalState . MainMenuAction . MainMenuActionOpen $
                               MainMenuProfile (u ^. userID, FormComplete (Right <$> (u ^. userAvatar), snd st))]
                            , Just $ st' & _2 .~ Nothing)
                  ] $ elemText "save"
                button_
                  [ onClick $ \_evt _ -> simpleHandler @_ $
                    \st' -> ([], Just $ st' & _2 .~ Nothing)
                  ] $ elemText "cancel"

mainMenuProfile_ :: HasCallStack => Bool -> Lookup User -> LocalStateRef ProfileLocalState -> ReactElementM eventHandler ()
mainMenuProfile_ editable user lst = view_ (mainMenuProfile editable user lst) "mainMenuProfile_"

mainMenuUpdateProcess :: HasCallStack => ID VDoc -> LocalStateRef UpdateVDoc -> View '[]
mainMenuUpdateProcess vid lst = mkPersistentStatefulView "MainMenuUpdateProcess" lst Nothing $
  renderCreateOrUpdateProcess
    updateVDocTitle updateVDocAbstract
    (MainMenuAction . MainMenuActionOpen . MainMenuUpdateProcess vid)
    (LoadVDoc vid)

mainMenuUpdateProcess_ :: HasCallStack => ID VDoc -> LocalStateRef UpdateVDoc -> ReactElementM eventHandler ()
mainMenuUpdateProcess_ vid lst = view_ (mainMenuUpdateProcess vid lst) "mainMenuUpdateProcess"

renderCreateOrUpdateProcess
  :: forall st a.
     ALens' st Title
  -> ALens' st Abstract
  -> (FormActionWith a st -> GlobalAction)  -- ^ save
  -> GlobalAction                           -- ^ cancel
  -> st
  -> ReactElementM ('StatefulEventHandlerCode st) ()
renderCreateOrUpdateProcess (cloneLens -> toTitle) (cloneLens -> toAbstract) save cancel st = do
  div_ ["className" $= "menupage-container c_bg_blue_light"] $ do
    div_ ["className" $= "menupage-header"] $ do
      div_ ["className" $= "right-column"] $ do
        div_ ["className" $= "inner-column-1"] $ do
          ibutton_ $ emptyIbuttonProps
            (ButtonImageIcon Svg.Close ColorSchemaDark)
            [cancel]
            & ibListKey .~ "2"
            & ibSize .~ Large

    div_ ["className" $= "menu-form m-t-1"] $ do
      div_ ["className" $= "menu-form-header"] .
        div_ ["className" $= "left-column"] .
          div_ ["className" $= "inner-column-1"] .
            div_ ["className" $= "menu-form-header__label"] $
              "Create / Update Process Details"

      div_ ["className" $= "menu-form__input-div m-b-1"] $
        contributionDialogTextForm (toTitle . unTitle) st 2 "title"

      div_ ["className" $= "menu-form__input-div m-b-1"] $
        contributionDialogTextForm (toAbstract . unAbstract) st 2 "abstract"

      div_ ["className" $= "menu-form__input-div m-b-1"] $ do
        ibutton_ $ emptyIbuttonProps
          (ButtonImageIcon Svg.Save ColorSchemaDark)
          [save $ FormComplete st]
          & ibEnabled .~ (not . ST.null $ st ^. toTitle . unTitle)
          & ibSize .~ XXLarge
          & ibListKey .~ "1"


mainMenuLoginTab :: HasCallStack => View '[MainMenuProps MainMenuSubTabLogin]
mainMenuLoginTab = mkView "MainMenuLoginTab" $ \(MainMenuProps currentTab menuErrors _currentUser) -> do
  let tabButton :: Int -> MainMenuSubTabLogin -> ReactElementM eventHandler ()
      tabButton key this =
        ibutton_ $ emptyIbuttonProps
            (ButtonImageIcon img ColorSchemaDark)
            [MainMenuAction . MainMenuActionOpen . MainMenuLogin $ this]
          & ibListKey .~ cs (show key)
          & ibPressed .~ Just (currentTab == this)
        where
          img = case this of
            MainMenuSubTabLogin        -> Svg.Login
            MainMenuSubTabRegistration -> Svg.Login  -- (do we need a different icon for this?)

  div_ ["className" $= "menupage-container c_bg_blue_light"] $ do

    div_ ["className" $= "menupage-header"] $ do
      div_ ["className" $= "left-column"] $ do
        div_ ["className" $= "inner-column-1"] $ do
          tabButton 0 MainMenuSubTabLogin
        div_ ["className" $= "inner-column-1"] $ do
          guardAccess_ "register" (AP.createUser [] []) $ tabButton 1 MainMenuSubTabRegistration

    div_ ["className" $= "menupage-hr"] $ pure ()

    case currentTab of
      MainMenuSubTabLogin        -> login_ (menuErrors ^. mmeLogin)
      MainMenuSubTabRegistration -> registration_ (menuErrors ^. mmeRegistration)

mainMenuLoginTab_ :: HasCallStack => MainMenuProps MainMenuSubTabLogin -> ReactElementM eventHandler ()
mainMenuLoginTab_ = view_ mainMenuLoginTab "mainMenuLoginTab_"
