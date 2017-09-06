{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.MainMenu.Component where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           React.Flux.Missing
import           Refine.Common.Types
import           Refine.Common.VDoc.Draft
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Dialog (contributionDialogTextForm)
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Types
import           Refine.Frontend.Login.Component
import           Refine.Frontend.Login.Status
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Store()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types
import           Refine.Frontend.Util
import qualified Refine.Prelude.BuildInfo as BuildInfo

import System.IO.Unsafe


topMenuBarInMainMenu :: HasCallStack => View '[TopMenuBarInMainMenuProps]
topMenuBarInMainMenu = mkView "TopMenuBarInMainMenu" $ \(TopMenuBarInMainMenuProps currentTab currentUser) ->
  div_ ["className" $= "c-mainmenu-content__header"] $ do
    div_ ["className" $= "gr-2"] $ do
      ibutton_ $ emptyIbuttonProps "Close" [MainMenuAction MainMenuActionClose]
        & ibListKey .~ "1"
        & ibDarkBackground .~ True
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

    div_ ["className" $= "gr-20"] $ do

      ibutton_ $ emptyIbuttonProps "User_profile"
        [ MainMenuAction . MainMenuActionOpen $ MainMenuProfile Nothing
        ]
        & ibListKey .~ "5"
        & ibDarkBackground .~ True
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

      ibutton_ $ emptyIbuttonProps "Group" [MainMenuAction . MainMenuActionOpen $ MainMenuGroups ()]
        & ibListKey .~ "3"
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ (if currentTab & has _MainMenuGroup then HighlightAlways else HighlightOnMouseOver)
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

      ibutton_ $ emptyIbuttonProps "Help" [MainMenuAction $ MainMenuActionOpen MainMenuHelp]
        & ibListKey .~ "4"
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ (if currentTab == MainMenuHelp then HighlightAlways else HighlightOnMouseOver)
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

      -- search

      loginStatusButton_
        ( (ibDarkBackground .~ True)
        . (ibLabel .~ mempty)
        . (ibHighlightWhen .~ case currentTab of MainMenuLogin _ -> HighlightAlways; _ -> HighlightOnMouseOver)
        )
        currentUser

    div_ ["className" $= "gr-2"] $ do
      ibutton_ $ emptyIbuttonProps "00_joker" [ShowNotImplementedYet]
        & ibListKey .~ "7"
        & ibDarkBackground .~ True
        & ibSize .~ XXLarge
        & ibLabel .~ mempty

topMenuBarInMainMenu_ :: HasCallStack => TopMenuBarInMainMenuProps -> ReactElementM eventHandler ()
topMenuBarInMainMenu_ = view_ topMenuBarInMainMenu "topMenuBarInMainMenu_"


tabStyles :: HasCallStack => [Decl]
tabStyles =
  [ decl "position" (Ident "absolute")
  , zindex ZIxLoginTab
  , decl "backgroundColor" (Ident "rgba(210, 217, 223, 1)")
  , decl "padding" (Px 50)
  , decl "borderRadius" (Px 12)
  ]

mainMenu :: HasCallStack => View '[MainMenuProps MainMenuTabProps]
mainMenu = mkView "MainMenu" $ \(MainMenuProps currentTab menuErrors currentUser) -> do
  div_ ["className" $= "row row-align-middle c-mainmenu-content"] $ do
    div_ ["className" $= "grid-wrapper"] $ do
      topMenuBarInMainMenu_ (TopMenuBarInMainMenuProps currentTab currentUser)
      div_ [ "className" $= "gr-2" ] $ do
        pure ()
      div_ [ "className" $= "gr-20"
           , "style" @@= tabStyles
           ] $ do
        case currentTab of
          MainMenuGroups groups               -> mainMenuGroups_ groups
          MainMenuGroup sub group             -> mainMenuGroup_ (sub, group)
          MainMenuCreateOrUpdateGroup mid lst -> mainMenuCreateGroup_ mid lst
          MainMenuCreateProcess lst           -> mainMenuCreateProcess_ lst
          MainMenuUpdateProcess pid lst       -> mainMenuUpdateProcess_ pid lst
          MainMenuProfile lst                 -> mainMenuProfile_ currentUser lst

          MainMenuHelp -> pre_ $ do
            elemString $ "commit hash: " <> show BuildInfo.gitCommitHash
            "\n"
            elemString $ "build timestamp: " <> show BuildInfo.gitBuildTimestamp
            "\n"

          MainMenuLogin subtab -> mainMenuLoginTab_ (MainMenuProps subtab menuErrors currentUser)

      div_ [ "className" $= "gr-2" ] $ do
        pure ()

mainMenu_ :: HasCallStack => MainMenuProps MainMenuTabProps -> ReactElementM eventHandler ()
mainMenu_ = view_ mainMenu "mainMenu_"


mainMenuGroups :: View '[GroupsProps]
mainMenuGroups = mkView "MainMenuGroups" $ \groups -> do
  div_ $ do
    div_ ["style" @@= [decl "marginLeft" (Px 3)]] $ do
      let mkCreateGroupAction :: GlobalAction
          mkCreateGroupAction = MainMenuAction . MainMenuActionOpen . MainMenuCreateOrUpdateGroup Nothing . FormBegin
                              $ newLocalStateRef
                                  (CreateGroup "" "" [] []
                                   $ flip (,) False <$> Map.elems (groups ^. _3))
                                  groups

      ibutton_ $ emptyIbuttonProps "Group_add" [mkCreateGroupAction]
        & ibListKey .~ "create_group"
        & ibSize .~ XLarge
        & ibDarkBackground .~ False
        & ibHighlightWhen .~ HighlightOnMouseOver
        & ibLabel .~ "create group"

    div_ $ do
      br_ [] >> br_ [] >> br_ [] >> hr_ []

    div_ $ do
      mainMenuGroupShort_ `mapM_` sortBy (compare `on` view groupTitle) (groups ^. _1)

mainMenuGroups_ :: HasCallStack => GroupsProps -> ReactElementM eventHandler ()
mainMenuGroups_ = view_ mainMenuGroups "mainMenuGroups"

mainMenuGroupShort :: HasCallStack => View '[Group]
mainMenuGroupShort = mkView "MainMenuGroupShort" $ \group -> do
  let listKey = "group-list-item-" <> (cs . show $ group ^. groupID . unID)
  div_ [ onClick $ \_ _ -> simpleHandler . dispatch
                         . MainMenuAction . MainMenuActionOpen
                         . MainMenuGroup MainMenuGroupProcesses $ group ^. groupID
       , "id" $= listKey  -- FUTUREWORK: get rid of this.  at the time of writing this it's only
                          -- used in the acceptance test.

       , "style" @@= [ decl "backgroundColor" (Ident "rgba(84, 99, 122, 1)")
                     , decl "padding" (Px 50)
                     , decl "margin" (Px 50)
                     , decl "borderRadius" (Px 12)
                     ]
       ] $ do

    -- image (FIXME: allow the user to store one in the group and use that)
    br_ []
    ibutton_ $ emptyIbuttonProps "Group" ([] :: [GlobalAction])
      & ibListKey .~ "group"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ True
      & ibHighlightWhen .~ HighlightNever
      & ibLabel .~ mempty

    -- title
    br_ []
    div_ [] $ do
      elemText $ group ^. groupTitle

    -- buttons for processes, users (FUTUREWORK: make those clickable in addition to the entire
    -- tile.  clicking on 'users' button should get you there directly.)
    br_ []
    div_ [] $ do
      let numProcesses :: Int = length $ group ^. groupVDocs
      ibutton_ $ emptyIbuttonProps "Process" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-processes")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ cs (show numProcesses)  -- FIXME: should be rendered differently, see click dummy

      let numUsers = length $ group ^. groupMembers
      ibutton_ $ emptyIbuttonProps "User" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-users")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ cs (show numUsers)  -- FIXME: should be rendered differently, see click dummy

mainMenuGroupShort_ :: HasCallStack => Group -> ReactElementM eventHandler ()
mainMenuGroupShort_ group = view_ mainMenuGroupShort listKey group
  where
    listKey = "mainMenuGroupShort-" <> (cs . show $ group ^. groupID . unID)

mainMenuGroup :: View '[(MainMenuGroup, GroupProps)]
mainMenuGroup = mkView "mainMenuGroup" $ \case
 (_, (Nothing, _, _)) -> "Loading..."
 (sub, (Just group, vdocs, users)) -> do
  div_ $ do
    ibutton_ $ emptyIbuttonProps "Arrow_left" [MainMenuAction . MainMenuActionOpen $ MainMenuGroups ()]
      & ibListKey .~ "group_back"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ False
      & ibHighlightWhen .~ HighlightOnMouseOver
      & ibLabel .~ mempty

    -- image (FIXME: allow the user to store one in the group and use that)
    br_ []
    ibutton_ $ emptyIbuttonProps "Group" ([] :: [GlobalAction])
      & ibListKey .~ "group"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ False
      & ibHighlightWhen .~ HighlightOnMouseOver
      & ibLabel .~ mempty

    -- title, abstract
    br_ [] >> hr_ []
    p_ . elemText $ group ^. groupTitle
    br_ []
    p_ . elemText $ group ^. groupDesc
    br_ [] >> hr_ []

    -- buttons for users, processes, create new process, update group
    -- the first two, users, processes, are morally tabs.
    span_ ["style" @@= [decl "marginLeft" (Px 50)]] . ibutton_ $ emptyIbuttonProps "User"
        [ MainMenuAction . MainMenuActionOpen
                         . MainMenuGroup MainMenuGroupMembers $ group ^. groupID ]
      & ibListKey .~ "user"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ False
      & ibHighlightWhen .~ HighlightOnMouseOver
      & ibLabel .~ "members"

    ibutton_ $ emptyIbuttonProps "Process"
        [ MainMenuAction . MainMenuActionOpen
                         . MainMenuGroup MainMenuGroupProcesses $ group ^. groupID ]
      & ibListKey .~ "process"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ False
      & ibHighlightWhen .~ HighlightAlways
      & ibLabel .~ "processes"

    ibutton_ $ emptyIbuttonProps "Process_add"
        [ MainMenuAction . MainMenuActionOpen . MainMenuCreateProcess . FormBegin
          $ newLocalStateRef (CreateVDoc (Title "[no title]") (Abstract "[no abstract]") emptyRawContent (group ^. groupID)) group
        ]
      & ibListKey .~ "process_add"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ False
      & ibHighlightWhen .~ HighlightOnMouseOver
      & ibLabel .~ "create new process"

    ibutton_ $ emptyIbuttonProps "Group_update"
        [ MainMenuAction . MainMenuActionOpen . MainMenuCreateOrUpdateGroup (Just $ group ^. groupID) . FormBegin
          $ newLocalStateRef
              (CreateGroup (group ^. groupTitle) (group ^. groupDesc) [] []
               [(u, (u ^. userID) `elem` (group ^. groupMembers)) | u <- Map.elems users])
              group
        ]
      & ibListKey .~ "group_update"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ False
      & ibHighlightWhen .~ HighlightOnMouseOver
      & ibLabel .~ "change group details"

    -- process list
    br_ [] >> br_ [] >> br_ [] >> br_ [] >> hr_ [] >> br_ []

    let mkProcProps vid = MainMenuProcessShortProps
          -- FIXME: use the trick from /docs/prototypes/ApplicativeDo.hs: load process props on
          -- demand, and when rendering filter for cache misses and just show the list of all
          -- processes already in the cache.  automatic re-rendering will take care of the rest.
          --
          -- FIXME: i think we don't have a rest-api end-point for this either.
          { _mmprocShrtID          = vid
          , _mmprocShrtIcon        = ()
          , _mmprocShrtTitle       = title
          , _mmprocShrtNumComments = stats ^. editStatsComments
          , _mmprocShrtNumEdits    = stats ^. editStatsEdits
          , _mmprocShrtNumUsers    = stats ^. editStatsUsers
          }
          where
            (title, stats) = maybe
              (cacheMiss (CacheKeyVDoc vid) (Title "loading", mempty) vid)
              ((^. vdocTitle) &&& (^. vdocStats))
              $ Map.lookup vid vdocs

        mkMemberProps uid = Map.lookup uid users

    case sub of
      MainMenuGroupProcesses -> mainMenuProcessShort_
                          `mapM_` ( sortBy (compare `on` view mmprocShrtTitle)
                                  . fmap mkProcProps
                                  $ group ^. groupVDocs
                                  )
      MainMenuGroupMembers -> mapM_ mainMenuMemberShort_
                            . fmap mkMemberProps $ group ^. groupMembers

mainMenuGroup_ :: HasCallStack => (MainMenuGroup, GroupProps) -> ReactElementM eventHandler ()
mainMenuGroup_ = view_ mainMenuGroup "mainMenuGroup"


mainMenuMemberShort :: HasCallStack => View '[User]
mainMenuMemberShort = mkView "MainMenuProcessShort" $ \props -> do
--  let listKey = "member-list-item-" <> (cs . show $ props ^. userID . unID)
  div_ [ {-onClick $ \_ _ -> simpleHandler . dispatch
                         . LoadVDoc $ props ^. mmprocShrtID

       , -}"style" @@= [ decl "backgroundColor" (Ident "rgba(84, 99, 122, 1)")
                     , decl "padding" (Px 50)
                     , decl "margin" (Px 50)
                     , decl "borderRadius" (Px 12)
                     ]
       ] $ do

    -- image
    br_ []
    ibutton_ $ emptyIbuttonProps "User" ([] :: [GlobalAction])
      & ibListKey .~ "user"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ True
      & ibHighlightWhen .~ HighlightNever
      & ibLabel .~ mempty

    -- title
    br_ []
    div_ [] $ do
      elemText $ props ^. userName
{-
    br_ []
    div_ [] $ do
      ibutton_ $ emptyIbuttonProps "Comment" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-comments")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ (cs . show $ props ^. mmprocShrtNumComments)

      ibutton_ $ emptyIbuttonProps "Edit" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-edits")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ (cs . show $ props ^. mmprocShrtNumEdits)

      ibutton_ $ emptyIbuttonProps "Group" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-groups")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ (cs . show $ props ^. mmprocShrtNumUsers)
-}
mainMenuMemberShort_ :: HasCallStack => Maybe User -> ReactElementM 'EventHandlerCode ()
mainMenuMemberShort_ Nothing = "Loading..."
mainMenuMemberShort_ (Just props) = view_ mainMenuMemberShort listKey props
  where
    listKey = "mainMenuMemberShort-" <> (cs . show $ props ^. userID . unID)

mainMenuProcessShort :: HasCallStack => View '[MainMenuProcessShortProps]
mainMenuProcessShort = mkView "MainMenuProcessShort" $ \props -> do
  let listKey = "process-list-item-" <> (cs . show $ props ^. mmprocShrtID . unID)
  div_ [ onClick $ \_ _ -> simpleHandler . dispatch
                         . LoadVDoc $ props ^. mmprocShrtID
       , "id" $= listKey  -- FUTUREWORK: get rid of this.  at the time of writing this is only
                          -- used in the acceptance test.

       , "style" @@= [ decl "backgroundColor" (Ident "rgba(84, 99, 122, 1)")
                     , decl "padding" (Px 50)
                     , decl "margin" (Px 50)
                     , decl "borderRadius" (Px 12)
                     ]
       ] $ do

    -- image
    br_ []
    ibutton_ $ emptyIbuttonProps "Process" ([] :: [GlobalAction])
      & ibListKey .~ "process"
      & ibSize .~ XXLarge
      & ibDarkBackground .~ True
      & ibHighlightWhen .~ HighlightNever
      & ibLabel .~ mempty

    -- title
    br_ []
    div_ [] $ do
      elemText $ props ^. mmprocShrtTitle . unTitle

    -- buttons for processes, users (FUTUREWORK: make those clickable in addition to the entire
    -- tile.  clicking on 'users' button should get you there directly.)
    br_ []
    div_ [] $ do
      ibutton_ $ emptyIbuttonProps "Comment" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-comments")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ (cs . show $ props ^. mmprocShrtNumComments)

      ibutton_ $ emptyIbuttonProps "Edit" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-edits")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ (cs . show $ props ^. mmprocShrtNumEdits)

      ibutton_ $ emptyIbuttonProps "User" ([] :: [GlobalAction])
        & ibListKey .~ (listKey <> "-users")
        & ibSize .~ Large
        & ibDarkBackground .~ True
        & ibHighlightWhen .~ HighlightNever
        & ibLabel .~ (cs . show $ props ^. mmprocShrtNumUsers)

mainMenuProcessShort_ :: HasCallStack => MainMenuProcessShortProps -> ReactElementM 'EventHandlerCode ()
mainMenuProcessShort_ props = view_ mainMenuProcessShort listKey props
  where
    listKey = "mainMenuProcessShort-" <> (cs . show $ props ^. mmprocShrtID . unID)

-- | FUTUREWORK: should this be @View '[LocalStateRef CreateGroup]@ or @View '[Maybe (ID Group),
-- LocalStateRef CreateGroup]@?  (same with 'mainMenuCreateProcess'.)
mainMenuCreateGroup :: HasCallStack => Maybe (ID Group) -> (LocalStateRef (CreateGroup_ [(User, Bool)]), Map (ID User) User) -> View '[]
mainMenuCreateGroup mid (lst, allusers)
  = mkPersistentStatefulView "MainMenuCreateGroup" lst (Just addUsers)
  $ \st@(CreateGroup title desc _ _ users) -> do

    contributionDialogTextForm createGroupTitle st 1 "group title"
    hr_ []
    contributionDialogTextForm createGroupDesc st 2 "group description"
    hr_ []
    "Invite Members"
    forM_ (zip [0..] users) $ \(i, (user, cv)) -> do
      br_ []
      div_ ["key" @= ("u-" <> show (user ^. userID . unID))] $ do
        input_ $
         ["checked" $= "checked" | cv] <>
         [ "style" @@= inputFieldStyles
         , "type" $= "checkbox"
         , onChange $ \_evt -> simpleHandler $ \st' -> ([], Just (st' & createGroupMembers . ix i . _2 %~ not))
         ]
        elemText $ user ^. userName <> " <" <> (user ^. userEmail) <> ">"
    hr_ []

    let enableOrDisable props = if ST.null desc || ST.null title
          then props
            & iconButtonPropsDisabled     .~ True
          else props
            & iconButtonPropsDisabled     .~ False
            & iconButtonPropsOnClick      .~ [MainMenuAction . MainMenuActionOpen . MainMenuCreateOrUpdateGroup mid $ FormComplete st]

    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "dark") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ maybe "save" (const "update") mid
            & iconButtonPropsAlignRight   .~ True
            & enableOrDisable

    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "cancel"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Close", "dark") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "cancel"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsDisabled     .~ False
            & iconButtonPropsOnClick      .~ [ MainMenuAction . MainMenuActionOpen $
                                               maybe (MainMenuGroups ()) (MainMenuGroup MainMenuGroupProcesses) mid
                                             ]
  where
    addUsers :: CreateGroup_ [(User, Bool)] -> CreateGroup_ [(User, Bool)]
    addUsers cg = cg & createGroupMembers %~ flip (foldr f) (Map.elems allusers)
      where
        f u asc = case lookup u asc of
          Nothing -> asc <> [(u, False)]
          _ -> asc


mainMenuCreateGroup_ :: HasCallStack => Maybe (ID Group) -> (LocalStateRef (CreateGroup_ [(User, Bool)]), Map (ID User) User) -> ReactElementM eventHandler ()
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

{-# NOINLINE currentTime #-}
currentTime :: (UTCTime -> a) -> b -> ReactElementM 'EventHandlerCode a
currentTime f x = unsafePerformIO $ x `seq` (pure . f <$> getCurrentTime)

mainMenuProfile :: HasCallStack => View '[CurrentUser, ImageUpload]
mainMenuProfile = mkView "MainMenuProfile" $ \user lst -> case user of
  UserLoggedIn u -> do

    let ID uid = u ^. userID
    elemText "current avatar"
    time <- currentTime (cs . show) lst
    img_ ["src" $= ("profile" <> cs (show uid) <> ".svg?t=" <> time)] $ pure ()

    let upd evt = case files of
          Just [f] -> simpleHandler . dispatch . MainMenuAction . MainMenuActionOpen . MainMenuProfile $ Just (NoJSONRep f, Nothing)
          _ -> simpleHandler []
          where
            files = unsafePerformIO $ fromJSVal $ target evt "files"

    input_ [ "type" $= "file"
           , onChange upd
           ]

    case lst of
      Just (NoJSONRep _f, Just source) -> do
        img_ ["src" $= cs source] $ pure ()
        button_
          [ onClick $ \_evt _ -> simpleHandler . dispatch $ UploadAvatar (u ^. userID) source
          ] $ elemText "upload"
      _ -> pure ()

  _ -> elemText "please log in"

mainMenuProfile_ :: HasCallStack => CurrentUser -> ImageUpload -> ReactElementM eventHandler ()
mainMenuProfile_ = view_ mainMenuProfile "mainMenuProfile_"

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
    contributionDialogTextForm (toTitle . unTitle) st 2 "title"
    hr_ []
    contributionDialogTextForm (toAbstract . unAbstract) st 2 "abstract"
    hr_ []

    let enableOrDisable props = if ST.null (st ^. toTitle . unTitle)
          then props
            & iconButtonPropsDisabled     .~ True
          else props
            & iconButtonPropsDisabled     .~ False
            & iconButtonPropsOnClick      .~ [save $ FormComplete st]

    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "save"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Save", "dark") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "save"
            & iconButtonPropsAlignRight   .~ True
            & enableOrDisable

    iconButton_ $ defaultIconButtonProps @[GlobalAction]
            & iconButtonPropsListKey      .~ "cancel"
            & iconButtonPropsIconProps    .~ IconProps "c-vdoc-toolbar" True ("icon-Close", "dark") XXLarge
            & iconButtonPropsElementName  .~ "btn-index"
            & iconButtonPropsLabel        .~ "cancel"
            & iconButtonPropsAlignRight   .~ True
            & iconButtonPropsDisabled     .~ False
            & iconButtonPropsOnClick      .~ [cancel]


mainMenuLoginTab :: HasCallStack => View '[MainMenuProps MainMenuSubTabLogin]
mainMenuLoginTab = mkView "MainMenuLoginTab" $ \(MainMenuProps currentTab menuErrors currentUser) -> do
  let tabButton :: Int -> MainMenuSubTabLogin -> ReactElementM eventHandler ()
      tabButton key this = div_ ["style" @@= [decl "marginLeft" (Px 40)]] $ do
        ibutton_ $ emptyIbuttonProps "00_joker" [MainMenuAction . MainMenuActionOpen . MainMenuLogin $ this]
          & ibListKey .~ cs (show key)
          & ibDarkBackground .~ False
          & ibHighlightWhen .~ (if currentTab == this then HighlightAlways else HighlightOnMouseOver)
          & ibLabel .~ (case this of
                         MainMenuSubTabLogin        -> "login"
                         MainMenuSubTabRegistration -> "register")

  div_ $ do
    tabButton 0 MainMenuSubTabLogin
    guardAccess_ "register" (AP.createUser [] []) $ tabButton 1 MainMenuSubTabRegistration

  br_ [] >> br_ [] >> br_ [] >> hr_ []

  div_ $ do
    case currentTab of
      MainMenuSubTabLogin        -> loginOrLogout_ currentUser (menuErrors ^. mmeLogin)
      MainMenuSubTabRegistration -> registration_  (menuErrors ^. mmeRegistration)

mainMenuLoginTab_ :: HasCallStack => MainMenuProps MainMenuSubTabLogin -> ReactElementM eventHandler ()
mainMenuLoginTab_ = view_ mainMenuLoginTab "mainMenuLoginTab_"
