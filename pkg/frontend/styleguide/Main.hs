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
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-unused-imports -Wno-incomplete-patterns #-}

module Main where

import Refine.Frontend.Prelude

import           Data.Char
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Language.Css.Syntax as Css
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Test.Hspec

import           React.Flux.Missing
import           Refine.Common.Color
import           Refine.Common.Test hiding (assert)
import           Refine.Common.Types
import           Refine.Frontend.Access
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Document.FFI (createEmpty)
import           Refine.Frontend.Document.Types
import           Refine.Frontend.Header.DiffToolbar
import           Refine.Frontend.Header.DiscussionToolbar
import           Refine.Frontend.Header.EditToolbar
import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Icon
import qualified Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Login.Component
import           Refine.Frontend.MainMenu.Component
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Store
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Test.Enzyme hiding (Prop)
import           Refine.Frontend.Util
import           Refine.Frontend.Views


-- * config

styleGuidePath :: FilePath
styleGuidePath = "./styleguide/html/"


-- * colors

colorCssVarDecl :: Color -> ST
colorCssVarDecl c = mconcat
  ["$", colorName ColorAttrDefault c, ": ", showRGBA $ colorValue c, ";"]

colorCssClassDecl :: ColorAttr -> Color -> ST
colorCssClassDecl attr c = mconcat
  [".", colorName attr c, " { ", showAttr attr, ": $", colorName ColorAttrDefault c, " }"]
  where
    showAttr ColorAttrDefault         = "color"
    showAttr ColorAttrBackgroundColor = "background-color"
    showAttr ColorAttrFill            = "fill"

colorCssFile :: ST
colorCssFile = ST.unlines $ header <> (kinds <*> [minBound ..])
  where
    kinds = colorCssVarDecl : (colorCssClassDecl <$> [minBound..])
    header =
      [ "// this file is auto-generated by the `styleguide` test suite."
      , "// you can edit it, but you need to carry the changes over to `../styleguide/Main.hs`."
      , ""
      ]

colorHtmlFile :: ST
colorHtmlFile = ST.unlines $ header <> mconcat (go <$> [minBound ..]) <> footer
  where
    title = "Color Palette"
    header =
      [ "<!doctype html>"
      , "<html>"
      , "<head>"
      , "<meta charset=\"UTF_8\">"
      , "<title>" <> title <> "</title>"
      , "<link href=\"../../scss-new/main.css\" rel=\"stylesheet\" type=\"text/css\">"
      , "<style>"
      , "  body{font-family: 'Source Sans Pro', sans-serif; font-weight: 300; font-size: 14px;}"
      , "  .black{font-weight: 800;}"
      , "  .container{"
      , "    position: relative;"
      , "    clear: both;"
      , "    height: 120px;"
      , "  }"
      , "  .color{"
      , "    width: 60px;"
      , "    height: 120px;"
      , "    float: left;"
      , "    margin-right: 10px;"
      , "  }"
      , "  .description{"
      , "    padding-top: 12px;"
      , "  }"
      , "  h1, h2{font-weight: 300;}"
      , "</style>"
      , "</head>"
      , ""
      , "<body>"
      , "<h1>" <> title <> "</h1>"
      ]
    footer =
      [ "</body>"
      , "</html>"
      ]

    go :: Color -> [ST]
    go color =
      [ "<div class=\"container\">"
      , "  <div class=\"color " <> colorName ColorAttrBackgroundColor color <> "\">"
      , "  </div>"
      , "  <div class=\"description\">"
      , "    Variable:&nbsp;&nbsp;<span class=\"black\">$" <> colorName ColorAttrDefault color <> "</span><br>"
      , "    Class (color):&nbsp;&nbsp;<span class=\"black\">" <> colorName ColorAttrDefault color <> "</span><br>"
      , "    Class (background-color):&nbsp;&nbsp;<span class=\"black\">" <> colorName ColorAttrBackgroundColor color <> "</span><br>"
      , "    Color:&nbsp;&nbsp;<span class=\"black\">" <> showRGBA (colorValue color) <> "</span><br>"
      , "  </div>"
      , "</div>"
      , "<br>"
      ]

colorPalette :: Spec
colorPalette = it "colorPalette" $ do
  ST.writeFile "./scss-new/20-colors.scss" colorCssFile
  ST.writeFile (styleGuidePath <> "colorPalette.html") colorHtmlFile


-- * combinators

clearHtml :: Spec
clearHtml = it "remove old html code" $ do
  (_, lines -> vcfiles, "")
      <- readProcessWithExitCode "/usr/bin/git" ["grep", "-l", ".", styleGuidePath] mempty
  (_, _, _)
      <- readProcessWithExitCode "/bin/rm" vcfiles mempty
  pure ()

wrapHtml :: FilePath -> [String] -> String -> String
wrapHtml nm ctx = (unlines header <>) . (<> unlines footer)
  where
    cssSource :: FilePath = relPath </> "main.css"
    relPath :: FilePath = assert (not $ "./" `isInfixOf` nm)  -- (without this assertion this hack won't work)
      . mconcat
      $ replicate (length $ filter (== '/') nm) "../"

    header =
      "<!DOCTYPE html>" :
      "<html>" :
      "<head>" :
      "  <meta charset=\"utf-8\">" :
      "  <title>liqlua style guide</title>" :
      "  <link rel=\"stylesheet\" media=\"all\" href=\"" <> cssSource <> "\" />" :
      "</head>" :
      "<body>" :
      ["  <div class=\"" <> unwords ctx <> "\">" | not $ null ctx]

    footer =
      ["  </div>" | not $ null ctx] <>
      ("</body>" :
      "</html>" :
      [])

-- | Run a haskell view and store the generated html in a file.
generateStyleGuide :: HasCallStack => (String, ReactElementM h (), [String]) -> Spec
generateStyleGuide (nm, comp, ctx) = it ("generateStyleGuide: " <> nm) $ do
  createDirectoryIfMissing True (styleGuidePath <> takeDirectory nm)
  writeFile (styleGuidePath <> nm <> ".html") . wrapHtml nm ctx . cs =<< html =<< mount comp

generateIndexHtml :: HasCallStack => Spec
generateIndexHtml = it "generateIndexHtml" $ do
  nms <- viewsFiles
  writeFile (styleGuidePath <> "index.html") . unlines $
    ["<!DOCTYPE html><html><body>"] <>
    ((\nm -> "<a href=\"" <> nm <> "\">" <> nm <> "</a><br/>") <$> nms) <>
    ["<br><hr><br><a href=\"colorPalette.html\">colorPalette.html</a>"] <>
    ["</body></html>"]

-- | Pretty-print, canonicalize, validate existing html.  This is called in the tests after
-- 'generateStyleGuide', and should also be called before and after manual edit to supress diff
-- noise.
--
-- FUTUREWORK: use `apt-get install w3c-markup-validator` to get more html errors?  it's an ugly cgi
-- script and may not be fun to deploy, but it may be well-maintained.
validateStyleGuide :: HasCallStack => Spec
validateStyleGuide = it "validateStyleGuide" $ viewsFiles >>= mapM_ (val . (styleGuidePath <>))
  where
    val  nm = do
      raw <- pretty nm =<< readFile nm
      force raw `seq` writeFile nm raw

    force s = case cs s of ((>0) . ST.length -> True) -> s

-- | use tidy(1) to do html pretty-printing and sanity checking.
--
-- Rationale: HaXml breaks the HTML tree!  (it turns siblings into ancestors / descendants) hxt does not
-- have a pretty-printer.  the other options are confusing and un-promising for the task at hand.
-- also, we may want to use other tools for html sanity checking here that are not available on
-- hackage.
pretty :: String -> String -> IO String
pretty nm content = do
  -- NOTE: 'system' has dynamic type errors in ghcjs/node!
  -- NOTE: 'rawSystem' can be called, but if tidy is not installed it triggers a js exception that
  -- cannot be caught here, so the following test is not producing the nice error message that i
  -- hoped.
  -- void (rawSystem "/usr/bin/tidy" []) `catch`
  --   \(SomeException msg) -> error $ show msg <> "\n\n*** try `apt-get install tidy`?\n\n"

  (code, o, e) <- readProcessWithExitCode "/usr/bin/tidy" ["--tidy-mark", "no", "-i"] content
  let err msg = error $ "pretty " <> msg <> ": "     <> show (nm, code, o, e)
  case code of
    ExitSuccess   -> pure o
    ExitFailure 1 -> pure o  -- (call @err "warnings"@ here?)
    ExitFailure 2 -> err "errors"
    ExitFailure _ -> err "UNEXPECTED"

-- | Make sure the style guide working copy contains no changes wrt. the last commit.  Run this
-- after generating content to make sure the the haskell code is in sync with the style guide.
checkWorkingCopy :: HasCallStack => Spec
checkWorkingCopy = it "checkWorkingCopy" $ do
  filter isDirty <$> gitStatus `shouldReturn` []


-- * sample content

toy_ :: forall h. ReactElementM h ()
toy_ = div_ [style [ "margin" ||= Css.Px 10
                   , "border" ||= Css.Px 2
                   , "border" ||= [Css.Ident "dashed", Css.Ident "black"]
                   ]
            ] $ do
  div_ [style (box "red")] $ do
    div_ [style (box "blue")] mempty
    div_ [style (box "blue")] mempty
    div_ [style (box "blue")] $ do
      div_ [style (box "red")] mempty
      div_ [style (box "red")] mempty
      div_ [style (box "red")] mempty
    div_ [style (box "green")] mempty
    div_ [style (box "gray")] mempty
  div_ [style (box "gray")] mempty
  div_ [style (box "gray")] mempty
  div_ [style (box "gray")] mempty

  where
    box :: String -> [Css.Decl]
    box bg =
      [ "border" ||= [Css.Ident "dashed", Css.Ident "black", Css.Ident "2px"]
      , "padding" ||= Css.Px 50
      , "margin" ||= Css.Px 60
      , "width" ||= Css.Px 70
      , "height" ||= Css.Px 70
      , "backgroundColor" ||= Css.Ident bg
      ]


-- * production content

-- | NOTE: listDirectory throws a `("setErrno not yet implemented: " + e);`, so we just take
-- `viewsSources`' word for it.
viewsFiles :: IO [FilePath]
viewsFiles = pure $ (<> ".html") . view _1 <$> viewsSources
  -- (styleGuidePath <>) <$$> listDirectoryRec styleGuidePath

viewsSources :: [(String, ReactElementM 'EventHandlerCode (), [String])]
viewsSources =
    [ ("toy_", toy_, ["testclass"])

    , ("wholeScreen_/empty-empty", wholeScreen_ emptyAccessState emptyGlobalState, [])
    , let Just gs = decode "{\"_gsPageState\":{\"PageStateMainMenu\":[{\"_mmErrors\":{\"_mmeRegistration\":null,\"_mmeLogin\":null},\"_mmState\":{\"MainMenuLogin\":\"MainMenuSubTabLogin\"}},null]},\"_gsServerCache\":{\"_scUsers\":[],\"_scUserIds\":null,\"_scGroupIds\":null,\"_scDiscussions\":[],\"_scVDocs\":[],\"_scEdits\":[],\"_scGroups\":[]},\"_gsDevState\":null,\"_gsScreenState\":{\"_ssHeaderHeight\":0,\"_ssWindowSize\":\"Desktop\",\"_ssWindowWidth\":0}}"
      in ("wholeScreen_/empty-login", wholeScreen_ emptyAccessState gs, [])
    , let Just as = decode "{\"_accGlobalRoles\":[],\"_accLoginState\":{\"UserLoggedIn\":{\"_loggedInUser\":1}},\"_accGroupRoles\":[],\"_accDispatchAfterLogin\":[]}"
          Just gs = decode "{\"_gsPageState\":{\"PageStateMainMenu\":[{\"_mmErrors\":{\"_mmeRegistration\":null,\"_mmeLogin\":null},\"_mmState\":{\"MainMenuLogin\":\"MainMenuSubTabLogin\"}},null]},\"_gsServerCache\":{\"_scUsers\":[[1,{\"_userEmail\":\"admin@localhost\",\"_userDescription\":\"\",\"_userAvatar\":null,\"_userMetaID\":{\"_miID\":1,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:24:29.335107782Z\",\"_metaChangedAt\":\"2017-10-08T11:24:29.335107782Z\",\"_metaChangedBy\":\"Anonymous\",\"_metaCreatedBy\":\"Anonymous\"}},\"_userName\":\"admin\"}]],\"_scUserIds\":null,\"_scGroupIds\":null,\"_scDiscussions\":[],\"_scVDocs\":[],\"_scEdits\":[],\"_scGroups\":[]},\"_gsDevState\":null,\"_gsScreenState\":{\"_ssHeaderHeight\":0,\"_ssWindowSize\":\"Desktop\",\"_ssWindowWidth\":0}}"
      in ("wholeScreen_/empty-iamadmin", wholeScreen_ as gs, [])

    -- FIXME: it would be nicer to have these in separate files, and generate them from those file on the fly.  nice task for after work.  :)
    , let Just as = decode "{\"_accGlobalRoles\":[],\"_accLoginState\":{\"UserLoggedIn\":{\"_loggedInUser\":1}},\"_accGroupRoles\":[],\"_accDispatchAfterLogin\":[]}"
          Just gs = decode "{\"_gsPageState\":{\"PageStateMainMenu\":[{\"_mmErrors\":{\"_mmeRegistration\":null,\"_mmeLogin\":null},\"_mmState\":{\"MainMenuProfile\":[1,\"NoJSONRep\"]}},{\"PageStateVDoc\":{\"_psVDocID\":1,\"_psHeaderState\":{\"_hsReadOnly\":false,\"_hsToolbarExtensionStatus\":\"ToolbarExtensionClosed\",\"_hsDiscussionFlatView\":false},\"_psDocumentState\":{\"DocumentStateView\":{\"_documentStateContent\":[]}},\"_psContributionState\":{\"_csCurrentSelectionWithPx\":null,\"_csDisplayedContributionID\":null,\"_csQuickCreateShowState\":\"QuickCreateNotShown\",\"_csHighlightedMarkAndBubble\":[],\"_csAllVerticalSpanBounds\":{\"MarkContribution (ContribIDDiscussion False (ID 1)) 0\":{\"_verticalSpanBoundsBottom\":291,\"_verticalSpanBoundsTop\":273}},\"_csBubblePositioning\":\"BubblePositioningAbsolute\",\"_csActiveDialog\":null,\"_csBubbleFilter\":null}}}]},\"_gsServerCache\":{\"_scUsers\":[[1,{\"_userEmail\":\"admin@localhost\",\"_userDescription\":\"wef\",\"_userAvatar\":\"data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI1MDAiIGhlaWdodD0iMjE2IiB2aWV3Qm94PSIwIDAgMjUwMCAxMDgwIiBzdHlsZT0iZmlsbDogd2hpdGUiPjx0aXRsZT5saXF1bGE8L3RpdGxlPjxwYXRoIGQ9Ik03MC4zLDE1LjE3QTE1LjE3LDE1LjE3LDAsMSwxLDU1LjEzLDAsMTUuMTcsMTUuMTcsMCwwLDEsNzAuMywxNS4xN1pNMTkzLjE2LDk5LjRsLjEuMjhWMjE2SDE2OC42OVYxNDQuNjhhNTMuNjIsNTMuNjIsMCwxLDEsMTUuNzgtNzQuMjVBNTMuNDQsNTMuNDQsMCwwLDEsMTkzLjE2LDk5LjRabS0yNC40Ny4yOGMwLS42MS0uMDUtMS4yLS4wOS0xLjhhMjkuMjQsMjkuMjQsMCwxLDAtMjkuMTcsMzEuMDZjMTUuNTIsMCwyOC4zMi0xMy4wNywyOS4yNi0yOC4zNlpNNDIuODUsMTUwLjg1SDY3LjQyVjQ1Ljk0SDQyLjg1Wk0wLDE1MC44NUgyNC41N1YwSDBabTQ2My00OS4zN2MwLS42LjA5LTEuMTkuMDktMS44cy0uMDUtMS4yLS4wOS0xLjhabTM3LDQ5LjhjLTMsMS4yOC03LjI2LDIuMTQtMTMsMi4xNC0xMC40NywwLTE2Ljk0LTQuMDctMjAuNDgtMTEuMTktMS4xMy44Ny0yLjI1LDEuNzctMy40NSwyLjU1YTUzLjc5LDUzLjc5LDAsMSwxLDAtOTAuMXYwYTU0LjA3LDU0LjA3LDAsMCwxLDE1Ljg3LDE1Ljc0djBhNTMuNDQsNTMuNDQsMCwwLDEsOC43LDI5LjI2djI1LjUzYzAsNiwyLjc4LDguMTIsNS4zNCw4LjEyYTE0Ljc4LDE0Ljc4LDAsMCwwLDMuODUtLjQzWm0tMzYuODctNTEuNmMwLS42MS0uMDUtMS4yLS4wOS0xLjhhMjkuMjYsMjkuMjYsMCwxLDAsMCwzLjZDNDYzLjA3LDEwMC44OCw0NjMuMTMsMTAwLjI5LDQ2My4xMyw5OS42OFpNMzM3LjI4LDE1MC44NWgyNC41N1YwSDMzNy4yOFpNMjk0LjUzLDU0Ljc0Vjk5LjY4YzAsLjYxLS4wNSwxLjItLjA5LDEuOEEyOS4yNCwyOS4yNCwwLDAsMSwyMzYsOTkuNjhWNDUuOTRIMjExLjUzVjk5LjY4YTUzLjc0LDUzLjc0LDAsMCwwLDEwNy40OCwwVjQ1Ljk0SDI5NC41M1ptLS4wOSw0My4xNHYzLjZjMC0uNi4wOS0xLjE5LjA5LTEuOFMyOTQuNDcsOTguNDgsMjk0LjQ0LDk3Ljg4WiIvPjwvc3ZnPgo=\",\"_userMetaID\":{\"_miID\":1,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:24:29.335107782Z\",\"_metaChangedAt\":\"2017-10-08T11:24:29.335107782Z\",\"_metaChangedBy\":\"Anonymous\",\"_metaCreatedBy\":\"Anonymous\"}},\"_userName\":\"admin\"}]],\"_scUserIds\":null,\"_scGroupIds\":[1,2],\"_scDiscussions\":[[1,{\"_discussionMetaID\":{\"_miID\":1,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:27:33.92341169Z\",\"_metaChangedAt\":\"2017-10-08T11:27:33.92341169Z\",\"_metaChangedBy\":{\"UserID\":2},\"_metaCreatedBy\":{\"UserID\":2}}},\"_discussionVDoc\":1,\"_discussionVotes\":[],\"_discussionIsNote\":false,\"_discussionTree\":[{\"_statementParent\":null,\"_statementVDoc\":1,\"_statementText\":\"oijoi\",\"_statementMetaID\":{\"_miID\":1,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:27:33.92563959Z\",\"_metaChangedAt\":\"2017-10-08T11:27:33.92563959Z\",\"_metaChangedBy\":{\"UserID\":2},\"_metaCreatedBy\":{\"UserID\":2}}}},[[{\"_statementParent\":1,\"_statementVDoc\":1,\"_statementText\":\"oijoij 77u\",\"_statementMetaID\":{\"_miID\":2,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:28:02.291598753Z\",\"_metaChangedAt\":\"2017-10-08T11:28:37.472229866Z\",\"_metaChangedBy\":{\"UserID\":2},\"_metaCreatedBy\":{\"UserID\":2}}}},[]]]]}]],\"_scVDocs\":[[1,{\"_vdocHeadEdit\":2,\"_vdocGroup\":2,\"_vdocImage\":null,\"_vdocMetaID\":{\"_miID\":1,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:27:03.378319582Z\",\"_metaChangedAt\":\"2017-10-08T11:27:17.720248231Z\",\"_metaChangedBy\":{\"UserID\":2},\"_metaCreatedBy\":{\"UserID\":1}}},\"_vdocTitle\":\"blabla\",\"_vdocStats\":{\"_editStatsComments\":1,\"_editStatsEdits\":0,\"_editStatsUsers\":2},\"_vdocAbstract\":\"blabla\"}]],\"_scEdits\":[[2,{\"_editDiscussions'\":[[1,{\"_rangeBegin\":{\"_columnIndex\":0,\"_blockIndex\":{\"_blockIndexKey\":\"b0\",\"_blockIndexIndex\":0}},\"_rangeEnd\":{\"_columnIndex\":0,\"_blockIndex\":{\"_blockIndexKey\":\"b0\",\"_blockIndexIndex\":0}}}]],\"_editVDocVersion\":{\"entityMap\":{},\"blocks\":[{\"key\":\"b0\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"oj\"},{\"key\":\"e1g7o\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"w\"},{\"key\":\"1h2hj\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"ef\"},{\"key\":\"as0j0\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"ewqf\"},{\"key\":\"ff5jg\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"wefw\"},{\"key\":\"3amaq\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"ef\"},{\"key\":\"5nbkk\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"w\"},{\"key\":\"beqcl\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"ef\"},{\"key\":\"83lbo\",\"depth\":0,\"type\":\"unstyled\",\"entityRanges\":[],\"inlineStyleRanges\":[],\"text\":\"\"}]},\"_editVDoc\":1,\"_editChildren\":[],\"_editKind\":\"Initial\",\"_editMetaID\":{\"_miID\":2,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:27:17.711187545Z\",\"_metaChangedAt\":\"2017-10-08T11:27:17.711187545Z\",\"_metaChangedBy\":{\"UserID\":2},\"_metaCreatedBy\":{\"UserID\":2}}},\"_editSource\":[[[{\"unERawContent\":[{\"unENonEmpty\":{\"tag\":\"EditItem\",\"contents\":[0,[{\"tag\":\"EditSecond\",\"contents\":{\"tag\":\"SegmentListEdit\",\"contents\":{\"tag\":\"InsertItem\",\"contents\":[0,[[null,[]],\"oj\"]]}}}]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[1,[[[\"unstyled\",0],\"e1g7o\"],[[[null,[]],\"w\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[2,[[[\"unstyled\",0],\"1h2hj\"],[[[null,[]],\"ef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[3,[[[\"unstyled\",0],\"as0j0\"],[[[null,[]],\"ewqf\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[4,[[[\"unstyled\",0],\"ff5jg\"],[[[null,[]],\"wefw\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[5,[[[\"unstyled\",0],\"3amaq\"],[[[null,[]],\"ef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[6,[[[\"unstyled\",0],\"5nbkk\"],[[[null,[]],\"w\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[7,[[[\"unstyled\",0],\"beqcl\"],[[[null,[]],\"ef\"]]]]}},{\"unENonEmpty\":{\"tag\":\"InsertItem\",\"contents\":[8,[[[\"unstyled\",0],\"83lbo\"],[]]]}}]}],1]],\"_editDesc\":\"initial content\",\"_editVotes\":[]}]],\"_scGroups\":[[1,{\"_groupParents\":[],\"_groupVDocs\":[],\"_groupTitle\":\"Universe\",\"_groupDesc\":\"The group that contains everything\",\"_groupImage\":null,\"_groupMetaID\":{\"_miID\":1,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:24:29.312710462Z\",\"_metaChangedAt\":\"2017-10-08T11:24:29.312710462Z\",\"_metaChangedBy\":\"Anonymous\",\"_metaCreatedBy\":\"Anonymous\"}},\"_groupChildren\":[],\"_groupMembers\":[]}],[2,{\"_groupParents\":[],\"_groupVDocs\":[1],\"_groupTitle\":\"Greek Party\",\"_groupDesc\":\"Something about ethnics and politics\",\"_groupImage\":null,\"_groupMetaID\":{\"_miID\":2,\"_miMeta\":{\"_metaCreatedAt\":\"2017-10-08T11:24:29.314100748Z\",\"_metaChangedAt\":\"2017-10-08T11:24:29.314100748Z\",\"_metaChangedBy\":\"Anonymous\",\"_metaCreatedBy\":\"Anonymous\"}},\"_groupChildren\":[],\"_groupMembers\":[]}]]},\"_gsDevState\":null,\"_gsScreenState\":{\"_ssHeaderHeight\":266,\"_ssWindowSize\":\"Tablet\",\"_ssWindowWidth\":702}}"
      in ("wholeScreen_/empty-iamadmin", wholeScreen_ as gs, [])

    , ("menu/login_", login_ Nothing, [])
    , ("menu/mainMenuGroups_", view_ mainMenuGroups "mainMenuGroups"
        ( GroupsProps
            [sampleGroup1, sampleGroup2]
            (Map.singleton (ID 0) sampleVDoc)
            (mkUserMap [ User sampleMetaID "user" "<email@bla.com>" Nothing ""
                       , User sampleMetaID "üsör" "<grr@bla.com>" Nothing "" & userID .~ ID 8479])
        ), [])
    , ("menu/mainMenuGroup_", view_ mainMenuGroup "mainMenuGroup"
        ( MainMenuGroupProcesses
        , GroupProps
            (Just sampleGroup1)
            (Map.singleton (ID 0) sampleVDoc)
            (mkUserMap [ User sampleMetaID "user" "<email@bla.com>" Nothing ""
                       , User sampleMetaID "üsör" "<grr@bla.com>" Nothing "" & userID .~ ID 8479])
        ), [])
    -- , ("mainMenuCreateGroup_", mainMenuCreateGroup_ _, [])
    -- , ("mainMenuCreateProcess_", mainMenuCreateProcess_ _, [])

    , ("toolbar/toolbar_", toolbar_ sampleVDoc, [])
    , ("toolbar/editToolbar_", editToolbar_ $ EditToolbarProps EditIsNotInitial, [])
    , ("toolbar/diffToolbar_", diffToolbar_ DiffToolbarProps
        { _diffToolbarPropsEditID = ID 0
        , _diffToolbarIndex       = EditIndex 1 3
        , _diffToolbarEditKind    = Meaning
        , _diffToolbarPropsVotes  = mempty
        , _diffToolbarCollapsed   = False
        , _diffToolbarEditable    = False
        }, [])
    , ("toolbar/discussionToolbar_.flat", discussionToolbar_ $ DiscussionToolbarProps (Just $ ID 0) True False mempty, [])
    , ("toolbar/discussionToolbar_.tree", discussionToolbar_ $ DiscussionToolbarProps (Just $ ID 0) False False mempty, [])

    , ("bubble_.edit", bubble_ BubbleProps
        { _bubblePropsContributionIds   = NoStack (ContribIDEdit (ID 0), 0)
        , _bubblePropsIconSide          = BubbleRight
        , _bubblePropsVerticalOffset    = Nothing
        , _bubblePropsHighlight         = False
        , _bubblePropsScreenState       = ScreenState 0 0 Desktop
        } (elemText "lorem ipsum"), [])

    , ("bubble_.discussion.active.desktop", bubble_ BubbleProps
        { _bubblePropsContributionIds   = NoStack (ContribIDDiscussion False (ID 0), 0)
        , _bubblePropsIconSide          = BubbleLeft
        , _bubblePropsVerticalOffset    = Nothing
        , _bubblePropsHighlight         = True
        , _bubblePropsScreenState       = ScreenState 0 0 Desktop
        } (elemText "lorem ipsum"), [])

    , ("bubble_.discussion.active.mobile", bubble_ BubbleProps
        { _bubblePropsContributionIds   = NoStack (ContribIDDiscussion False (ID 0), 0)
        , _bubblePropsIconSide          = BubbleLeft
        , _bubblePropsVerticalOffset    = Nothing
        , _bubblePropsHighlight         = True
        , _bubblePropsScreenState       = ScreenState 0 0 Mobile
        } (elemText "lorem ipsum"), [])

    , ("bubble_.stack", bubble_ BubbleProps
        { _bubblePropsContributionIds   = Stack ((ContribIDEdit (ID 0), 0) :| [(ContribIDEdit (ID 0), 0)])
        , _bubblePropsIconSide          = BubbleLeft
        , _bubblePropsVerticalOffset    = Nothing
        , _bubblePropsHighlight         = False
        , _bubblePropsScreenState       = ScreenState 0 0 Desktop
        } (elemText "lorem ipsum"), [])

    , ("addEdit_", addEdit_ AddContributionProps
        { _acpRange         = Nothing
        , _acpLocalState    = let info = EditInfo "this is what i did" (Just Grammar)
                                           (newLocalStateRef st '3')
                                  st = EditInputState info [(Grammar, ButtonState Svg.Pressed Svg.NotRollOver)]
                              in (info, createEmpty)
        , _acpWindowWidth   = 1600
        }, [])
    ]
  where
    mkUserMap = Map.fromList . map ((^. userID) &&& id)


-- * git (cloned from `/scripts/style-check.hs`)

data GitStatus = GitStatus GitStatusCode GitStatusCode FilePath
  deriving (Eq, Show, Ord)

data GitStatusCode =
    Unmodified
  | Modified
  | Added
  | Deleted
  | Renamed
  | Copied
  | UpdatedButUnmerged
  | Untracked
  | Ignored
  deriving (Eq, Show, Ord)

isDirty :: GitStatus -> Bool
isDirty (GitStatus _ Ignored _)   = False
isDirty _                         = True

gitStatus :: IO [GitStatus]
gitStatus = do
  (code, o, e) <- readProcessWithExitCode "/usr/bin/git" ["status", "--porcelain", styleGuidePath] ""
  case code of
    ExitSuccess -> pure . fmap prse . ST.lines . cs $ o
    _ -> error $ "gitStatus: " <> show (code, o, e)
  where
    prse :: ST -> GitStatus
    prse line = GitStatus (parseCode ix_) (parseCode wt_) (cs file_)
      where
        ix_   = ST.take 1               line
        wt_   = ST.take 1 . ST.drop 1 $ line
        file_ =             ST.drop 3   line

    parseCode :: ST -> GitStatusCode
    parseCode " " = Unmodified
    parseCode "M" = Modified
    parseCode "A" = Added
    parseCode "D" = Deleted
    parseCode "R" = Renamed
    parseCode "C" = Copied
    parseCode "U" = UpdatedButUnmerged
    parseCode "?" = Untracked
    parseCode "!" = Ignored
    parseCode bad = error $ "gitStatus: could not parse status code " <> show bad


-- * main

main :: IO ()
main = do
  registerInitialStore emptyGlobalState
  registerInitialStore emptyAccessState
  registerInitialStore emptyEditorStore
  initRouting
  hspec spec

spec :: Spec
spec = describe "@STYLEGUIDE" $ do
    describe "clearHtml"      clearHtml
    describe "color palette"  colorPalette
    describe "generate views" (generateStyleGuide `mapM_` viewsSources)
    describe "generate index" generateIndexHtml
    describe "validate"       (validateStyleGuide >> checkWorkingCopy)
