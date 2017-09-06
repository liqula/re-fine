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
import           Refine.Common.Test hiding (assert)
import           Refine.Common.Types
import           Refine.Frontend.Contribution.Bubble
import           Refine.Frontend.Contribution.Dialog
import           Refine.Frontend.Contribution.Types
import           Refine.Frontend.Header.DiffToolbar
import           Refine.Frontend.Header.DiscussionToolbar
import           Refine.Frontend.Header.EditToolbar
import           Refine.Frontend.Header.Toolbar
import           Refine.Frontend.Header.Types
import           Refine.Frontend.Login.Component
import           Refine.Frontend.MainMenu.Component
import           Refine.Frontend.MainMenu.Types
import           Refine.Frontend.Screen.Types
import           Refine.Frontend.Test.Enzyme hiding (Prop)
import           Refine.Frontend.Util


-- * config

styleGuidePath :: FilePath
styleGuidePath = "./styleguide/html/"


-- * colors

colorValue :: Color -> RGBA
colorValue MainMenuBackground    = RGBA 179 190 200 1
colorValue MainMenuDarkBlue      = RGBA 36 48 65 1
colorValue MainMenuBlue          = RGBA 141 157 173 1
colorValue MainMenuIcon          = RGBA 210 217 221 1
colorValue ToolbarIcon           = RGBA 104 125 146 1
colorValue InteractionYellowNeon = RGBA 231 252 85 1
colorValue InteractionOrange     = RGBA 238 114 54 1
colorValue InteractionOrangeNeon = RGBA 237 100 43 1
colorValue InteractionRed        = RGBA 212 68 53 1
colorValue FormGreen             = RGBA 139 196 77 1
colorValue FormOrangeLight       = RGBA 249 215 194 1
colorValue FormOrangeMedium      = RGBA 232 119 72 1
colorValue FormOrangeStrong      = RGBA 237 100 43 1
colorValue FormBackground        = RGBA 179 190 200 1
colorValue FormIcon              = RGBA 28 40 51 1
colorValue NoteDark              = RGBA 169 153 61 1
colorValue NoteBubble            = RGBA 223 213 167 1
colorValue NoteBackground        = RGBA 242 235 210 1
colorValue DiscussionDark        = RGBA 121 129 63 1
colorValue DiscussionBubble      = RGBA 198 198 163 1
colorValue DiscussionBackground  = RGBA 229 228 208 1
colorValue EditDark              = RGBA 156 89 42 1
colorValue EditBubble            = RGBA 224 199 179 1
colorValue EditBackground        = RGBA 242 231 222 1
colorValue VoteNo                = RGBA 221 73 43 1
colorValue VoteYes               = RGBA 139 196 77 1

data Color
  = MainMenuBackground
  | MainMenuDarkBlue
  | MainMenuBlue
  | MainMenuIcon
  | ToolbarIcon
  | InteractionYellowNeon
  | InteractionOrange
  | InteractionOrangeNeon
  | InteractionRed
  | FormGreen
  | FormOrangeLight
  | FormOrangeMedium
  | FormOrangeStrong
  | FormBackground
  | FormIcon
  | NoteDark
  | NoteBubble
  | NoteBackground
  | DiscussionDark
  | DiscussionBubble
  | DiscussionBackground
  | EditDark
  | EditBubble
  | EditBackground
  | VoteNo
  | VoteYes
  deriving (Eq, Show, Ord, Bounded, Enum)


data RGBA = RGBA Int Int Int Double
  deriving (Eq, Show)

showRGBA :: RGBA -> ST
showRGBA (RGBA r g b a) = cs $ "rgba(" <> intercalate ", " [show r, show g, show b, show a] <> ")"

rgbFromHex :: HasCallStack => ST -> RGBA
rgbFromHex num = f $ toLower <$> cs num
  where
    f ('#' : xs) = f xs
    f [r1, r2, g1, g2, b1, b2] = RGBA (h r1 r2) (h g1 g2) (h b1 b2) 1
    f _   = error $ "rgbFromHex: bad input: " <> show num
    h x1 x2 = 16 * q x1 + q x2
    q '0' = 0
    q '1' = 1
    q '2' = 2
    q '3' = 3
    q '4' = 4
    q '5' = 5
    q '6' = 6
    q '7' = 7
    q '8' = 8
    q '9' = 9
    q 'a' = 10
    q 'b' = 11
    q 'c' = 12
    q 'd' = 13
    q 'e' = 14
    q 'f' = 15
    q _   = error $ "rgbFromHex: bad input: " <> show num

camelToUnderscore :: ST -> ST
camelToUnderscore = cs . go True . cs
  where
    go :: Bool -> String -> String
    go _ "" = ""
    go isfirst (x:xs)
      | isUpper x && isfirst = toLower x       : go False xs
      | isUpper x            = '_' : toLower x : go False xs
      | otherwise            = x               : go False xs

colorName :: Color -> ST
colorName = ("c_" <>) . camelToUnderscore . cs . show

bgColorName :: Color -> ST
bgColorName = ("c_bg_" <>) . camelToUnderscore . cs . show

colorCssVar :: Color -> ST
colorCssVar c = mconcat ["$", colorName c, ": ", showRGBA $ colorValue c, ";"]

colorCssClass :: Color -> ST
colorCssClass c = mconcat [".", colorName c, " { color: $", colorName c, " }"]

bgColorCssClass :: Color -> ST
bgColorCssClass c = mconcat [".", bgColorName c, " { background-color: $", colorName c, " }"]

colorCssFile :: ST
colorCssFile = ST.unlines $ header <> ([colorCssVar, colorCssClass, bgColorCssClass] <*> [minBound ..])
  where
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
      , "  <div class=\"color " <> bgColorName color <> "\">"
      , "  </div>"
      , "  <div class=\"description\">"
      , "    Variable:&nbsp;&nbsp;<span class=\"black\">$" <> colorName color <> "</span><br>"
      , "    Class (color):&nbsp;&nbsp;<span class=\"black\">" <> colorName color <> "</span><br>"
      , "    Class (background-color):&nbsp;&nbsp;<span class=\"black\">" <> bgColorName color <> "</span><br>"
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

    , ("menu/login_", login_ Nothing, [])
    , ("menu/mainMenuGroups_", mainMenuGroups_
        ( [sampleGroup1, sampleGroup2]
        , Map.singleton (ID 0) sampleVDoc
        , mkUserMap [ User sampleMetaID "user" "<email@bla.com>" Nothing
                    , User sampleMetaID "üsör" "<grr@bla.com>" Nothing & userID .~ ID 8479]
        ), [])
    , ("menu/mainMenuGroup_", mainMenuGroup_ . (,) MainMenuGroupProcesses $
        ( Just sampleGroup1
        , Map.singleton (ID 0) sampleVDoc
        , mkUserMap [ User sampleMetaID "user" "<email@bla.com>" Nothing
                    , User sampleMetaID "üsör" "<grr@bla.com>" Nothing & userID .~ ID 8479]
        ), [])
    -- , ("mainMenuCreateGroup_", mainMenuCreateGroup_ _, [])
    -- , ("mainMenuCreateProcess_", mainMenuCreateProcess_ _, [])

    , ("toolbar/toolbar_", toolbar_ sampleVDoc, [])
    , ("toolbar/editToolbar_", editToolbar_ $ EditToolbarProps EditIsNotInitial LinkButtonDisabled, [])
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
                                           (newLocalStateRef (EditInputState info (Just Grammar)) '3')
                              in info
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
main = hspec spec

spec :: Spec
spec = describe "@STYLEGUIDE" $ do
    describe "colorPalette"      colorPalette
    describe "validate-before" $ validateStyleGuide >> checkWorkingCopy
    describe "generate"        $ generateIndexHtml >> generateStyleGuide `mapM_` viewsSources
    describe "validate-after"  $ validateStyleGuide >> checkWorkingCopy
