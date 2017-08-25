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

-- import           Language.Css.Build.Attributes hiding (style)
-- import           Language.Css.Build hiding ((|=))
-- import           Language.Css.Build.Idents hiding (show)
-- import           Language.Css.Build.Tags hiding (style, html)
import           Language.Css.Syntax
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Test.Hspec

import           React.Flux.Missing
import           Refine.Common.Test.Samples
import           Refine.Common.Types
import           Refine.Frontend.Colors
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

  (code, o, e) <- readProcessWithExitCode "/usr/bin/tidy" ["-i"] content
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
toy_ = div_ [style [ "margin" ||= Px 10
                   , "border" ||= Px 2
                   , "border" ||= [Ident "dashed", Ident "black"]
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
    box :: String -> [Decl]
    box bg =
      [ "border" ||= [Ident "dashed", Ident "black", Ident "2px"]
      , "padding" ||= Px 50
      , "margin" ||= Px 60
      , "width" ||= Px 70
      , "height" ||= Px 70
      , "backgroundColor" ||= Ident bg
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
        , mkUserMap [ User sampleMetaID "user" "<email@bla.com>"
                    , User sampleMetaID "üsör" "<grr@bla.com>" & userID .~ ID 8479]
        ), [])
    , ("menu/mainMenuGroup_", mainMenuGroup_ . (,) MainMenuGroupProcesses $
        ( Just sampleGroup1
        , Map.singleton (ID 0) sampleVDoc
        , mkUserMap [ User sampleMetaID "user" "<email@bla.com>"
                    , User sampleMetaID "üsör" "<grr@bla.com>" & userID .~ ID 8479]
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
    , ("toolbar/discussionToolbar_.flat", discussionToolbar_ $ DiscussionToolbarProps (ID 0) True False mempty, [])
    , ("toolbar/discussionToolbar_.tree", discussionToolbar_ $ DiscussionToolbarProps (ID 0) False False mempty, [])

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
    describe "validate-before" $ validateStyleGuide >> checkWorkingCopy
    describe "generate"        $ generateIndexHtml >> generateStyleGuide `mapM_` viewsSources
    describe "validate-after"  $ validateStyleGuide >> checkWorkingCopy
