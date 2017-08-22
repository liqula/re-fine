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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-redundant-constraints -Wno-orphans -Wno-incomplete-patterns -Wno-unused-imports #-}

-- | webdriver acceptance tests for refine.
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Exception.Base (bracket_)
import           Control.Monad (when, void, forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson
import           Data.Maybe (fromMaybe)
import           Data.String.Conversions
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import           GHC.Stack (HasCallStack)
import           System.Environment (lookupEnv)
import           System.Exit
import           System.Process (system)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Test.Hspec as Hspec
import           Test.Hspec.WebDriver hiding (runWD)
import           Test.WebDriver hiding (runWD)
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.Session
import           Text.Read (readMaybe)


main :: IO ()
main = do
  ["SELENIUM_HUB_PORT", "REFINE_APP_PORT", "REFINE_RUN_APP"] `forM_` \v -> do
    putStrLn . ((v <> "=") <>) . fromMaybe "" =<< lookupEnv v

  Just (seleniumPort :: Int) <- (>>= readMaybe) <$> lookupEnv "SELENIUM_HUB_PORT"
  Just (appPort :: Int)      <- (>>= readMaybe) <$> lookupEnv "REFINE_APP_PORT"
  pleaseRunApp :: Bool       <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "REFINE_RUN_APP"

  let appUrl = "http://localhost:" <> show appPort <> "/"
      runApp = if pleaseRunApp
        then bracket_ startApp stopApp
        else id

  runApp . hspec $ webdriver (defaultConfig { wdHost = "localhost", wdPort = seleniumPort}) appUrl

startApp :: IO ()
startApp = do
  ExitSuccess <- system "stack exec -- selenium start || ( stack exec -- selenium status ; stack exec -- selenium stop ; false )"
    -- (we have to shut down selenium here; bracket_ does not call the closing backet if the opening bracket already crashes.)
  pure ()

stopApp :: IO ()
stopApp = do
  ExitSuccess <- system "stack exec -- selenium stop"
  pure ()

allBrowsers :: [(Capabilities, String)]
allBrowsers = fst <$> filter snd
  [ ((defaultCaps, "geckodriver"), True)
  , ((chromeCaps, "chromedriver"), False)
  ]


userName :: ST
userName = "me2"

userEmail :: ST
userEmail = "me2"

userPassword :: ST
userPassword = "me"

textA, textB :: ST
textA = "___textA___"
textB = "___textB___"

block1Text :: ST
block1Text = "YT4rxgAPR4HJE"

block2Text :: ST
block2Text = "CtbPG6cX/ZVng"

block3Text :: ST
block3Text = "yfgHD6jT0s7VA"


webdriver :: WDConfig -> String -> Spec
webdriver cnf appurl = sessionWith cnf "@webdriver" . using allBrowsers $ do
  it ("opens page " <> appurl) . runWD $ do
    openPage appurl
    t <- getTitle
    t `shouldBe` "liqula:Product1"

  it "renders the landing page" . runWD $ do
    onEl [ByCSS "h1"] $ \el -> do
      txt <- getText el
      txt `shouldSatisfy` ("Login" `ST.isInfixOf`)

  it "login, if unsuccessful create user, and login again" . runWD $ do
    let doLogin = do
          onEl [ByXPath "//input[@id='login-username']"] $ sendKeys userName
          onEl [ByXPath "//input[@id='login-password']"] $ sendKeys userPassword
          onEl [ByXPath "//span[text()='submit']"] click

        doRegister = do
          onEl [ByXPath "//input[@id='registration-username']"]  $ sendKeys userName
          onEl [ByXPath "//input[@id='registration-email1']"]    $ sendKeys userEmail
          onEl [ByXPath "//input[@id='registration-email2']"]    $ sendKeys userEmail
          onEl [ByXPath "//input[@id='registration-password1']"] $ sendKeys userPassword
          onEl [ByXPath "//input[@id='registration-agree']"]       click
          onEl [ByXPath "//span[text()='submit']"]                 click

    onEl [xpathButton "Login"] click
    doLogin
    onEls [ByXPath "//span[text()='register']"] $ \case
      [] -> pure ()  -- ok, login seems to have worked.
      [r] -> do  -- we still appear to be on the login page, so let's register and try again.
        click r
        doRegister
        doLogin

  it "create new process" . runWD $ do
    let titleText = "iDWD16VgtbLgI"
    onEl [ByCSS "#group-list-item-1"] click
    onEl [ByCSS ".icon-Process_add_dark"] click
    onEls [ByXPath "//*[@id='o-vdoc-overlay-content__textarea-annotation']"] $ sendKeys titleText . head
    onEl [ByCSS ".icon-Save_dark"] click
    onEls [ByCSS "h1"] $ \els -> do
      txt <- mconcat <$> (getText `mapM` els)
      txt `shouldSatisfy` (titleText `ST.isInfixOf`)

  it "create, save initial content" . runWD $ do
    yScrollTo 0  -- some processes open scrolled to the bottom.  (this should have a ticket.)
    onEl [ByCSS ".public-DraftEditor-content", ByXPath "//div[@data-contents='true']/child::node()"] $ \block -> do
      addTextToDraft block [block1Text, block2Text, block3Text]
        -- FIXME: this is added as one block containing newlines.  not good!
    onEls [ByCSS ".public-DraftEditor-content", ByXPath "//div[@data-contents='true']/child::node()"] $ \blocks -> do
      click $ last blocks
    onEl [xpathButton "Edit_toolbar_h1"] click
    onEl [xpathButton "Edit_toolbar_h1"] click  -- #401?
    onEl [ByCSS ".icon-Save_dark"] click

  it "scroll to first heading" . runWD $ do
    onEl [ByCSS ".icon-Index_desktop_dark"] click
    let editor = findElem $ ByCSS ".public-DraftEditor-content"
    assertVerticalPos (>) editor $ onEl [ByCSS ".__index-heading-0"] click
    yScrollTo 0

  it "open edit mode" . runWD $ do
    onEl [ByCSS ".icon-New_Edit_dark"] click
    onEl [ByCSS ".public-DraftEditor-content"] $ \el -> do
      editableText <- getText el
      block3Text `shouldSatisfy` (`ST.isInfixOf` editableText)

  it "adding two tokens in the first and last draft.js block, resp." . runWD $ do
    onEls [ByCSS ".public-DraftEditor-content", ByXPath "//div[@data-contents='true']/child::node()"] $ \blocks -> do
      addTextToDraft (head blocks) [textA]
    onEls [ByCSS ".public-DraftEditor-content", ByXPath "//div[@data-contents='true']/child::node()"] $ \blocks -> do
      addTextToDraft (last blocks) [textB]

    onEls [ByCSS ".public-DraftEditor-content", ByXPath "//div[@data-contents='true']/child::node()"] $ \blocks -> do
      h <- getText $ head blocks
      t <- getText $ last blocks
      textA `shouldSatisfy` (`ST.isInfixOf` h)
      textB `shouldSatisfy` (`ST.isInfixOf` t)

  it "save edit" . runWD $ do
    yScrollTo 0  -- scrolling is a workaround for #376.
    onEl  [ByCSS ".icon-Save_dark"] click
    onEls [ByCSS ".skylight-dialog", ByCSS ".icon-New_Edit_dark"] (click . head)
    onEl  [ByCSS ".skylight-dialog", ByXPath "//textarea"]        (sendKeys "bloorp")
    onEl  [ByCSS ".skylight-dialog", ByCSS ".icon-Save_bright"]   click

  it "find two bubbles" . runWD $ do
    onEls [ByCSS ".o-snippet__content"] $ \bubbles ->
      length bubbles `shouldBe` 1  -- TODO #403

  it "find both tokens in the html source of the diff view" . runWD $ do
    onEl [ByCSS ".o-snippet__content"] click
    stubborn $ do
      everything <- getSource
      textA `shouldSatisfy` (`ST.isInfixOf` everything)
      textB `shouldSatisfy` (`ST.isInfixOf` everything)


-- * xpath shortcuts

-- | Find any of _RO, _dark, _bright
xpathButton :: ST -> Selector
xpathButton button = ByXPath $ "//*[@id='refine']//div[@class[contains(., 'icon-" <> button <> "')]]"


-- * hspec-webdriver amendments

-- | hspec-webdriver does not call 'closeOnException', which causes the selenium node to enter an
-- inconsistent state.  this is the variant that works.  (note that 'finallyClose' cannot be called
-- in the same way here, since it would terminate the session after every 'it', but the session
-- should span the 'sessionWith' call.
--
-- FIXME: closeOnException is nice because it breeds slightly fewer zombie selenium and gecko jobs,
-- but it's not nice because it will break hspec-webdriver sessions and die one or two test cases
-- before the end with "CLIENT_STOPPED_SESSION".  so for now we don't use it and have to keep
-- killing the zombies by hand.
runWD :: WD () -> WdExample ()
runWD = WdExample () WdOptions { skipRemainingTestsAfterFailure = True } -- . closeOnException

shouldSatisfy :: (Hspec.HasCallStack, Show a) => a -> (a -> Bool) -> WD ()
v `shouldSatisfy` p = liftIO $ v `Hspec.shouldSatisfy` p

dump :: [Element] -> WD ()
dump els = do
  path <- liftIO $ ("/tmp/refine-accept-" <>) . show <$> modifyMVar dumpCount (\i -> pure (i+1, i))
  saveScreenshot (path <> ".png")
  liftIO . ST.writeFile (path <> ".html") =<< getSource
  liftIO . ST.writeFile (path <> ".elems") . cs . show =<< (getText `mapM` els)

{-# NOINLINE dumpCount #-}
dumpCount :: MVar Int
dumpCount = unsafePerformIO $ newMVar 1000

delay :: Double -> MonadIO m => m ()
delay = liftIO . threadDelay . round . (*1000000)

-- | http://hackage.haskell.org/package/webdriver-0.8.5/docs/Test-WebDriver-Commands-Wait.html
--
-- (i sure hope we don't lose session state during exceptions handling, and if we do that that's
-- not a problem...)
onEls' :: forall a els. HasCallStack => ([Element] -> Either String els) -> [Selector] -> (els -> WD a) -> WD a
onEls' sanitiseElems (sel:sels) action = stubborn $ do
  findElems sel >>= drill sels >>= \els -> do
    expectNotStale `mapM_` els
    case sanitiseElems els of
      Right els' -> action els'
      Left msg   -> liftIO . throwIO . ErrorCall $ "onEls' element sanitation: " <> msg
  where
    drill []             els = pure els
    drill (sel' : sels') els = ((`findElemsFrom` sel') `mapM` els) >>= drill sels' . mconcat

stubborn :: WD a -> WD a
stubborn = waitUntil' pollFreqMicroSeconds pollTimeoutSeconds
  where
    pollFreqMicroSeconds = 1401000
    pollTimeoutSeconds = 13

onEls :: forall a. HasCallStack => [Selector] -> ([Element] -> WD a) -> WD a
onEls = onEls' pure

onEl :: forall a. HasCallStack => [Selector] -> (Element -> WD a) -> WD a
onEl sels = onEls' mhead sels
  where
    mhead [el] = Right el
    mhead bad  = Left $ "not a one-elem list: " <> show (sels, bad)


-- * js

-- | FIXME: at least some of the delays here are necessary, or the clicks will miss the corrsponding
-- blocks.
--
-- | 'sendKeys', 'sendRawKeys' didn't work.  curiously, running firefox manually on the app as run
-- here and attempting to entering text there didn't work either.  perhaps draft as we use it just
-- doesn't work on firefox yet?
addTextToDraft :: Element -> [ST] -> WD ()
addTextToDraft el txt = do
  delay 1.2
  click el
  delay 1.2
  click el
  delay 1.2
  True <- executeJS [] . ST.unlines $
    findReactByDOMNode <>
    draftInsertText txt <>
    ["return draftInsertText();"]
  pure ()

-- | https://stackoverflow.com/questions/24462679/react-get-react-component-from-a-child-dom-element/39165212#39165212
findReactByDOMNode :: [ST]
findReactByDOMNode =
  [ "var findReactByDOMNode = function(dom) {"
  , "  for (var key in dom) {"
  , "    if (key.startsWith('__reactInternalInstance$')) {"
  , "      var compInternals = dom[key]._currentElement;"
  , "      var compWrapper = compInternals._owner;"
  , "      var comp = compWrapper._instance;"
  , "      return comp;"
  , "    }"
  , "  }"
  , "  return null;"
  , "};"
  ]

-- | https://github.com/facebook/draft-js/issues/325#issuecomment-296274109
draftInsertText :: [ST] -> [ST]
draftInsertText = (header <>) . (<> footer) . injection
  where
    header =
      [ "var draftInsertText = function(newText) {"
      , "  var editor = findReactByDOMNode(document.getElementsByClassName('public-DraftEditor-content')[0]);"
        <> log_ "editor"
      , "  var editorState = editor.props.editorState;"
        <> log_ "editorState"
      , "  var contentState = '';"
      ]

    injection = go
      where
        go [] = []
        go [txt] = writeToBlock txt
        go (txt:(txts@(_:_))) = writeToBlock txt <> splitBlock <> go txts

        writeToBlock txt =
          [ "  contentState = Draft.Modifier.insertText(editorState.getCurrentContent(),"
          , "                                           editorState.getSelection(),"
          , "                                           " <> cs (show txt) <> ","
          , "                                           editorState.getCurrentInlineStyle(), null);"
            <> log_ "contentState"
          , "  editorState = Draft.EditorState.push(editorState, contentState, 'insert-characters');"
            <> log_ "editor"
          ]

        splitBlock =
          [ "  contentState = Draft.Modifier.splitBlock(editorState.getCurrentContent(),"
          , "                                           editorState.getSelection());"
            <> log_ "contentState"
          , "  editorState = Draft.EditorState.push(editorState, contentState, 'split-block');"
            <> log_ "editor"
          ]

    footer =
      [ "  editorState = Draft.EditorState.forceSelection(editorState, contentState.getSelectionAfter());"
        <> log_ "editor"
        <> log_ "'triggering onChange event...'"
      , "  editor.props.onChange(editorState);"
        <> log_ "'done'"
      , "  return true;"
      , "};"
      ]

    log_ s = mconcat ["  console.log(" <> s <> ");" | verbose]
    verbose = False

-- | 'moveToFrom' is not supported by geckodriver.
yScrollTo :: Int -> WD ()
yScrollTo y = do
  (_ :: Value) <- executeJS [JSArg y] "scrollBy(arguments[0], -pageYOffset);"
  pure ()

-- | Run a 'WD' action, and take the element position before and after.  If @p before after@ fails
-- to hold, throw an error.
assertVerticalPos :: (Int -> Int -> Bool) -> WD Element -> WD a -> WD a
assertVerticalPos p el act = do
  (w, h) <- elemPos =<< el
  a <- act
  (w', h') <- elemPos =<< el
  when (w /= w' || not (p h h')) . liftIO . throwIO . ErrorCall $
    "no scroll happend: " <> show (w, h) <> " " <> show (w', h')
  pure a

{- manual script

-- run server with god mode, database wiped

>> groups tab
>> default group
>> create new process
>> set title to "doc1"
>> save
>> enter "first commit"
>> save

>> select "first"
>> quickcreate note
>> select note
>> enter "a"
>> submit
>> bubble "a"
-------------- vote does not work
>> close

>> select "commit"
>> quickcreate note
>> select discussion
>> enter "b"
>> submit
>> click on bubble stack
>> bubble "b"
--------------- cannot edit
---------- close does not work

>> select "commit"
>> quickcreate edit
>> register tab
>> enter "a" "a" "a" "a"
>> check the checkbox
>> submit
>> enter "a" "a"
>> submit
>> delete "commit"
>> save
>> select meaning
>> enter "c"
>> save
>> bubble "c"
>> vote up



-}
