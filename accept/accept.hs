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

{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-unused-imports #-}

-- | webdriver acceptance tests for refine.
module Main where

import           Control.Exception.Base (bracket_)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromMaybe)
import           Data.String.Conversions
import qualified Data.Text as ST
import           System.Environment (lookupEnv)
import           System.Process (system)
import           System.Exit
import qualified Test.Hspec as Hspec
import           Test.Hspec.WebDriver hiding (runWD)
import           Test.WebDriver hiding (runWD)
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

webdriver :: WDConfig -> String -> Spec
webdriver cnf appurl = sessionWith cnf "@webdriver" . using allBrowsers $ do
    it ("opens page " <> appurl) . runWD $ do
        openPage appurl
        t <- getTitle
        t `shouldBe` "liqula:Product1"

    it "opens the page and renders the landing page" . runWD $ do
        elems :: [Element] <- findElems (ByCSS "h1")
        txts  :: [ST]      <- getText `mapM` elems
        mconcat txts `shouldSatisfy` ("Load a VDoc" `ST.isInfixOf`)


-- * hspec-webdriver amendments

-- | hspec-webdriver does not call 'closeOnException', which causes the selenium node to enter an
-- inconsistent state.  this is the variant that works.  (note that 'finallyClose' cannot be called
-- in the same way here, since it would terminate the session after every 'it', but the session
-- should span the 'sessionWith' call.
runWD :: WD () -> WdExample ()
runWD = WdExample () (WdOptions { skipRemainingTestsAfterFailure = True }) . closeOnException

shouldSatisfy :: (Hspec.HasCallStack, Show a) => a -> (a -> Bool) -> WD ()
v `shouldSatisfy` p = liftIO $ v `Hspec.shouldSatisfy` p
