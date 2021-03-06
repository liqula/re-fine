#!/usr/bin/env stack
{- stack --resolver lts-8.11 --install-ghc runghc
    --package attoparsec
    --package executable-path
    --package hspec
    --package string-conversions
    --package system-filepath
    --package temporary
    --package turtle

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-imports #-}

import           Control.Applicative
import           Control.Exception (assert)
import qualified Control.Foldl as Fold
import           Control.Monad
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as A (lookAhead)
import           Data.Char
import           Data.List (foldl', sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String.Conversions
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import           Filesystem.Path.CurrentOS hiding (empty, null)
import qualified GHC.IO
import           Prelude hiding (FilePath)
import           System.Directory
import qualified System.FilePath
import           System.Environment
import           System.Environment.Executable
import           System.Exit
import           System.IO.Temp
import           Test.Hspec
import           Text.Read (readMaybe)
import           Turtle hiding (f, o, x, s, d, e)


verbose :: Bool
verbose = False

-- | run this on a clean working copy (no un-commited changes; untracked and ignored files are
-- allowed).  it reports *and fixes* style violations and lets you examine and commit the changes
-- when it is done.
main :: IO ()
main = sh $ do
  liftIO . hspec $ do
    testWrapJsFFI
    testFixWhitespace
  assertWorkingCopyClean
  setProperCurrentDirectory
  () <- fixWhitespace =<< getSourceFiles ["pkg/frontend/jsbits"] ["js"]
  () <- fixWhitespace =<< getSourceFiles
          [ "pkg" </> pkg </> topic | pkg <- ["prelude", "common", "backend", "frontend"]
                                    , topic <- ["src", "test"] ]
          ["hs"]
  () <- wrapJsFFI =<< getSourceFiles
          [ "pkg/frontend" </> topic | topic <- ["src", "test", "jsbits"] ]
          ["hs"]
  failOnChangedFiles

getSourceFiles :: MonadIO m => [FilePath] -> [ST] -> m [FilePath]
getSourceFiles roots exts = filterExts exts <$> getAllFiles roots

failOnChangedFiles :: Shell ()
failOnChangedFiles = do
  gs <- dirtyFiles
  if null gs
    then do
      echo ".../scripts/style-check.hs: all clear."
    else do
      echo ".../scripts/style-check.hs has made changes to the code, which means this version contains style rule violations."
      echo "please re-run the script locally and commit the changes, or open an issue complaining about the style rules."
      echo "NOTE: always run this script on a clean working copy!"
      echo `mapM_` (unsafeTextToLine . cs . show <$> gs)
      exit $ ExitFailure 1


-- * rules

-- | both horizontally and vertically
fixWhitespace :: [FilePath] -> Shell ()
fixWhitespace = mapM_ (transformFile fixWhitespaceTrans)

fixWhitespaceTrans :: [ST] -> [ST]
fixWhitespaceTrans = fmap (killTabs . ST.stripEnd) . reverse . dropEmptyLines . reverse
  where
      dropEmptyLines ("" : xs@("" : _)) = dropEmptyLines xs
      dropEmptyLines xs                 = xs

      killTabs s = case ST.breakOn "\t" s of
                     (_, "") -> s
                     (x, y) -> x <> ST.replicate 8 " " <> killTabs (ST.tail y)

testFixWhitespace :: Spec
testFixWhitespace = it "works" $ do
  fixWhitespaceTrans ["\t\t\t"] `shouldBe` [""]
  fixWhitespaceTrans [" \t \t \t "] `shouldBe` [""]
  fixWhitespaceTrans [".\t.\t.\t."] `shouldBe` [".        .        .        ."]
  fixWhitespaceTrans [". \t \t \t ."] `shouldBe` ["." <> ST.replicate 28 " " <> "."]
  fixWhitespaceTrans ["\t.\t.\t"] `shouldBe` ["        .        ."]


wrapJsFFI :: [FilePath] -> Shell ()
wrapJsFFI = mapM_ (transformFile (ST.lines . renderFFIBlocks . consolidateFFIBlocks . parseFFIBlocks . ST.unlines))

consolidateFFIBlocks :: [FFIBlock] -> [FFIBlock]
consolidateFFIBlocks = f []
  where
    f acc (FFIBlock ffis : bs)              = f (ffis : acc) bs
    f acc [NonFFIBlock txt, b@(FFIBlock _)] = NonFFIBlock (ST.stripEnd txt <> "\n") : f acc [b]
    f acc (b@(NonFFIBlock _) : bs)          = b : f acc bs
    f [] []                                 = []
    f acc []                                = [FFIBlock . mconcat . reverse $ acc]

data FFIBlock = FFIBlock [(ST, ST)] | NonFFIBlock ST
  deriving (Eq, Show)

testWrapJsFFI :: Spec
testWrapJsFFI = describe "wrapJsFFI" $ do
  it "works" $ do
    A.parseOnly pDeclHead "foreign import javascript safe\n"
      `shouldBe` Right "foreign import javascript safe\n"
    A.parseOnly pDeclHead "foreign import javascript unsafe\n"
      `shouldBe` Right "foreign import javascript unsafe\n"
    A.parseOnly pFun "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String\n\n"
      `shouldBe` Right ("js_fun :: JSVal -> String", "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String")

    A.parseOnly (pMany1Till (A.char 'a') (A.char '*')) "*"
      `shouldBe` Left "Failed reading: pMany1Till: empty output list"
    A.parseOnly (pMany1Till (A.char 'a') (A.char '*') >> A.char '*' >> A.char '!') "aa*!"
      `shouldBe` Left "'*': Failed reading: satisfy"
    A.parseOnly (pMany1Till (A.char 'a') (A.char '*') >> A.char '!') "aa*!"
      `shouldBe` Right '!'

    A.parseOnly (pOptionalBracket (A.char '[') (A.char ']') (A.char '*')) "[*]"
      `shouldBe` Right '*'
    A.parseOnly (pOptionalBracket (A.char '[') (A.char ']') (A.char '*')) "*"
      `shouldBe` Right '*'


    A.parseOnly pMultiLineString "\"woeifj\""
      `shouldBe` Right "\"woeifj\""

    A.parseOnly pMultiLineString "\"woe\nifj\""
      `shouldBe` Right "\"woe\nifj\""

    A.parseOnly pMultiLineString "\"woe \\\nifj\""
      `shouldBe` Right "\"woe \\\nifj\""

    A.parseOnly pMultiLineString "\"woe \\\n  \\\"ifj\""
      `shouldBe` Right "\"woe \\\n  \\\"ifj\""

    A.parseOnly pMultiLineString "\"woe \\\n\\\"ifj\""
      `shouldBe` Right "\"woe \\\n\\\"ifj\""


    A.parseOnly pFFIBlock "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String\n\n"
      `shouldBe` Right (FFIBlock [("js_fun :: JSVal -> String", "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String")])

    A.parseOnly pNonFFIBlock ""
      `shouldBe` Left "Failed reading: pMany1Till: empty output list"

    parseFFIBlocks "\n" `shouldBe` [NonFFIBlock "\n"]

    A.parseOnly pCommentLine "-- ..."        `shouldBe` Right "-- ..."
    A.parseOnly pCommentLine "--"            `shouldBe` Right "--"
    A.parseOnly pCommentLine "-- aufn awfme" `shouldBe` Right "-- aufn awfme"

    A.parseOnly pDeclHead (ST.unlines
                 [ "-- ..."
                 , "--"
                 , "-- aufn awfme"
                 , "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String"
                 ])
      `shouldBe` Right "-- ...\n--\n-- aufn awfme\nforeign import javascript safe\n"

    let i1 :: ST
        i1 = ST.unlines
          [ "module Some where"
          , ""
          , "import Some.Other"
          , ""
          , "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String"
          , "foreign import javascript safe\n  \"$1\"\n  js_foon :: JSVal -> String"
          , ""
          , "foreign import javascript safe\n  \"$1\"\n  js_fawn :: JSVal -> String"
          , ""
          ]

        r1 = [ NonFFIBlock "module Some where\n\nimport Some.Other\n\n"
             , FFIBlock
               [ ("js_fun :: JSVal -> String", "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String")
               , ("js_foon :: JSVal -> String", "foreign import javascript safe\n  \"$1\"\n  js_foon :: JSVal -> String")
               , ("js_fawn :: JSVal -> String", "foreign import javascript safe\n  \"$1\"\n  js_fawn :: JSVal -> String")
               ]
             ]

        i2 :: [ST] -> [ST] -> ST
        i2 noise1 noise2 = ST.unlines $
          [ "module Some where"
          , ""
          , "import Some.Other"
          , ""
          , "#ifdef __GHCJS__"
          , ""
          , "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String"
          , "foreign import javascript safe\n  \"$1\"\n  js_foon :: JSVal -> String"
          , ""
          , "foreign import javascript safe\n  \"$1\"\n  js_fawn :: JSVal -> String"
          ] <> noise1 <>
          [ "#else"
          ] <> noise2 <>
          [ "#endif"
          , ""
          ]

        i3 :: ST
        i3 = ST.unlines
          [ "module Some where"
          , ""
          , "import Some.Other"
          , ""
          , "#ifdef __GHCJS__"
          , ""
          , "-- ..."
          , "-- aufn awfme"
          , "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String"
          , "-- ***"
          , "foreign import javascript safe\n  \"$1\"\n  js_foon :: JSVal -> String"
          , ""
          , "foreign import javascript safe\n  \"$1\"\n  js_fawn :: JSVal -> String"
          , "#else"
          , "#endif"
          , ""
          ]

        r2 = [ NonFFIBlock "module Some where\n\nimport Some.Other\n\n"
             , FFIBlock
               [ ("js_fun :: JSVal -> String", ST.intercalate "\n"
                   [ "-- ..."
                   , "-- aufn awfme"
                   , "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String"
                   ])
               , ("js_foon :: JSVal -> String", ST.intercalate "\n"
                   [ "-- ***"
                   , "foreign import javascript safe\n  \"$1\"\n  js_foon :: JSVal -> String"
                   ])
               , ("js_fawn :: JSVal -> String", "foreign import javascript safe\n  \"$1\"\n  js_fawn :: JSVal -> String")
               ]
            ]

        i4 :: ST
        i4 = ST.unlines
          [ "module Some where"
          , ""
          , "import Some.Other"
          , ""
          , "-- ..."
          , "--"
          , "-- aufn awfme"
          , "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String"
          , ""
          , "someNormalFun :: Bool"
          , "someNormalFun = _"
          , "-- ***"
          , "foreign import javascript safe\n  \"$1\"\n  js_foon :: JSVal -> String"
          , ""
          , "foreign import javascript safe\n  \"$1\"\n  (===) :: JSVal -> String"
          , ""
          ]

        r3 = [ NonFFIBlock "module Some where\n\nimport Some.Other\n\n"
             , FFIBlock
                 [ ("js_fun :: JSVal -> String", ST.intercalate "\n"
                     [ "-- ..."
                     , "--"
                     , "-- aufn awfme"
                     , "foreign import javascript safe\n  \"$1\"\n  js_fun :: JSVal -> String"
                     ])
                 ]
             , NonFFIBlock "someNormalFun :: Bool\nsomeNormalFun = _\n"
             , FFIBlock
                 [ ("js_foon :: JSVal -> String", ST.intercalate "\n"
                     [ "-- ***"
                     , "foreign import javascript safe\n  \"$1\"\n  js_foon :: JSVal -> String"
                     ])
                 , ("(===) :: JSVal -> String", "foreign import javascript safe\n  \"$1\"\n  (===) :: JSVal -> String")
                 ]
             ]
     in do
       parseFFIBlocks i1 `shouldBe` r1

       parseFFIBlocks (i2 []     [])     `shouldBe` r1
       parseFFIBlocks (i2 ["  "] [])     `shouldBe` r1
       parseFFIBlocks (i2 []     [".."]) `shouldBe` r1
       parseFFIBlocks (i2 ["  "] [".."]) `shouldBe` r1

       parseFFIBlocks i3 `shouldBe` r2
       parseFFIBlocks i4 `shouldBe` r3


parseFFIBlocks :: ST -> [FFIBlock]
parseFFIBlocks = either (error . show) id . A.parseOnly (A.many1 (pFFIBlock <|> pNonFFIBlock))

pFFIBlock :: A.Parser FFIBlock
pFFIBlock = FFIBlock <$> pOptionalBracket pIf pRest (A.many1 pFun)

pNonFFIBlock :: A.Parser FFIBlock
pNonFFIBlock = NonFFIBlock . cs <$> pMany1Till A.anyChar (A.lookAhead $ void (pIfDefGHCJS >> pSpace >> pDeclHead) <|> void pDeclHead <|> void A.endOfInput)


pIfDefGHCJS :: A.Parser ST
pIfDefGHCJS = A.string "#ifdef __GHCJS__\n"

pDeclHead :: A.Parser ST
pDeclHead = do
  c :: [ST] <- A.option [] (A.many1 pCommentLine)
  h :: ST <- A.string "foreign import javascript unsafe\n" <|> A.string "foreign import javascript safe\n"
  pure . ST.intercalate "\n" $ c <> [h]


pFun :: A.Parser (ST, ST)
pFun = (A.<?> "pFun") $ do
  decl <- pDeclHead
  str  <- (<>) <$> pSpace <*> pMultiLineString
  sp1  <- A.char '\n' >> pSpace
  fun  <- pNotSpace
  typ  <- pLine True
  _ <- A.option "" pSpace
  pure (fun <> typ, decl <> str <> "\n" <> sp1 <> fun <> ST.stripEnd typ)

pIf :: A.Parser ()
pIf = (A.<?> "pIf") $ do
  _ <- pIfDefGHCJS
  _ <- pSpace
  pure ()

pRest :: A.Parser ()
pRest = (A.<?> "pRest") $ do
  _ <- A.string "#else\n"
  _ <- A.manyTill (pLine True) (A.string "#endif")
  _ <- pSpace
  pure ()


pCommentLine :: A.Parser ST
pCommentLine = do
  x <- A.string "--"
  y <- pLine True <|> ((void (A.char '\n') <|> A.endOfInput) >> pure "")
  pure $ x <> y


pSpace :: A.Parser ST
pSpace = cs <$> A.many1 (A.satisfy isSpace)

pNotSpace :: A.Parser ST
pNotSpace = cs <$> A.many1 (A.satisfy (not . isSpace))

pLine :: Bool -> A.Parser ST
pLine consumeNewline = (cs <$> A.many1 (A.satisfy (/= '\n')) <* (newl <|> (A.endOfInput >> pure '\n'))) <|> (newl >> pure "")
  where newl = (if consumeNewline then id else A.lookAhead) $ A.char '\n'

pMultiLineString :: A.Parser ST
pMultiLineString = (A.<?> "pMultiLineString") $ do
  o <- A.string "\""
  s <- (cs . mconcat <$> A.many1 (someChars <|> someEscapedChar)) <|> pure ""
  c <- A.string "\""
  pure $ o <> s <> c
  where
    someChars = do
      cs <$> A.many1 (A.satisfy (not . (`elem` ("\\\"" :: String))))
    someEscapedChar = do
      c <- A.string "\\"
      d <- A.anyChar
      pure $ c <> cs [d]

pOptionalBracket :: A.Parser open -> A.Parser close -> A.Parser body -> A.Parser body
pOptionalBracket open close body = do
  o <- A.option False (const True <$> open)
  b <- body
  () <- when o (void close)
  pure b

-- | 'A.many1Till' seems to be missing.
pMany1Till :: A.Parser elem -> A.Parser terminator -> A.Parser [elem]
pMany1Till el terminator = A.manyTill el terminator
  >>= \case
    []  -> fail "pMany1Till: empty output list"
    txt -> pure txt


renderFFIBlocks :: [FFIBlock] -> ST
renderFFIBlocks = mconcat . fmap rBlock
  where
    rBlock (FFIBlock fs) = "\n#ifdef __GHCJS__\n\n"
                        <> ST.intercalate "\n" (rFunGHCJS <$> fs)
                        <> "\n#else\n\n"
                        <> ST.intercalate "\n" (rFunGHC <$> fs)
                        <> "\n#endif\n"
    rBlock (NonFFIBlock txt) = txt

    rFunGHCJS :: (ST, ST) -> ST
    rFunGHCJS (_, ffi) = ffi <> "\n"

    rFunGHC :: (ST, ST) -> ST
    rFunGHC (fname, _) = "{-# ANN " <> fname' <> " (\"HLint: ignore Use camelCase\" :: String) #-}\n"
                      <> fname <> "\n"
                      <> fname' <> " = error \"javascript FFI not available in GHC\"\n"
      where fname' = ST.takeWhile (not . isSpace) fname


-- * git

dirtyFiles :: Shell [GitStatus]
dirtyFiles = do
  let interesting (GitStatus _ Untracked _) = False
      interesting (GitStatus _ Ignored _)   = False
      interesting _                         = True
  mconcat <$> (filter interesting <$> gitStatus) `fold` Fold.list

assertWorkingCopyClean :: Shell ()
assertWorkingCopyClean = do
  gs <- dirtyFiles
  unless (null gs) $ do
    echo "this script can only be run on a clean working copy!"
    exit $ ExitFailure 1

gitStatus :: Shell [GitStatus]
gitStatus = fmap parse . ST.lines . lineToText <$> inshell "git status --porcelain ." Turtle.empty
  where
    parse :: ST -> GitStatus
    parse line = GitStatus (parseCode ix) (parseCode wt) (cs file)
      where
        ix   = ST.take 1               line
        wt   = ST.take 1 . ST.drop 1 $ line
        file =             ST.drop 3   line

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


-- * helpers

withClosedSystemTempFile :: String -> (FilePath -> IO a) -> IO a
withClosedSystemTempFile template action = withSystemTempDirectory template $ action . cs . (<> "/file")

-- | (not directories)
getAllFiles :: MonadIO m => [FilePath] -> m [FilePath]
getAllFiles roots = do
  paths <- fmap mconcat $ (\r -> lstree r `fold` Fold.list) `mapM` roots
  filterM (liftIO . doesFileExist . cs) paths

filterExts :: [ST] -> [FilePath] -> [FilePath]
filterExts exts = filter ((`elem` (Just <$> exts)) . extension)

transformFile :: ([ST] -> [ST]) -> FilePath -> Shell ()
transformFile trans file = do
  debugLog (cs file <> "..." :: ST)
  contents :: [ST] <- lineToText <$$> (input file `fold` Fold.list)
  let contents' :: [ST] = trans contents
  when (contents' /= contents) . liftIO $ do
    withClosedSystemTempFile "refine.tmp" $ \file' -> do
      ST.writeFile  (cs file') $ ST.unlines contents'
      mv file' file


-- * amendments to turtle

instance ConvertibleStrings ST Filesystem.Path.CurrentOS.FilePath where
  convertString = Filesystem.Path.CurrentOS.fromText

instance ConvertibleStrings Filesystem.Path.CurrentOS.FilePath ST where
  convertString = either (error . show) id . Filesystem.Path.CurrentOS.toText

instance ConvertibleStrings String Filesystem.Path.CurrentOS.FilePath where
  convertString = cs @ST . cs

instance ConvertibleStrings Filesystem.Path.CurrentOS.FilePath String where
  convertString = cs @ST . cs


echoShow :: (MonadIO m, Show a) => a -> m ()
echoShow = echoCS . show

echoCS :: (MonadIO m, ConvertibleStrings s ST) => s -> m ()
echoCS = mapM_ echo . textToLines . cs


-- * should go to a separate packge

(<$$>) :: (a -> b) -> Shell [a] -> Shell [b]
(<$$>) = fmap . fmap

setProperCurrentDirectory :: MonadIO m => m ()
setProperCurrentDirectory = liftIO $ do
  progName <- getProgName

  let setdir ((</> "..") . cs . System.FilePath.takeDirectory -> workingDir) = do
        putStrLn $ progName <> ": setting working directory to " <> show workingDir
        setCurrentDirectory (cs workingDir)

      setdiri = do
        wd <- getCurrentDirectory
        when (System.FilePath.takeFileName wd == "scripts") $ setCurrentDirectory ".."
        putStr "running interactively; wd: "
        putStrLn =<< getCurrentDirectory

  getScriptPath >>= \case
    Executable wd -> setdir wd
    RunGHC wd     -> setdir wd
    Interactive   -> setdiri

debugLog :: MonadIO m => ConvertibleStrings s ST => s -> m ()
debugLog = when verbose . liftIO . sh . mapM_ echo . textToLines . cs
