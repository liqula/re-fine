#!/usr/bin/env stack
{- stack --resolver lts-8.11 --install-ghc runghc
    --package css-text
    --package executable-path
    --package mtl
    --package string-conversions
    --package system-filepath
    --package tagsoup
    --package template
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

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import           Control.Applicative
import           Control.Exception (assert)
import qualified Control.Foldl as Fold
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.List (elemIndex)
import           Data.String.Conversions
import qualified Data.Text as ST
import qualified Data.Text.IO as ST
import qualified Data.Text.Template as TMPL
import           Filesystem.Path.CurrentOS hiding (empty, null)
import           Prelude hiding (FilePath)
import           System.Directory
import qualified System.FilePath
import           System.Environment
import           System.Environment.Executable
import qualified Text.CSS.Parse as CSS
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Turtle hiding (f, o, x, s, d, e, g, header)


main :: IO ()
main = do
  setProperCurrentDirectory

  [imagesPath, targetFile] <- getArgs

  -- find all svg files
  paths :: [FilePath] <- getSourceFiles [cs imagesPath] ["svg"]

  -- read all of them into strings
  raws :: [ST] <- (ST.readFile . cs) `mapM` paths

  -- parse them to tagsoup trees
  let trees :: [[TagTree ST]] = tagTree . parseTags <$> raws

  -- find <style> part and extract list of color class names
  let colorClassNames :: [[ST]] = findColorClasses <$> trees

  -- discard tags that are not svg
  let svgSubTrees :: [TagTree ST] = reduceToSvgTags <$> trees

  -- replace color classes in svg content with template variables
  let svgSubTreeTemplates :: [TagTree ST] = substituteColorClasses <$> svgSubTrees

  -- generate templates using https://hackage.haskell.org/package/template
  let result :: [Template] = mkTemplate <$> zip3 paths svgSubTreeTemplates colorClassNames

  -- run compiler backend
  let backend = case System.FilePath.takeExtension targetFile of
        ".php" -> backendPhp
        ".hs"  -> backendHaskell $ guessHsModuleName targetFile
        bad    -> error $ "unsupported target file extension: " <> show bad

  ST.writeFile targetFile $ backend result


-- * compile svgs into templates

-- | @TagBranch "style" [] [TagLeaf (TagText "  .st0{ fill:#d2d9df; }"), TagLeaf (TagText ".st1{fill:red;}")] => ["st0", "st1"]@
findColorClasses :: [TagTree ST] -> [ST]
findColorClasses = f
  where
    f :: [TagTree ST] -> [ST]
    f (TagBranch "style" _ leafs : _)      = g leafs
    f (TagBranch _ _ nodes : nodes')       = f $ nodes <> nodes'
    f (TagLeaf _ : nodes)                  = f nodes
    f []                                   = []

    g :: [TagTree ST] -> [ST]
    g (TagLeaf (TagText classdef) : nodes) = h classdef <> g nodes
    g (TagLeaf _ : nodes)                  = g nodes
    g (TagBranch _ _ nodes : nodes')       = g $ nodes <> nodes'
    g []                                   = []

    -- FIXME: we should be more specific when parsing the css: the class must specify the fill
    -- attribute and nothing else, or this script won't work!
    h :: ST -> [ST]
    h css = either bad good parse
      where
        parse = do  -- e.g., @[LeafBlock (".a",[("fill","#d2d9df")])] => ["a"]@
          mapM parseLeafClassDef =<< CSS.parseNestedBlocks css

        parseLeafClassDef (CSS.LeafBlock (classname, _)) = pure $ ST.tail classname
        parseLeafClassDef bad' = throwError $ "prsLeafClassDef: " <> show bad'

        bad msg = error $ "findColorClasses: could not parse style tag contents: " <> show (css, msg)
        good = id

-- | find the svg node and traverse the tree under it.  return all nodes that are svg tags, discard
-- all others.
--
-- ASSUMPTION: there is exactly one svg tag in the input.
reduceToSvgTags :: [TagTree ST] -> TagTree ST
reduceToSvgTags tree = f tree
  where
    f :: [TagTree ST] -> TagTree ST
    f (TagBranch "svg" attrs nodes : _)    = TagBranch "svg" attrs (mconcat $ g <$> nodes)
    f (TagBranch _ _ nodes : nodes')       = f $ nodes <> nodes'
    f (TagLeaf _ : nodes)                  = f nodes
    f []                                   = error $ "reduceToSvgTags: no svg node: " <> show tree

    g :: TagTree ST -> [TagTree ST]
    g (TagBranch tag attrs nodes)
      | tag `elem` svgTags                 = [TagBranch tag attrs (mconcat $ g <$> nodes)]
    g _                                    = []

    svgTags = [ "circle", "clipPath", "defs", "ellipse", "g", "image", "line", "linearGradient", "mask"
              , "path", "pattern", "polygon", "polyline", "radialGradient", "rect", "stop", "text", "tspan"
              ]

substituteColorClasses :: TagTree ST -> TagTree ST
substituteColorClasses = f
  where
    f :: TagTree ST -> TagTree ST
    f (TagBranch tag attrs nodes)          = TagBranch tag (g <$> attrs) (f <$> nodes)
    f (TagLeaf (TagOpen str attrs))        = TagLeaf (TagOpen str $ g <$> attrs)
    f noop@(TagLeaf _)                     = noop

    g :: Attribute ST -> Attribute ST
    g ("class", c)                         = assert (not . ST.any isSpace $ c) ("class", colorVarPrefix <> c)
    g noop                                 = noop

colorVarPrefix :: ST
colorVarPrefix = "$icon_color_"

unColorVarPrefix :: ST -> ST
unColorVarPrefix s = case ST.splitAt 11 s of
  ("icon_color_", s') -> s'
  bad                  -> error $ "unColorVarPrefix: " <> show bad

data Template = Template
  { tmplName           :: ST
  , tmplTemplateSource :: TagTree ST
  , tmplTemplateString :: ST
  , tmplProtoCtx       :: [ST]
  }
  deriving (Eq, Show)

mkTemplate :: (FilePath, TagTree ST, [ST]) -> Template
mkTemplate (path, tree, colors) = Template
  (cs . ("svg_" <>) . System.FilePath.takeBaseName . cs $ path)
  tree
  (renderTree [tree])
  colors


-- * php backend

type Backend = [Template] -> ST

mkSimpleBackend :: Template -> (ST -> ST) -> ST -> ST -> ST
mkSimpleBackend tmpl resolve header footer = header <> cs body <> footer
  where
    body :: LT
    body = TMPL.substitute (tmplTemplateString tmpl) resolve

backendPhp :: Backend
backendPhp tmpls = ST.unlines (mkbe <$> tmpls) <> testPage
  where
    testPage :: ST
    testPage = ST.unlines $ "<?php function svgTestPage() { ?>" : (go <$> tmpls) <> ["<?php } ?>"]
      where
        go (Template funname _ _ (length -> numargs)) = ST.unlines
          [ "<hr><div>"
          , block . take numargs $ palette
          , block . take numargs $ drop 1 palette
          , "</div>"
          ]
          where
            block palette' = "<p>" <> txt <> "</p>"
                          <> "<div style=" <> styles <> "><?php " <> txt <> " ?></div>"
              where
                styles = "\"position: relative; width: 70px; margin: 3px; border: 1px dashed black;\""
                txt = funname <> "(" <> ST.intercalate ", " palette' <> ");"

        palette :: [ST]
        palette = (cs . show <$> p) <> palette
          where
            p = (("c_fill_" :: ST) <>) <$>
              ["main_menu_dark_blue", "interaction_orange_neon", "interaction_red", "form_green", "interaction_yellow_neon"]

    mkbe :: Template -> ST
    mkbe tmpl = mkSimpleBackend tmpl resolve header footer
      where
        resolve = ("<?= $" <>) . (<> " ?>")

        header = "<?php function " <> funname <> "(" <> funargs <> ") { ?>\n"
          where
            funname = tmplName tmpl
            funargs = ST.intercalate ", " $ (colorVarPrefix <>) <$> tmplProtoCtx tmpl

        footer = "\n<?php } ?>\n"


-- * hs backend

-- | @pkg/frontend/src/Refine/Frontend/Images.hs@ => @Refine.Frontend.Images@
guessHsModuleName :: System.FilePath.FilePath -> ST
guessHsModuleName path = case System.FilePath.splitDirectories $ System.FilePath.dropExtensions path of
  ("pkg" : "frontend" : "src" : xs) -> ST.intercalate "." (cs <$> xs)
  _ -> "Main"

backendHaskell :: ST -> Backend
backendHaskell modulename ts = ST.unlines $ header <> (icon <$> ts)
  where
    header :: [ST]
    header =
      [ "module " <> modulename <> " where"
      , ""
      , "import React.Flux"
      , "import Refine.Common.Color"
      , ""
      ]

    icon :: Template -> ST
    icon (Template funname tree _ protoctx) = ST.unlines $ hdr <> bdy
      where
        hdr :: [ST]
        hdr =
          [ funname <> " :: " <> ST.replicate (length protoctx) "Color -> " <> "ReactElementM h ()"
          , funname <> " " <> ST.unwords args <> " = do"
          ]

        mkArg :: Int -> ST
        mkArg = ("c" <>) . cs . show

        args :: [ST]
        args = zipWith (\i _ -> mkArg i) [0..] protoctx

        bdy :: [ST]
        bdy = filter (not . ST.null) $ rtree "  " tree

        rtree :: ST -> TagTree ST -> [ST]
        rtree indent = \case
          TagBranch tagname attrs []    -> [rtag indent (TagOpen tagname attrs) <> " mempty"]
          TagBranch tagname attrs nodes -> rtag indent (TagOpen tagname attrs) : mconcat (rtree ("  " <> indent) <$> nodes)
          TagLeaf tag                   -> [rtag indent tag]

        rtag :: ST -> Tag ST -> ST
        rtag indent = (indent <> ) . \case
          TagOpen tagname []    -> tagname <> "_ $ do"
          TagOpen tagname attrs -> tagname <> "_ [" <> ST.intercalate ", " (rattr <$> attrs) <> "] $ do"
          TagClose _tagname     -> ""
          TagText txt           -> "elemText " <> cs (show txt)
          TagComment _cmt       -> ""
          TagWarning _          -> ""
          TagPosition _ _       -> ""

        rattr :: Attribute ST -> ST
        rattr (k, v) = cs (show k) <> " $= " <> rattrval v

        rattrval :: ST -> ST
        rattrval tpl = if after == before then after else "(" <> after <> ")"
          where
            before = "\"" <> cs (show tpl) <> "\""
            after  = cs $ TMPL.substitute tpl resolve

            -- @..., class = "$icon_color_foo_blrg", ...@ => @..., class = mkFillColorClass c1, ...@
            -- input @class = "other_class $icon_color_foo"@ is not supported
            resolve :: ST -> ST
            resolve attrval = case elemIndex (unColorVarPrefix attrval) protoctx of
              Nothing -> attrval
              Just i  -> "colorName ColorAttrFill " <> mkArg i


-- * code stolen from style-guide.hs

getSourceFiles :: MonadIO m => [FilePath] -> [ST] -> m [FilePath]
getSourceFiles roots exts = filterExts exts <$> getAllFiles roots

-- | (not directories)
getAllFiles :: MonadIO m => [FilePath] -> m [FilePath]
getAllFiles roots = do
  paths <- fmap mconcat $ (\r -> lstree r `fold` Fold.list) `mapM` roots
  filterM (liftIO . doesFileExist . cs) paths

filterExts :: [ST] -> [FilePath] -> [FilePath]
filterExts exts = filter ((`elem` (Just <$> exts)) . extension)


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
