{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Code for handling `VDocVersions` with HTML content.
--
-- TODO: think of a better module name / place?
module Refine.Common.VDoc.HTML where

import           Data.Char (isSpace)
import           Data.String.Conversions (ST, (<>))
import qualified Data.Text as ST
import           Text.HTML.Parser (Token(..), canonicalizeTokens)


-- | TODO: type and implementation will change quite a bit, this is just a dummy.
canonicalizeVDocVersion :: [Token] -> [Token]
canonicalizeVDocVersion
    = fmap (onText canonicalizeWhitespace)
    . canonicalizeTokens

onText :: (ST -> ST) -> Token -> Token
onText f (ContentText s) = ContentText $ f s
onText _ t = t

canonicalizeWhitespace :: ST -> ST
canonicalizeWhitespace t = leading t <> ST.intercalate "\n" (ST.words t) <> trailing t
  where
    leading s = if ST.all isSpace (ST.take 1 s) then "\n" else ""
    trailing = leading . ST.reverse
