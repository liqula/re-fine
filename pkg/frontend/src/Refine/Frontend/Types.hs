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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Types where

import Data.Char (toLower)
import Data.JSString (JSString)
import Data.String.Conversions

import Refine.Frontend.CS ()


class CssClass a where
  showCssClass :: a -> JSString

type ReactListKey = JSString  -- do not move this to Frontend.Types, importing this here creates a cycle.

data IconSize
  = S
  | M
  | L
  | XL
  | XXL
  deriving (Eq, Show)

instance CssClass IconSize where
  showCssClass = ("iconsize-" <>) . cs . fmap toLower . show

type IconDescription = (JSString, JSString)
