{-# LANGUAGE NoImplicitPrelude          #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Icon.Types
  ( ReactListKey
  , IconSize(..)
  , CssClass(..)
  , IconDescription

  , IconProps(..)
  , iconPropsBlockName
  , iconPropsHighlight
  , iconPropsDesc
  , iconPropsSize

  , IconButtonPropsWithHandler(..)
  , iconButtonPropsListKey
  , iconButtonPropsIconProps
  , iconButtonPropsElementName
  , iconButtonPropsModuleName
  , iconButtonPropsLabel
  , iconButtonPropsDisabled
  , iconButtonPropsPosition
  , iconButtonPropsAlignRight
  , iconButtonPropsOnClick
  , iconButtonPropsClickPropag
  , iconButtonPropsExtraClasses
  ) where

import Refine.Frontend.Prelude

import           Refine.Frontend.CS ()
import           Refine.Frontend.Types


-- * css

data IconSize
  = S
  | M
  | L
  | XL
  | XXL
  deriving (Eq, Show)


class CssClass a where  -- TODO: remove; use @css :: a -> [Decl]@ instead.
  showCssClass :: a -> JSString

instance CssClass IconSize where
  showCssClass = ("iconsize-" <>) . cs . fmap toLower . show


-- * icon

data IconProps = IconProps
  { _iconPropsBlockName :: JSString
  , _iconPropsHighlight :: Bool
  , _iconPropsDesc      :: IconDescription
  , _iconPropsSize      :: IconSize
  }
  deriving (Eq)

type IconDescription = (JSString, JSString)

instance UnoverlapAllEq IconProps
makeLenses ''IconProps

instance Default IconProps where
  def = IconProps
    { _iconPropsBlockName = ""
    , _iconPropsHighlight = False
    , _iconPropsDesc      = ("", "")
    , _iconPropsSize      = L
    }


-- * icon button

data IconButtonPropsWithHandler onclick = IconButtonProps
  { _iconButtonPropsListKey      :: ReactListKey  -- (this is not morally part of the props, but it's convenient to keep it here.)
  , _iconButtonPropsIconProps    :: IconProps
  , _iconButtonPropsElementName  :: JSString
  , _iconButtonPropsModuleName   :: JSString
  , _iconButtonPropsLabel        :: JSString
  , _iconButtonPropsDisabled     :: Bool  -- TODO: make this 'enabled'
  , _iconButtonPropsPosition     :: Maybe Int
  , _iconButtonPropsAlignRight   :: Bool
  , _iconButtonPropsOnClick      :: onclick
  , _iconButtonPropsClickPropag  :: Bool
  , _iconButtonPropsExtraClasses :: [JSString]
  }
  deriving (Eq)

instance UnoverlapAllEq (IconButtonPropsWithHandler onclick)
makeLenses ''IconButtonPropsWithHandler
