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

import Refine.Frontend.Prelude hiding (S)

import Language.Css.Syntax hiding (S)

import           Refine.Frontend.CS ()
import           Refine.Frontend.Types
import           Refine.Frontend.Util


-- * css

data IconSize
  = S
  | M
  | L
  | XL
  | XXL
  deriving (Eq, Show)


instance Css IconSize where
  css = f . \case
    S   -> 9
    M   -> 14
    L   -> 20
    XL  -> 26
    XXL -> 32
    where
      f i = [ decl "backgroundSize" (Percentage 100)
            , decl "width" (Px i)
            , decl "height" (Px i)
            ]


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
