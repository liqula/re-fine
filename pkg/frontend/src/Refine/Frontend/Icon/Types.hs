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


-- * icon sizes

data IconSize
  = Medium
  | Large
  | XLarge
  | XXLarge
  deriving (Eq, Show)


instance Css IconSize where
  css = f . \case
    Medium  -> 14
    Large   -> 20
    XLarge  -> 26
    XXLarge -> 32
    where
      f i = [ decl "backgroundSize" (Percentage 100)
            , decl "width" (Px i)
            , decl "height" (Px i)
            ]


-- * background images

data BackgroundImageState = BisRO | BisBright | BisDark
  deriving (Eq, Show, Generic)

data BackgroundImage = BackgroundImage
  { _backgroundImageName  :: ST
  , _backgroundImageState :: BackgroundImageState
  }
  deriving (Eq, Show, Generic)

-- FIXME: this instance does not work, since webpack loads all images into the bundle and replaces
-- the paths with data-urls in the css class, but not in the inline-styles here.  use 'iconCssClass'
-- instead for now.
instance Css BackgroundImage where
  css bimg = [decl "backgroundImage" ex]
    where
      ex = Func "url" (expr $ Ident fp)
      fp = "\"../images/" <> cs (iconCssClass bimg) <> ".svg\""

-- | work-around for @instance Css BackgroundImage@.
iconCssClass :: BackgroundImage -> JSString
iconCssClass (BackgroundImage fn st) = mconcat ["icon-", cs fn, "_", renderState st]
    where
      renderState BisRO     = "RO"
      renderState BisBright = "bright"
      renderState BisDark   = "dark"

makeLenses ''BackgroundImage


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
    , _iconPropsSize      = Large
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
