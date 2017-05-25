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

  , Align(..)
  , IbuttonProps(..)
  , ibEnabled
  , ibSize
  , ibListKey
  , ibAlign
  , ibClickPropag
  , ibLabel
  , ibOnClick
  , ibPosition
  , ibDarkBackground
  , ibImage

  , IconSize(..)
  , sizePx

  , IconDescription

  , BackgroundImageState(..)
  , BackgroundImage(..)
  , backgroundImageName
  , backgroundImageState
  , iconCssClass

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

import Refine.Frontend.Prelude hiding (S, fn)

import Language.Css.Syntax hiding (S)
import Language.Css.Build hiding (ex, s)

import           Refine.Frontend.CS ()
import           Refine.Frontend.Types
import           Refine.Frontend.Util


-- * icon buttons

data Align = AlignRight | AlignLeft
  deriving (Eq, Show, Generic)

data IbuttonProps onclick = IbuttonProps
  { _ibListKey          :: ReactListKey  -- ^ this is not morally part of the props, but it's convenient to keep it here.
  , _ibLabel            :: ST
  , _ibDarkBackground   :: Bool
  , _ibImage            :: ST
  , _ibOnClick          :: onclick
  , _ibClickPropag      :: Bool
  , _ibEnabled          :: Bool
  , _ibSize             :: IconSize
  , _ibAlign            :: Align
  , _ibPosition         :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance UnoverlapAllEq (IbuttonProps onclick)


-- * icon sizes

data IconSize
  = Medium
  | Large
  | XLarge
  | XXLarge
  deriving (Eq, Show)

sizePx :: IconSize -> Px
sizePx Medium  = Px 14
sizePx Large   = Px 20
sizePx XLarge  = Px 26
sizePx XXLarge = Px 32

instance Css IconSize where
  css s = [ decl "backgroundSize" (Percentage 100)
          , decl "width" (sizePx s)
          , decl "height" (sizePx s)
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


-- * TH instances

makeLenses ''BackgroundImage
makeLenses ''IbuttonProps


-- * outdated

-- FUTUREWORK: the rest of this module should be removed and everything ported to the ibutton_
-- component above.  this may take a few more steps, though.


-- ** icon

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


-- ** icon button

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
