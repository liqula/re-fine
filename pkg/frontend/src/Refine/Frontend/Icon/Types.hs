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

  , IconButtonPropsWithHandler(..), IconButtonProps
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

  , IconButtonPropsOnClick(..)
  ) where

import Refine.Frontend.Prelude

import           Refine.Frontend.CS ()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Store (dispatchMany)
import           Refine.Frontend.Types


-- * icon

data IconProps = IconProps
  { _iconPropsBlockName :: JSString
  , _iconPropsHighlight :: Bool
  , _iconPropsDesc      :: IconDescription
  , _iconPropsSize      :: IconSize
  }
  deriving (Eq)

instance UnoverlapAllEq IconProps
makeLenses ''IconProps

instance Default IconProps where
  def = IconProps
    { _iconPropsBlockName = ""
    , _iconPropsHighlight = False
    , _iconPropsDesc      = ("", "")
    , _iconPropsSize      = L
    }

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

instance IconButtonPropsOnClick onclick => Default (IconButtonPropsWithHandler onclick) where
  def = IconButtonProps
    { _iconButtonPropsListKey      = ""
    , _iconButtonPropsIconProps    = def
    , _iconButtonPropsElementName  = ""
    , _iconButtonPropsModuleName   = ""
    , _iconButtonPropsLabel        = ""
    , _iconButtonPropsDisabled     = False
    , _iconButtonPropsPosition     = Nothing
    , _iconButtonPropsAlignRight   = False
    , _iconButtonPropsOnClick      = defaultOnClick
    , _iconButtonPropsClickPropag  = True  -- Iff 'False', call 'stopPropagation'.  See 'mkClickHandler'.
    , _iconButtonPropsExtraClasses = []
    }

class (Typeable onclick, Eq onclick) => IconButtonPropsOnClick onclick where
  runIconButtonPropsOnClick :: Event -> MouseEvent -> onclick -> ViewEventHandler
  defaultOnClick            :: onclick  -- ^ @instance Default [GlobalAction]@ would lead to overlaps.

type IconButtonProps = IconButtonPropsWithHandler [GlobalAction]

instance IconButtonPropsOnClick [GlobalAction] where
  runIconButtonPropsOnClick _ _ = dispatchMany
  defaultOnClick                = mempty
