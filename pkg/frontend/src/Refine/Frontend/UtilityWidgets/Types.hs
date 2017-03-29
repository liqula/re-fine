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

module Refine.Frontend.UtilityWidgets.Types
  ( ReactListKey
  , IconSize(..)
  , IconDescription

  , IconProps(..)
  , iconPropsBlockName
  , iconPropsHighlight
  , iconPropsDesc
  , iconPropsSize

  , IconButtonProps(..)
  , iconButtonPropsListKey
  , iconButtonPropsIconProps
  , iconButtonPropsElementName
  , iconButtonPropsModuleName
  , iconButtonPropsLabel
  , iconButtonPropsDisabled
  , iconButtonPropsPosition
  , iconButtonPropsAlignRight
  , iconButtonPropsClickActions
  , iconButtonPropsClickPropag
  , iconButtonPropsExtraClasses
  ) where

import           Control.Lens (makeLenses)
import           Data.Default (Default(def))
import           GHCJS.Types (JSString)

import           Refine.Frontend.CS ()
import           Refine.Frontend.Store.Types
import           Refine.Frontend.Types


-- * icon

data IconProps = IconProps
  { _iconPropsBlockName :: JSString
  , _iconPropsHighlight :: Bool
  , _iconPropsDesc      :: IconDescription
  , _iconPropsSize      :: IconSize
  }
  deriving (Eq)

makeLenses ''IconProps

instance Default IconProps where
  def = IconProps
    { _iconPropsBlockName = ""
    , _iconPropsHighlight = False
    , _iconPropsDesc      = ("", "")
    , _iconPropsSize      = L
    }

data IconButtonProps = IconButtonProps
  { _iconButtonPropsListKey      :: ReactListKey  -- (this is not morally part of the props, but it's convenient to keep it here.)
  , _iconButtonPropsIconProps    :: IconProps
  , _iconButtonPropsElementName  :: JSString
  , _iconButtonPropsModuleName   :: JSString
  , _iconButtonPropsLabel        :: JSString
  , _iconButtonPropsDisabled     :: Bool  -- TODO: make this 'enabled'
  , _iconButtonPropsPosition     :: Maybe Int
  , _iconButtonPropsAlignRight   :: Bool
  , _iconButtonPropsClickActions :: [GlobalAction]
  , _iconButtonPropsClickPropag  :: Bool
  , _iconButtonPropsExtraClasses :: [JSString]
  }
  deriving (Eq)

makeLenses ''IconButtonProps

instance Default IconButtonProps where
  def = IconButtonProps
    { _iconButtonPropsListKey      = ""
    , _iconButtonPropsIconProps    = def
    , _iconButtonPropsElementName  = ""
    , _iconButtonPropsModuleName   = ""
    , _iconButtonPropsLabel        = ""
    , _iconButtonPropsDisabled     = False
    , _iconButtonPropsPosition     = Nothing
    , _iconButtonPropsAlignRight   = False
    , _iconButtonPropsClickActions = []
    , _iconButtonPropsClickPropag  = True  -- Iff 'False', call 'stopPropagation'.  See 'mkClickHandler'.
    , _iconButtonPropsExtraClasses = []
    }
