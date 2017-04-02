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

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Char (toLower)
import Data.JSString (JSString)
import Data.Maybe (catMaybes)
import Data.String.Conversions
import GHC.Generics (Generic)

import Refine.Common.Types
import Refine.Frontend.CS ()
import Refine.Prelude.TH (makeRefineType, makeSOPGeneric, makeNFData)
import Refine.Prelude.Aeson ((.=?))


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


-- | `viewport` is the browser window (the visible part of the `document`, or `page`).  See
-- `/docs/frontend/offsets.pdf`.
newtype OffsetFromViewportTop = OffsetFromViewportTop { _unOffsetFromViewportTop :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

-- | Distance between document top and viewport top.  See `/docs/frontend/offsets.pdf`.
newtype ScrollOffsetOfViewport = ScrollOffsetOfViewport { _unScrollOffsetOfViewport :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

-- | Distance between document top and node (e.g., `<mark>` or `</mark>`).
newtype OffsetFromDocumentTop = OffsetFromDocumentTop { _unOffsetFromDocumentTop :: Int }
  deriving (Show, Generic, Eq, Ord, Num)

makeRefineType ''OffsetFromViewportTop
makeRefineType ''ScrollOffsetOfViewport
makeRefineType ''OffsetFromDocumentTop


data Range = Range
    { _rangeStartPoint   :: Maybe ChunkPoint
    , _rangeEndPoint     :: Maybe ChunkPoint
    , _rangeTopOffset    :: OffsetFromViewportTop
    , _rangeBottomOffset :: OffsetFromViewportTop
    , _rangeScrollOffset :: ScrollOffsetOfViewport
    }
    deriving (Show, Eq, Generic)

makeLenses ''Range
makeSOPGeneric ''Range
makeNFData ''Range

instance FromJSON Range where
    parseJSON = withObject "Range" $ \v -> Range <$>
                             v .:? "start" <*>
                             v .:? "end" <*>
                             v .: "top" <*>
                             v .: "bottom" <*>
                             v .: "scrollOffset"

instance ToJSON Range where
    toJSON (Range sp ep t b s) = object $
      catMaybes [ "start" .=? sp
                , "end"   .=? ep
                ]
      <> [ "top"          .= t
         , "bottom"       .= b
         , "scrollOffset" .= s
         ]

data Selection =
    NothingSelected
  | NothingSelectedButUpdateTriggered OffsetFromDocumentTop  -- TODO when can this happen?
  | RangeSelected Range OffsetFromDocumentTop
  deriving (Show, Eq, Generic)

makeRefineType ''Selection
