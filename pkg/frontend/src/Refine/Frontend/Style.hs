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


-- | see also: #211
module Refine.Frontend.Style where

import           Data.Aeson ((.=), object)
import           Data.Aeson.Types (ToJSON, toJSON)
import           Data.String.Conversions


-- | 'Style' constructor takes a ToJSON value; the other constructors take values of concrete types.
-- Our types have the same structure as
-- <http://hackage.haskell.org/package/language-css-0.0.3/docs/Language-Css-Syntax.html#g:6>.
data Style where
  Style :: forall a. (ToJSON a) => ST -> a -> Style  -- FIXME: this is deprecated, use the other constructors instead!

  StyleIdent      :: ST -> Int -> Style
  -- StyleFunc       :: ST -> Ident -> Expr -> Style  -- (not sure how this works, and we're not using it yet.)
  StyleDeg        :: ST -> Double -> Style
  StyleRad        :: ST -> Double -> Style
  StyleGrad       :: ST -> Double -> Style
  StyleCword      :: ST -> ST -> Style
  StyleHz         :: ST -> Double -> Style
  StyleKHz        :: ST -> Double -> Style
  StyleEm         :: ST -> Double -> Style
  StyleEx         :: ST -> Double -> Style
  StylePx         :: ST -> Int -> Style
  StyleIn         :: ST -> Double -> Style
  StyleCm         :: ST -> Double -> Style
  StyleMm         :: ST -> Double -> Style
  StylePc         :: ST -> Double -> Style
  StylePt         :: ST -> Int -> Style
  StylePercentage :: ST -> Double -> Style
  StyleMs         :: ST -> Double -> Style
  StyleS          :: ST -> Double -> Style
  StyleUri        :: ST -> ST -> Style

instance ToJSON [Style] where
  toJSON = object . fmap (\case
    Style k v -> k .= v

    StyleIdent      k v -> k .= v
    -- StyleFunc       k v e -> k .= [v, e]
    StyleDeg        k v -> k .= v
    StyleRad        k v -> k .= v
    StyleGrad       k v -> k .= v
    StyleCword      k v -> k .= v
    StyleHz         k v -> k .= v
    StyleKHz        k v -> k .= v
    StyleEm         k v -> k .= v
    StyleEx         k v -> k .= v
    StylePx         k v -> k .= v
    StyleIn         k v -> k .= v
    StyleCm         k v -> k .= v
    StyleMm         k v -> k .= v
    StylePc         k v -> k .= v
    StylePt         k v -> k .= v
    StylePercentage k v -> k .= v
    StyleMs         k v -> k .= v
    StyleS          k v -> k .= v
    StyleUri        k v -> k .= v
    )
