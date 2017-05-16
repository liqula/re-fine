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


-- | see also: #211
-- FIXME: how do we represent this?  @[OldStyle.StyleST "border-bottom" "2px solid", OldStylemkStyle
-- "border-bottom" Color.VDocRollover]@ these types need more work...
module Refine.Frontend.Style
  ( Style(..), styleCS
  , IsStyle(..)
  ) where

import Refine.Frontend.Prelude


-- | 'Style' constructor takes a ToJSON value; the other constructors take values of concrete types.
data Style =
    StyleInt ST Int
  | StyleDouble ST Double
  | StyleST ST ST
  | StyleRem ST Double

  -- The following constructors follow
  -- <http://hackage.haskell.org/package/language-css-0.0.3/docs/Language-Css-Syntax.html#g:6>.

  -- StyleIdent ST Int
  -- StyleFunc  ST Ident Expr
  -- StyleDeg ST Double
  -- StyleRad ST Double
  -- StyleGrad ST Double
  -- StyleCword ST ST
  -- StyleHz ST Double
  -- StyleKHz ST Double
  | StyleEm ST Double
  | StyleEx ST Double
  | StylePx ST Int
  | StyleIn ST Double
  | StyleCm ST Double
  | StyleMm ST Double
  | StylePc ST Double
  | StylePt ST Int
  | StylePercentage ST Double
  -- StyleMs ST Double
  -- StyleS ST Double
  -- StyleUri ST ST
  deriving (Eq)

instance {-# OVERLAPPING #-} ToJSON [Style] where
  toJSON = object . fmap (\case
    StyleInt         k v -> k .:= vshow v
    StyleDouble      k v -> k .:= vshow v
    StyleST          k v -> k .:= v
    StyleRem         k v -> k .:= (vshow v <> "rem")

    -- StyleIdent      k v -> k .:= v
    -- StyleFunc       k v e -> k .:= [v, e]  -- (probably not)
    -- StyleDeg        k v -> k .:= v
    -- StyleRad        k v -> k .:= v
    -- StyleGrad       k v -> k .:= v
    -- StyleCword      k v -> k .:= v
    -- StyleHz         k v -> k .:= v
    -- StyleKHz        k v -> k .:= v
    StyleEm         k v -> k .:= (vshow v <> "em")  -- TODO: only add unit of measure if /= 0
    StyleEx         k v -> k .:= (vshow v <> "ex")
    StylePx         k v -> k .:= (vshow v <> "px")
    StyleIn         k v -> k .:= (vshow v <> "in")
    StyleCm         k v -> k .:= (vshow v <> "cm")
    StyleMm         k v -> k .:= (vshow v <> "mm")
    StylePc         k v -> k .:= (vshow v <> "pc")
    StylePt         k v -> k .:= (vshow v <> "pt")
    StylePercentage k v -> k .:= (vshow v <> "%")
    -- StyleMs         k v -> k .:= v
    -- StyleS          k v -> k .:= v
    -- StyleUri        k v -> k .:= v
    )


-- TODO: there should be a faster implementation of converting doubles and ints to aeson string.
-- look it up and use it!
vshow :: Show a => a -> String
vshow = show


class IsStyle a where
  mkStyle :: ST -> a -> Style


-- | (would be nice to have this as a constructor, but then 'Eq' wouldn't be derivable any more.
-- would be nice to make it a 'IsStyle' instance, but that would overlap.)
styleCS :: forall s. (ConvertibleStrings s ST) => ST -> s -> Style
styleCS k v = StyleST k (cs v)
