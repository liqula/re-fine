{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Orphans where

import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Tree (Tree(..))


instance Ord a => Ord (Tree a) where
  compare (Node x xs) (Node x' xs') = compare (x, xs) (x', xs')

instance (ToJSON k, ToJSON v) => ToJSON (Map k v) where
  toJSON = toJSON . Map.toList

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
  parseJSON = fmap Map.fromList . parseJSON
