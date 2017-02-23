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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Prelude.Generic where

import           Data.Aeson
import           Data.Aeson.Types (Parser)

import qualified Generics.SOP        as SOP
import qualified Generics.SOP.JSON   as SOP


-- * generic json

gtoJSONDef :: forall a . (SOP.Generic a, SOP.HasDatatypeInfo a, SOP.All2 ToJSON (SOP.Code a)) => a -> Value
gtoJSONDef = SOP.gtoJSON SOP.defaultJsonOptions

gparseJSONDef :: forall a. (SOP.Generic a, SOP.HasDatatypeInfo a, SOP.All2 FromJSON (SOP.Code a)) => Value -> Parser a
gparseJSONDef = SOP.gparseJSON SOP.defaultJsonOptions
