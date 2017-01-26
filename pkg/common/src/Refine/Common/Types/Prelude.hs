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

module Refine.Common.Types.Prelude where

import Control.DeepSeq
import Control.Lens
import Data.Int
import Data.String.Conversions (cs)
import Generics.SOP        as SOP
import Generics.SOP.JSON   as SOP
import Generics.SOP.NFData as SOP
import GHC.Generics        as GHC
import Text.Read
import Web.HttpApiData

import Refine.Prelude (ClearTP(..))
import Refine.Prelude.Generic


newtype ID a = ID { _unID :: Int64 }
  deriving (Eq, Ord, Show, Read, GHC.Generic)

instance ClearTP ID where
  clearTP (ID x) = ID x

type family Create a :: *


-- * lens

makeLenses ''ID
makePrisms ''ID

instance SOP.Generic (ID a)
instance SOP.HasDatatypeInfo (ID a)
instance NFData (ID a) where rnf = SOP.grnf
instance ToJSON (ID a) where toJSON = gtoJSONDef
instance FromJSON (ID a) where parseJSON = gparseJSONDef


-- * HttpApiData

instance ToHttpApiData (ID a) where
  toUrlPiece (ID x) = cs $ show x

instance FromHttpApiData (ID a) where
  parseUrlPiece = either (Left . cs) (Right . ID) . readEither @Int64 . cs
