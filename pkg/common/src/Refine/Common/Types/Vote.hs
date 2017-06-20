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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}

module Refine.Common.Types.Vote where

import Refine.Common.Prelude

import GHC.Generics (Generic)

import Refine.Common.Types.Prelude (ID, User)

data Vote = Yeay | Nay
  deriving (Eq, Ord, Show, Generic)

type Votes = Map (ID User) Vote

type VoteCount = Map Vote Int


makeRefineType ''Vote

-- to be able to use Vote as Map key
instance ToJSONKey Vote where
  toJSONKey = toJSONKeyText $ \case
    Yeay -> "yeay"
    Nay  -> "nay"

instance FromJSONKey Vote where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case t of
        "yeay"  -> pure Yeay
        "nay"   -> pure Nay
        _       -> fail $ "Cannot parse key into Vote: " <> cs t

instance ToHttpApiData Vote where
  toUrlPiece Yeay  = "yeay"
  toUrlPiece Nay   = "nay"

instance FromHttpApiData Vote where
  parseUrlPiece "yeay" = Right Yeay
  parseUrlPiece "nay"  = Right Nay
  parseUrlPiece bad    = Left . cs $ "FromHttpApiData @Vote: no parse: " <> show bad
