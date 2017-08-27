{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Common.Types.Vote where
#include "import_common.hs"

import GHC.Generics (Generic)

import Refine.Common.Types.Prelude (ID, User)


data Vote = Yeay | Nay
  deriving (Eq, Ord, Show, Generic)

type Votes = Map (ID User) Vote

type VoteCount = Map Vote Int

votesToCount :: Votes -> VoteCount
votesToCount = Map.fromListWith (+) . map (flip (,) 1) . Map.elems

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
