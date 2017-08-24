{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Common.Types.Config where

import Refine.Common.Prelude

import qualified Generics.SOP as SOP


data ClientCfg = ClientCfg
  { _clientCfgWSPort :: Int
  , _clientCfgWSHost :: ST
  , _clientCfgWSSSL  :: Bool
  }
  deriving (Eq, Show, Generic)

instance Default ClientCfg where
  def = ClientCfg 3000 "localhost" False

deriveClasses [([''ClientCfg], [''SOP.Generic, ''Lens', ''FromJSON])]
