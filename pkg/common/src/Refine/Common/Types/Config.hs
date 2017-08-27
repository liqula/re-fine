{-# LANGUAGE CPP #-}
#include "language_common.hs"

module Refine.Common.Types.Config where
#include "import_common.hs"


data ClientCfg = ClientCfg
  { _clientCfgWSPort :: Int
  , _clientCfgWSHost :: ST
  , _clientCfgWSSSL  :: Bool
  }
  deriving (Eq, Show, Generic)

instance Default ClientCfg where
  def = ClientCfg 3000 "localhost" False

deriveClasses [([''ClientCfg], [''SOP.Generic, ''Lens', ''FromJSON])]
