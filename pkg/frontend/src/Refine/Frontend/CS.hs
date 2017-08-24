{-# LANGUAGE CPP #-}
#include "language.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.CS
where

import Refine.Common.Prelude

import React.Flux

import qualified Data.JSString as JSS
import           Data.String (fromString)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT

import Refine.Common.Types.Translation (TKey)

{-# ANN module ("HLint: ignore Use cs" :: String) #-}


{- FUTUREWORK: we used to have this:

```
instance {-# OVERLAPPABLE #-} (ConvertibleStrings a String, IsString b)
  => ConvertibleStrings a b
  where
    convertString = fromString . cs
```

but that made ghcjs choke (not enough heap).  possibly a ghcjs bug?

-}

instance ConvertibleStrings JSS.JSString JSS.JSString where
  convertString = id

instance ConvertibleStrings JSS.JSString String where
  convertString = JSS.unpack

instance ConvertibleStrings String JSS.JSString where
  convertString = JSS.pack

instance ConvertibleStrings JSS.JSString ST where
  convertString = ST.pack . JSS.unpack

instance ConvertibleStrings ST JSS.JSString where
  convertString = JSS.pack . ST.unpack

instance ConvertibleStrings JSS.JSString LT where
  convertString = LT.pack . JSS.unpack

instance ConvertibleStrings LT JSS.JSString where
  convertString = JSS.pack . LT.unpack

instance ConvertibleStrings JSS.JSString LBS where
  convertString = cs . ST.pack . JSS.unpack

instance ConvertibleStrings LBS JSS.JSString where
  convertString = JSS.pack . ST.unpack . cs


instance (a ~ ()) => ConvertibleStrings String (ReactElementM_ handler a) where
  convertString = fromString

instance (a ~ ()) => ConvertibleStrings ST (ReactElementM_ handler a) where
  convertString = fromString . cs

instance (a ~ ()) => ConvertibleStrings LT (ReactElementM_ handler a) where
  convertString = fromString . cs

instance (a ~ ()) => ConvertibleStrings LBS (ReactElementM_ handler a) where
  convertString = fromString . cs

instance (a ~ ()) => ConvertibleStrings JSS.JSString (ReactElementM_ handler a) where
  convertString = fromString . cs


-- * getting the type checker to like all this...

-- | The return type of the result monad type needs to be fixed to '()', because usually on the call
-- site there is no reason for the type checker to assume that.
elemCS :: HasCallStack => ConvertibleStrings s (ReactElementM_ handler ()) => s -> ReactElementM_ handler ()
elemCS = cs

-- | this is a sub-type of TranslationsCS that makes the type checker not trip over the constraints.
type TranslationsRE = TKey -> forall handler. ReactElementM handler ()
