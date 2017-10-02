{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Icon.Types
  ( ReactListKey
  , Svg.ColorSchema(..)
  , Svg.ButtonState(..)
  , Svg.ButtonRollOver(..)

  , ButtonImage(..)
  , IbuttonProps(..)
  , ibListKey
  , ibOnClick
  , ibOnClickMods
  , ibPressed
  , ibImage
  , ibIndexNum
  , ibEnabled
  , ibGrayedOut
  , ibSize
  , IbuttonState(..)
  , ibuttonMouseOver
  , ibuttonState
  , IbuttonOnClick(..)
  , IconSize(..), iconSizeCls
  ) where
#include "import_frontend.hs"

import Language.Css.Syntax hiding (S)
import Language.Css.Build hiding (ex, s)

import           Refine.Common.Types.Prelude
import           Refine.Frontend.CS ()
import qualified Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Types
import           Refine.Frontend.Util


-- * icon buttons

data ButtonImage
  = ButtonImageIcon Svg.Icon Svg.ColorSchema  -- ^ inlined icon
  | ButtonImageInline ImageInline  -- ^ "data:image/..."
  deriving (Eq, Show, Generic)

data IbuttonProps onclick = IbuttonProps
  { _ibListKey          :: ReactListKey  -- ^ this is not morally part of the props, but it's convenient to keep it here.
  , _ibOnClick          :: onclick
  , _ibOnClickMods      :: [EventModification]
  , _ibPressed          :: Maybe Bool  -- ^ @Just _@: behave like a light switch; @Nothing@: behave like a car honk.
  , _ibImage            :: ButtonImage
  , _ibIndexNum         :: Maybe Int -- ^ the number in the small black circle in the upper right corner of the button.
  , _ibEnabled          :: Bool
  , _ibGrayedOut        :: Bool
  , _ibSize             :: IconSize
  }
  deriving (Eq, Show, Generic)

data IbuttonState st = IbuttonState { _ibuttonMouseOver :: Bool, _ibuttonState :: st }
  deriving (Eq, Ord, Generic)


-- * click events

class (Typeable onclick, Eq onclick) => IbuttonOnClick onclick handler where
  runIbuttonOnClick :: Event -> MouseEvent -> onclick -> EventHandlerType handler


-- * icon sizes

-- | FUTUREWORK: eliminate duplication between this and the corresponding scss variables.
data IconSize
  = Medium
  | Large
  | XLarge
  | XXLarge
  | XXXLarge
  deriving (Eq, Show)

iconSizeCls :: IconSize -> ST
iconSizeCls Medium   = "ibutton_medium"
iconSizeCls Large    = "ibutton_large"
iconSizeCls XLarge   = "ibutton_xlarge"
iconSizeCls XXLarge  = "ibutton_xxlarge"
iconSizeCls XXXLarge = "ibutton_xxxlarge"


-- * TH instances

deriveClasses
  [ ([''IbuttonState], allClass)
  , ([''IbuttonProps], [''Lens'])
  ]
