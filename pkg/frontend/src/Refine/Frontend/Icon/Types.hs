{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Icon.Types
  ( ReactListKey
  , Align(..)
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
  , ibSize
  , ibAlign
  , IbuttonState(..)
  , ibuttonMouseOver
  , ibuttonState

  , IbuttonOnClick(..)

  , IconSize(..)
  , sizePx, sizeInt

  , IconDescription

  , IconProps(..)
  , iconPropsBlockName
  , iconPropsHighlight
  , iconPropsDesc
  , iconPropsSize

  , IconButtonPropsWithHandler(..)
  , iconButtonPropsListKey
  , iconButtonPropsIconProps
  , iconButtonPropsElementName
  , iconButtonPropsModuleName
  , iconButtonPropsLabel
  , iconButtonPropsDisabled
  , iconButtonPropsPosition
  , iconButtonPropsAlignRight
  , iconButtonPropsOnClick
  , iconButtonPropsOnClickMods
  , iconButtonPropsExtraClasses
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

data Align = AlignRight | AlignLeft
  deriving (Eq, Show, Generic)

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
  , _ibSize             :: IconSize
  , _ibAlign            :: Align
  }
  deriving (Eq, Show, Generic)

data IbuttonState st = IbuttonState { _ibuttonMouseOver :: Bool, _ibuttonState :: st }
  deriving (Eq, Ord, Generic)

class (Typeable onclick, Eq onclick) => IbuttonOnClick onclick handler where
  runIbuttonOnClick :: Event -> MouseEvent -> onclick -> EventHandlerType handler


-- * icon sizes

data IconSize  -- FIXME: eliminate duplication between this and the corresponding scss variables.
  = Medium
  | Large
  | XLarge
  | XXLarge
  deriving (Eq, Show)

sizePx :: HasCallStack => IconSize -> Px
sizePx = Px . sizeInt

sizeInt :: HasCallStack => IconSize -> Int
sizeInt Medium  = 14
sizeInt Large   = 20
sizeInt XLarge  = 26
sizeInt XXLarge = 32

instance Css IconSize where
  css s = [ decl "backgroundSize" (Percentage 100)
          , decl "width" (sizePx s)
          , decl "height" (sizePx s)
          ]


-- * outdated

-- FUTUREWORK: the rest of this module should be removed and everything ported to the ibutton_
-- component above.  this may take a few more steps, though.


-- ** icon

data IconProps = IconProps
  { _iconPropsBlockName :: JSString
  , _iconPropsHighlight :: Bool
  , _iconPropsDesc      :: IconDescription
  , _iconPropsSize      :: IconSize
  }
  deriving (Eq)

type IconDescription = (JSString, JSString)

instance Default IconProps where
  def = IconProps
    { _iconPropsBlockName = ""
    , _iconPropsHighlight = False
    , _iconPropsDesc      = ("", "")
    , _iconPropsSize      = Large
    }


-- ** icon button

data IconButtonPropsWithHandler onclick = IconButtonProps
  { _iconButtonPropsListKey      :: ReactListKey  -- (this is not morally part of the props, but it's convenient to keep it here.)
  , _iconButtonPropsIconProps    :: IconProps
  , _iconButtonPropsElementName  :: JSString
  , _iconButtonPropsModuleName   :: JSString
  , _iconButtonPropsLabel        :: JSString
  , _iconButtonPropsDisabled     :: Bool  -- FIXME: make this 'enabled'
  , _iconButtonPropsPosition     :: Maybe Int
  , _iconButtonPropsAlignRight   :: Bool
  , _iconButtonPropsOnClick      :: onclick
  , _iconButtonPropsOnClickMods  :: [EventModification]
  , _iconButtonPropsExtraClasses :: [JSString]
  }
  deriving (Eq)


-- * TH instances

deriveClasses
  [ ([''IbuttonState], allClass)
  , ([''IconProps, ''IbuttonProps, ''IconButtonPropsWithHandler], [''Lens'])
  ]
