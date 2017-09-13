{-# LANGUAGE CPP #-}
#include "language_common.hs"
module Refine.Common.Color
  ( Color(..), ColorAttr(..), colorValue, colorName
  , RGBA(..), showRGBA
  ) where
#include "import_common.hs"

import Data.Char
import Data.List


data Color
  = MainMenuBackground
  | MainMenuDarkBlue
  | MainMenuBlue
  | MainMenuIcon
  | ToolbarIcon
  | InteractionYellowNeon
  | InteractionOrange
  | InteractionOrangeNeon
  | InteractionRed
  | FormGreen
  | FormOrangeLight
  | FormOrangeMedium
  | FormOrangeStrong
  | FormBackground
  | FormIcon
  | NoteDark
  | NoteBubble
  | NoteBackground
  | DiscussionDark
  | DiscussionBubble
  | DiscussionBackground
  | EditDark
  | EditBubble
  | EditBackground
  | VoteNo
  | VoteYes
  deriving (Eq, Show, Ord, Bounded, Enum)

colorValue :: Color -> RGBA
colorValue MainMenuBackground    = RGBA 179 190 200 1
colorValue MainMenuDarkBlue      = RGBA 36 48 65 1
colorValue MainMenuBlue          = RGBA 141 157 173 1
colorValue MainMenuIcon          = RGBA 210 217 221 1
colorValue ToolbarIcon           = RGBA 104 125 146 1
colorValue InteractionYellowNeon = RGBA 231 252 85 1
colorValue InteractionOrange     = RGBA 238 114 54 1
colorValue InteractionOrangeNeon = RGBA 237 100 43 1
colorValue InteractionRed        = RGBA 212 68 53 1
colorValue FormGreen             = RGBA 139 196 77 1
colorValue FormOrangeLight       = RGBA 249 215 194 1
colorValue FormOrangeMedium      = RGBA 232 119 72 1
colorValue FormOrangeStrong      = RGBA 237 100 43 1
colorValue FormBackground        = RGBA 179 190 200 1
colorValue FormIcon              = RGBA 28 40 51 1
colorValue NoteDark              = RGBA 169 153 61 1
colorValue NoteBubble            = RGBA 223 213 167 1
colorValue NoteBackground        = RGBA 242 235 210 1
colorValue DiscussionDark        = RGBA 121 129 63 1
colorValue DiscussionBubble      = RGBA 198 198 163 1
colorValue DiscussionBackground  = RGBA 229 228 208 1
colorValue EditDark              = RGBA 156 89 42 1
colorValue EditBubble            = RGBA 224 199 179 1
colorValue EditBackground        = RGBA 242 231 222 1
colorValue VoteNo                = RGBA 221 73 43 1
colorValue VoteYes               = RGBA 139 196 77 1


data ColorAttr
  = ColorAttrDefault
  | ColorAttrBackgroundColor
  | ColorAttrFill
  deriving (Eq, Ord, Bounded, Enum, Show)


colorName :: ColorAttr -> Color -> ST
colorName attr = (("c_" <> showAttr attr) <>) . camelToUnderscore . cs . show
  where
    showAttr ColorAttrDefault         = ""
    showAttr ColorAttrBackgroundColor = "bg_"
    showAttr ColorAttrFill            = "fill_"

camelToUnderscore :: ST -> ST
camelToUnderscore = cs . go True . cs
  where
    go :: Bool -> String -> String
    go _ "" = ""
    go isfirst (x:xs)
      | isUpper x && isfirst = toLower x       : go False xs
      | isUpper x            = '_' : toLower x : go False xs
      | otherwise            = x               : go False xs


data RGBA = RGBA Int Int Int Double
  deriving (Eq, Show)

showRGBA :: RGBA -> ST
showRGBA (RGBA r g b a) = cs $ "rgba(" <> intercalate ", " [show r, show g, show b, show a] <> ")"

_rgbFromHex :: HasCallStack => ST -> RGBA
_rgbFromHex num = f $ toLower <$> cs num
  where
    f ('#' : xs) = f xs
    f [r1, r2, g1, g2, b1, b2] = RGBA (h r1 r2) (h g1 g2) (h b1 b2) 1
    f _   = error $ "rgbFromHex: bad input: " <> show num
    h x1 x2 = 16 * q x1 + q x2
    q '0' = 0
    q '1' = 1
    q '2' = 2
    q '3' = 3
    q '4' = 4
    q '5' = 5
    q '6' = 6
    q '7' = 7
    q '8' = 8
    q '9' = 9
    q 'a' = 10
    q 'b' = 11
    q 'c' = 12
    q 'd' = 13
    q 'e' = 14
    q 'f' = 15
    q _   = error $ "rgbFromHex: bad input: " <> show num
