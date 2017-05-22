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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | translation of @.../scss/0-settings/_settings.colors.scss@.  (keep in sync, or remove the scss one!)
module Refine.Frontend.Colors where

import Refine.Frontend.Prelude

import Data.List (intercalate)

import qualified Language.Css.Build as Css
import qualified Language.Css.Syntax as Css


data RGBA = RGBA Int Int Int Double
  deriving (Eq, Show)

instance Css.ToExpr RGBA where
  expr (RGBA r g b a) = Css.expr $ Css.Ident ("rgba(" <> intercalate ", " [show r, show g, show b, show a] <> ")")

rgb :: Int -> Int -> Int -> RGBA
rgb r g b = RGBA r g b 1

opacity :: ToRGBA a => a -> Double -> RGBA
opacity (toRGBA -> RGBA r g b a) a' = RGBA r g b (a * a')


class ToRGBA a where
  toRGBA :: a -> RGBA


data SimpleColor =
    -- main color scheme blue
    SCBlue01
  | SCBlue02
  | SCBlue03
  | SCBlue04
  | SCBlue05
  | SCBlue06
  | SCBlue07
  | SCBlue08

    -- signal color
  | SCSignalYellow
  | SCSignalOrange

    -- voting and ranking
  | SCRed01
  | SCRed02
  | SCGreen01
  | SCGreen02
  | SCOrange01
  | SCOrange02

    -- comment
  | SCLightYellow  -- for PRIVATE NOTE (?)
  | SCLightGreen   -- for QUESTION
  | SCLightRed     -- for COMMENT (!)
  | SCLightBlue    -- for DISCUSSION

    -- helpers
  | SCWhite
  | SCBlack
  | SCDarkGrey
  deriving (Eq, Show)

instance Css.ToExpr SimpleColor where
  expr = Css.expr . toRGBA

instance ToRGBA SimpleColor where
  -- main color scheme blue
  toRGBA SCBlue01       = RGBA 32 49 67 1
  toRGBA SCBlue02       = RGBA 59 67 87 1
  toRGBA SCBlue03       = RGBA 84 99 122 1
  toRGBA SCBlue04       = RGBA 100 126 149 1
  toRGBA SCBlue05       = RGBA 179 188 199 1
  toRGBA SCBlue06       = RGBA 210 217 223 1
  toRGBA SCBlue07       = RGBA 234 236 239 1
  toRGBA SCBlue08       = RGBA 246 247 249 1

  -- signal color
  toRGBA SCSignalYellow = RGBA 224 255 18 1
  toRGBA SCSignalOrange = RGBA 255 89 0 1

  -- voting and ranking
  toRGBA SCRed01        = RGBA 200 55 20 1
  toRGBA SCRed02        = RGBA 226 155 140 1
  toRGBA SCGreen01      = RGBA 120 200 50 1
  toRGBA SCGreen02      = RGBA 188 226 155 1
  toRGBA SCOrange01     = RGBA 231 181 0 1
  toRGBA SCOrange02     = RGBA 239 213 129 1

  -- comment
  toRGBA SCLightYellow  = RGBA 237 237 192 1
  toRGBA SCLightGreen   = RGBA 220 229 211 1
  toRGBA SCLightRed     = RGBA 219 204 221 1
  toRGBA SCLightBlue    = RGBA 215 233 255 1

  -- helpers
  toRGBA SCWhite        = RGBA 255 255 255 1
  toRGBA SCBlack        = RGBA 0 0 0 1
  toRGBA SCDarkGrey     = RGBA 169 169 169 1


data Color =
    -- typography
    TextColor
  | DisabledTextColor
  | HeadlineColor
  | LinkColor
  | ActiveColor

    -- mainmenu
  | MainmenuBg
  | MainmenuMainbuttonColor
  | MainmenuMainbuttonCombinedColor
  | MainmenuContentBg
  | MainmenuContentColor

    -- footer
  | FooterBg
  | FooterContentColor

    -- vdoc header
  | VDocHeaderBg
  | VDocToolbarBg
  | VDocToolbarExtensionBg

    -- icons
  | DefaultIconBg
  | FallbackAvatarBg
  | IconRollover
  | IconDark
  | IconBright
  | IconBgActive

    -- comments & edits etc.
  | VDocComment
  | VDocCommentMark
  | VDocCommentRo

  | VDocNote
  | VDocNoteMark
  | VDocNoteRo

  | VDocQuestion
  | VDocQuestionMark
  | VDocQuestionRo

  | VDocDiscussion
  | VDocDiscussionMark
  | VDocDiscussionRo

  | VDocEdit
  | VDocEditMark
  | VDocEditRo

  | VDocEditByme
  | VDocEditBymeMark
  | VDocEditBymeRo

    -- maybe we get away with a single ro color ...
  | VDocRollover

    -- overlay elements
  | OverlayContent
  | OverlayMeta
  | OverlayBackdrop
  deriving (Eq, Show)

instance Css.ToExpr Color where
  expr = Css.expr . toRGBA

instance ToRGBA Color where
  -- typography
  toRGBA TextColor = toRGBA SCBlack
  toRGBA DisabledTextColor = toRGBA SCDarkGrey
  toRGBA HeadlineColor = toRGBA SCBlue01
  toRGBA LinkColor = toRGBA SCBlue04
  toRGBA ActiveColor = toRGBA SCSignalOrange

  -- mainmenu
  toRGBA MainmenuBg = toRGBA SCBlue01
  toRGBA MainmenuMainbuttonColor = toRGBA SCBlue06
  toRGBA MainmenuMainbuttonCombinedColor = toRGBA SCBlue02
  toRGBA MainmenuContentBg = toRGBA SCBlue05
  toRGBA MainmenuContentColor = toRGBA SCBlue06

  -- footer
  toRGBA FooterBg = toRGBA MainmenuBg
  toRGBA FooterContentColor = toRGBA SCBlue06

  -- vdoc header
  toRGBA VDocHeaderBg = toRGBA SCBlue07
  toRGBA VDocToolbarBg = toRGBA SCBlue06
  toRGBA VDocToolbarExtensionBg = toRGBA SCBlue04

  -- icons
  toRGBA DefaultIconBg = toRGBA SCBlue02
  toRGBA FallbackAvatarBg = toRGBA SCBlue01
  toRGBA IconRollover = toRGBA SCSignalOrange
  toRGBA IconDark = toRGBA SCBlue01
  toRGBA IconBright = toRGBA SCBlue06
  toRGBA IconBgActive = toRGBA SCSignalYellow

  -- comments & edits etc.
  toRGBA VDocComment = toRGBA SCLightRed
  toRGBA VDocCommentMark = opacity VDocComment 0.5
  toRGBA VDocCommentRo = toRGBA SCSignalOrange

  toRGBA VDocNote = toRGBA SCLightYellow
  toRGBA VDocNoteMark = opacity VDocNote 0.5
  toRGBA VDocNoteRo = toRGBA SCSignalOrange

  toRGBA VDocQuestion = toRGBA SCLightGreen
  toRGBA VDocQuestionMark = opacity VDocQuestion 0.5
  toRGBA VDocQuestionRo = toRGBA SCSignalOrange

  toRGBA VDocDiscussion = toRGBA SCLightBlue
  toRGBA VDocDiscussionMark = opacity VDocDiscussion 0.8
  toRGBA VDocDiscussionRo = toRGBA SCSignalOrange

  toRGBA VDocEdit = toRGBA SCBlue06
  toRGBA VDocEditMark = opacity VDocEdit 0.7
  toRGBA VDocEditRo = toRGBA SCSignalYellow

  toRGBA VDocEditByme = toRGBA SCSignalYellow
  toRGBA VDocEditBymeMark = opacity VDocEditByme 0.5
  toRGBA VDocEditBymeRo = toRGBA SCSignalOrange

  -- maybe we get away with a single ro color ...
  toRGBA VDocRollover = toRGBA SCSignalOrange

  -- overlay elements
  toRGBA OverlayContent = toRGBA TextColor
  toRGBA OverlayMeta = toRGBA SCBlue03
  toRGBA OverlayBackdrop = opacity SCWhite 0.8
