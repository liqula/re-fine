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

import qualified Language.Css.Build as Css
import qualified Language.Css.Pretty as Css
import qualified Language.Css.Syntax as Css

data RGB =
    RGB Int Int Int
  | RGBA Css.Expr Double
  deriving (Eq, Show)

instance Css.ToExpr RGB where
  expr (RGB r g b)  = Css.expr (Css.Crgb r g b)
  expr (RGBA color a) = Css.expr (Css.VString ("rgba(" <> Css.prettyPrint color <> ", " <> show a <> ")"))


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
  -- main color scheme blue
  expr SCBlue01       = Css.expr $ RGB 32 49 67
  expr SCBlue02       = Css.expr $ RGB 59 67 87
  expr SCBlue03       = Css.expr $ RGB 84 99 122
  expr SCBlue04       = Css.expr $ RGB 100 126 149
  expr SCBlue05       = Css.expr $ RGB 179 188 199
  expr SCBlue06       = Css.expr $ RGB 210 217 223
  expr SCBlue07       = Css.expr $ RGB 234 236 239
  expr SCBlue08       = Css.expr $ RGB 246 247 249

  -- signal color
  expr SCSignalYellow = Css.expr $ RGB 224 255 18
  expr SCSignalOrange = Css.expr $ RGB 255 89 0

  -- voting and ranking
  expr SCRed01        = Css.expr $ RGB 200 55 20
  expr SCRed02        = Css.expr $ RGB 226 155 140
  expr SCGreen01      = Css.expr $ RGB 120 200 50
  expr SCGreen02      = Css.expr $ RGB 188 226 155
  expr SCOrange01     = Css.expr $ RGB 231 181 0
  expr SCOrange02     = Css.expr $ RGB 239 213 129

  -- comment
  expr SCLightYellow  = Css.expr $ RGB 237 237 192
  expr SCLightGreen   = Css.expr $ RGB 220 229 211
  expr SCLightRed     = Css.expr $ RGB 219 204 221
  expr SCLightBlue    = Css.expr $ RGB 215 233 255

  -- helpers
  expr SCWhite        = Css.expr $ RGB 255 255 255
  expr SCBlack        = Css.expr $ RGB 0 0 0
  expr SCDarkGrey     = Css.expr $ RGB 169 169 169


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
  -- typography
  expr TextColor = Css.expr SCBlack
  expr DisabledTextColor = Css.expr SCDarkGrey
  expr HeadlineColor = Css.expr SCBlue01
  expr LinkColor = Css.expr SCBlue04
  expr ActiveColor = Css.expr SCSignalOrange

  -- mainmenu
  expr MainmenuBg = Css.expr SCBlue01
  expr MainmenuMainbuttonColor = Css.expr SCBlue06
  expr MainmenuMainbuttonCombinedColor = Css.expr SCBlue02
  expr MainmenuContentBg = Css.expr SCBlue05
  expr MainmenuContentColor = Css.expr SCBlue06

  -- footer
  expr FooterBg = Css.expr MainmenuBg
  expr FooterContentColor = Css.expr SCBlue06

  -- vdoc header
  expr VDocHeaderBg = Css.expr SCBlue07
  expr VDocToolbarBg = Css.expr SCBlue06
  expr VDocToolbarExtensionBg = Css.expr SCBlue04

  -- icons
  expr DefaultIconBg = Css.expr SCBlue02
  expr FallbackAvatarBg = Css.expr SCBlue01
  expr IconRollover = Css.expr SCSignalOrange
  expr IconDark = Css.expr SCBlue01
  expr IconBright = Css.expr SCBlue06
  expr IconBgActive = Css.expr SCSignalYellow

  -- comments & edits etc.
  expr VDocComment = Css.expr SCLightRed
  expr VDocCommentMark = Css.expr $ RGBA (Css.expr VDocComment) 0.5
  expr VDocCommentRo = Css.expr SCSignalOrange

  expr VDocNote = Css.expr SCLightYellow
  expr VDocNoteMark = Css.expr $ RGBA (Css.expr VDocNote) 00.5
  expr VDocNoteRo = Css.expr SCSignalOrange

  expr VDocQuestion = Css.expr SCLightGreen
  expr VDocQuestionMark = Css.expr $ RGBA (Css.expr VDocQuestion) 0.5
  expr VDocQuestionRo = Css.expr SCSignalOrange

  expr VDocDiscussion = Css.expr SCLightBlue
  expr VDocDiscussionMark = Css.expr $ RGBA (Css.expr VDocDiscussion) 0.8
  expr VDocDiscussionRo = Css.expr SCSignalOrange

  expr VDocEdit = Css.expr SCBlue06
  expr VDocEditMark = Css.expr $ RGBA (Css.expr VDocEdit) 0.7
  expr VDocEditRo = Css.expr SCSignalYellow

  expr VDocEditByme = Css.expr SCSignalYellow
  expr VDocEditBymeMark = Css.expr $ RGBA (Css.expr VDocEditByme) 0.5
  expr VDocEditBymeRo = Css.expr SCSignalOrange

  -- maybe we get away with a single ro color ...
  expr VDocRollover = Css.expr SCSignalOrange

  -- overlay elements
  expr OverlayContent = Css.expr TextColor
  expr OverlayMeta = Css.expr SCBlue03
  expr OverlayBackdrop = Css.expr $ RGBA (Css.expr SCWhite) 0.8
