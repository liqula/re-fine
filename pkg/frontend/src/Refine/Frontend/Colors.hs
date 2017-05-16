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
import Refine.Frontend.Style


data RGB =
    RGB Int Int Int
  | RGBA JSString Double
  deriving (Eq, Show)

instance ConvertibleStrings RGB JSString where
  convertString (RGB r g b) = "rgb(" <> cs (show r) <> ", " <> cs (show g) <> ", " <> cs (show b) <> ")"
  convertString (RGBA s a)  = "rgba(" <> s <> ", " <> cs (show a) <> ")"


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

instance ConvertibleStrings SimpleColor JSString where
  -- main color scheme blue
  convertString SCBlue01       = cs $ RGB 32 49 67
  convertString SCBlue02       = cs $ RGB 59 67 87
  convertString SCBlue03       = cs $ RGB 84 99 122
  convertString SCBlue04       = cs $ RGB 100 126 149
  convertString SCBlue05       = cs $ RGB 179 188 199
  convertString SCBlue06       = cs $ RGB 210 217 223
  convertString SCBlue07       = cs $ RGB 234 236 239
  convertString SCBlue08       = cs $ RGB 246 247 249

  -- signal color
  convertString SCSignalYellow = cs $ RGB 224 255 18
  convertString SCSignalOrange = cs $ RGB 255 89 0

  -- voting and ranking
  convertString SCRed01        = cs $ RGB 200 55 20
  convertString SCRed02        = cs $ RGB 226 155 140
  convertString SCGreen01      = cs $ RGB 120 200 50
  convertString SCGreen02      = cs $ RGB 188 226 155
  convertString SCOrange01     = cs $ RGB 231 181 0
  convertString SCOrange02     = cs $ RGB 239 213 129

  -- comment
  convertString SCLightYellow  = cs $ RGB 237 237 192
  convertString SCLightGreen   = cs $ RGB 220 229 211
  convertString SCLightRed     = cs $ RGB 219 204 221
  convertString SCLightBlue    = cs $ RGB 215 233 255

  -- helpers
  convertString SCWhite        = cs $ RGB 255 255 255
  convertString SCBlack        = cs $ RGB 0 0 0
  convertString SCDarkGrey     = cs $ RGB 169 169 169


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

instance ConvertibleStrings Color JSString where
  -- typography
  convertString TextColor = cs SCBlack
  convertString DisabledTextColor = cs SCDarkGrey
  convertString HeadlineColor = cs SCBlue01
  convertString LinkColor = cs SCBlue04
  convertString ActiveColor = cs SCSignalOrange

  -- mainmenu
  convertString MainmenuBg = cs SCBlue01
  convertString MainmenuMainbuttonColor = cs SCBlue06
  convertString MainmenuMainbuttonCombinedColor = cs SCBlue02
  convertString MainmenuContentBg = cs SCBlue05
  convertString MainmenuContentColor = cs SCBlue06

  -- footer
  convertString FooterBg = cs MainmenuBg
  convertString FooterContentColor = cs SCBlue06

  -- vdoc header
  convertString VDocHeaderBg = cs SCBlue07
  convertString VDocToolbarBg = cs SCBlue06
  convertString VDocToolbarExtensionBg = cs SCBlue04

  -- icons
  convertString DefaultIconBg = cs SCBlue02
  convertString FallbackAvatarBg = cs SCBlue01
  convertString IconRollover = cs SCSignalOrange
  convertString IconDark = cs SCBlue01
  convertString IconBright = cs SCBlue06
  convertString IconBgActive = cs SCSignalYellow

  -- comments & edits etc.
  convertString VDocComment = cs SCLightRed
  convertString VDocCommentMark = cs $ RGBA (cs VDocComment) 0.5
  convertString VDocCommentRo = cs SCSignalOrange

  convertString VDocNote = cs SCLightYellow
  convertString VDocNoteMark = cs $ RGBA (cs VDocNote) 00.5
  convertString VDocNoteRo = cs SCSignalOrange

  convertString VDocQuestion = cs SCLightGreen
  convertString VDocQuestionMark = cs $ RGBA (cs VDocQuestion) 0.5
  convertString VDocQuestionRo = cs SCSignalOrange

  convertString VDocDiscussion = cs SCLightBlue
  convertString VDocDiscussionMark = cs $ RGBA (cs VDocDiscussion) 0.8
  convertString VDocDiscussionRo = cs SCSignalOrange

  convertString VDocEdit = cs SCBlue06
  convertString VDocEditMark = cs $ RGBA (cs VDocEdit) 0.7
  convertString VDocEditRo = cs SCSignalYellow

  convertString VDocEditByme = cs SCSignalYellow
  convertString VDocEditBymeMark = cs $ RGBA (cs VDocEditByme) 0.5
  convertString VDocEditBymeRo = cs SCSignalOrange

  -- maybe we get away with a single ro color ...
  convertString VDocRollover = cs SCSignalOrange

  -- overlay elements
  convertString OverlayContent = cs TextColor
  convertString OverlayMeta = cs SCBlue03
  convertString OverlayBackdrop = cs $ RGBA (cs SCWhite) 0.8


instance IsStyle Color where
  mkStyle c = StyleST c . cs

instance ConvertibleStrings Color ST where
  convertString = cs @JSString @ST . cs @Color @JSString
