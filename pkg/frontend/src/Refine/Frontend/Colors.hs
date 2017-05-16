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

module Refine.Frontend.Colors where

import Refine.Frontend.Prelude
import Refine.Frontend.Style

{-
//comment types
$lightyellow: rgb(237, 237, 192); //used for PRIVATE NOTE (?)
$lightgreen: rgb(220, 229, 211); //used for QUESTION
$lightred: rgb(219, 204, 221); //used for COMMENT (!)
$lightblue: rgb(215, 233, 255); //used for DISCUSSION
-}

-- | FIXME: (1) translate scss/0-settings/_settings.colors.scss into this type.  (2) make two types:
-- one that describes the color, and one that describes its meaning, whose values are associated
-- with values of the former.
data Color =
    DisabledText
  | VDocComment
  | VDocNote
  | VDocQuestion
  | VDocDiscussion
  | VDocRollover
  | White
  | OverlayBackdrop

instance ConvertibleStrings Color ST where
  convertString DisabledText    = "rgb(169, 169, 169)" -- dark grey
  convertString VDocComment     = "rgb(219, 204, 221)" -- lightred
  convertString VDocNote        = "rgb(219, 204, 221)" -- lightred
  convertString VDocQuestion    = "rgb(220, 229, 211)" -- lightgreen
  convertString VDocDiscussion  = "rgb(215, 233, 255)" -- lightblue
  convertString VDocRollover    = "rgb(255, 89, 0)"    -- $signal-orange
  convertString White           = "rgb(255, 255, 255)"
  convertString OverlayBackdrop = "rgba(255, 255, 255, .8)"


instance IsStyle Color where
  mkStyle c = StyleST c . cs
