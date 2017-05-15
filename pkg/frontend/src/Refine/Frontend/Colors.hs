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

data Color =
    DisabledText
  | VDocComment
  | VDocNote
  | VDocQuestion
  | VDocDiscussion
  | White
  | OverlayBackdrop

instance IsStyle Color where
  mkStyle c DisabledText    = StyleST c "rgb(169,169,169)" -- dark grey
  mkStyle c VDocComment     = StyleST c "rgb(219, 204, 221)" -- lightred
  mkStyle c VDocNote        = StyleST c "rgb(219, 204, 221)" -- lightred
  mkStyle c VDocQuestion    = StyleST c "rgb(220, 229, 211)" -- lightgreen
  mkStyle c VDocDiscussion  = StyleST c "rgb(215, 233, 255)" -- lightblue
  mkStyle c White           = StyleST c "rgb(255, 255, 255)"
  mkStyle c OverlayBackdrop = StyleST c "rgba(255, 255, 255, .8)"


-- FUTUREWORK: make two types: one that describes the color, and one that describes its meaning,
-- whose values are associated with values of the former.
