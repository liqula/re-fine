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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Colors where

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

disabledText :: String
disabledText = "rgb(169,169,169)" -- dark grey

vdoc_comment :: String
vdoc_comment = "rgb(219, 204, 221)" -- lightred

vdoc_discussion :: String
vdoc_discussion = "rgb(215, 233, 255)" -- lightblue

vdoc_question :: String
vdoc_question = "rgb(220, 229, 211)" -- lightgreen

{-
//comment types
$lightyellow: rgb(237, 237, 192); //used for PRIVATE NOTE (?)
$lightgreen: rgb(220, 229, 211); //used for QUESTION
$lightred: rgb(219, 204, 221); //used for COMMENT (!)
$lightblue: rgb(215, 233, 255); //used for DISCUSSION
-}
