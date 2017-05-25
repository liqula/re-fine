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
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Login.Status where

import Refine.Frontend.Prelude

import Refine.Frontend.Icon
import Refine.Frontend.Login.Types
import Refine.Frontend.MainMenu.Types
import Refine.Frontend.Store.Types


loginStatusButton_ :: Bool -> CurrentUser -> ReactElementM handler ()
loginStatusButton_ darkBackground cu = ibutton_ $ emptyIbuttonProps "Login" onclick
  & ibDarkBackground .~ darkBackground
  & ibLabel .~ mkLabel cu
  & ibSize .~ XXLarge
  & ibAlign .~ AlignRight
  where
    onclick = [MainMenuAction $ MainMenuActionOpen (MainMenuLogin MainMenuSubTabLogin)]

    mkLabel UserLoggedOut    = "login"
    mkLabel (UserLoggedIn n) = "I am " <> n
