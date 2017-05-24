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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Frontend.Header.UserLoginLogout where

import Refine.Frontend.Prelude

import           Refine.Frontend.Login.Types
import           Refine.Frontend.Icon
import           Refine.Frontend.Icon.Types
import           Refine.Prelude()


instance UnoverlapAllEq CurrentUser

userLoginLogoutButton :: View '[CurrentUser]
userLoginLogoutButton = mkView "UserLoginLogoutButton" $ \case
  UserLoggedOut           -> iconButton_ loginLogoutProps { _iconButtonPropsLabel = "login" }
  (UserLoggedIn username) -> iconButton_ loginLogoutProps { _iconButtonPropsLabel = cs username }

userLoginLogoutButton_ :: CurrentUser -> ReactElementM eventHandler ()
userLoginLogoutButton_ !currentUser = view_ userLoginLogoutButton "userLoginLogoutButton_" currentUser

loginLogoutProps :: IconButtonProps
loginLogoutProps = defaultIconButtonProps
  & iconButtonPropsListKey .~ "logInOut"
  & iconButtonPropsIconProps .~ (def
      & iconPropsBlockName .~ "c-mainmenu-content"
      & iconPropsDesc      .~ ("icon-Login", "bright")
      & iconPropsSize      .~ XXL
      )
  & iconButtonPropsElementName  .~ "section-button"
  & iconButtonPropsExtraClasses .~ ["c-mainmenu-content__btn-help"]
  -- not translated from prototype2016:
  -- button attribute data-section="help"
