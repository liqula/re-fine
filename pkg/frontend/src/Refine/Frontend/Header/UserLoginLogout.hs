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

module Refine.Frontend.Header.UserLoginLogout where

import           Data.String.Conversions (cs)
import           React.Flux

import           Refine.Frontend.Login.Types
import           Refine.Frontend.UtilityWidgets
import           Refine.Prelude()



userLoginLogoutButton :: ReactView CurrentUser
userLoginLogoutButton = defineView "UserLoginLogoutButton" $ \currentUser ->
  case currentUser of
    UserLoggedOut         -> iconButton_ loginLogoutProps { _iconButtonPropsLabel = "Log In" }
    UserLoggedIn username -> iconButton_ loginLogoutProps { _iconButtonPropsLabel = cs username }


loginLogoutProps :: IconButtonProps
loginLogoutProps = IconButtonProps
  { _iconButtonPropsIconProps = IconProps
     { _iconPropsBlockName = "c-mainmenu-content"
     , _iconPropsHighlight = False
     , _iconPropsDesc      = ("icon-Exit", "dark")
     , _iconPropsSize      = XXL
     }
  , _iconButtonPropsElementName  = "section-button"
  , _iconButtonPropsModuleName   = ""
  , _iconButtonPropsContentType  = ""
  , _iconButtonPropsLabel        = ""
  , _iconButtonPropsDisabled     = False
  , _iconButtonPropsClickHandler = \_ -> []
  , _iconButtonPropsExtraClasses = ["c-mainmenu-content__btn-help"]
  -- not translated from prototype2016:
  -- button attribute data-section="help"
  }

userLoginLogoutButton_ :: CurrentUser -> ReactElementM eventHandler ()
userLoginLogoutButton_ currentUser = view userLoginLogoutButton currentUser mempty

