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

module Refine.Frontend.Login.Component where

import Refine.Frontend.Prelude

import           Control.Lens (ASetter)
import qualified Data.Text as ST
import           Language.Css.Syntax

import           Refine.Common.Types.Prelude
import           Refine.Frontend.Colors as Color
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Types
import qualified Refine.Frontend.Store.Types as RS
import           Refine.Frontend.Util


-- * Helper

inputFieldStyles :: HasCallStack => [Decl]
inputFieldStyles =
  [ decl "borderRadius" (Px 5)
  , decl "margin" (Px 12)
  , decl "backgroundColor" Color.SCBlue08
  ]

inputFieldWithKey
  :: (FromJSVal c)
  => JSString -> JSString -> JSString -> JSString -> ASetter s s b c
  -> ReactElementM ('StatefulEventHandlerCode s) ()
inputFieldWithKey fieldId fieldType fieldPlaceholder fieldKey lens =
  input_ [ "id" $= fieldId
         , "style" @@= inputFieldStyles
         , "type" $= fieldType
         , "placeholder" $= fieldPlaceholder
         , onChange $ \evt -> simpleHandler $ \st -> ([], Just (st & lens .~ target evt fieldKey))
         ]

inputField
  :: (FromJSVal c)
  => JSString -> JSString -> JSString -> ASetter s s b c
  -> ReactElementM ('StatefulEventHandlerCode s) ()
inputField i t p = inputFieldWithKey i t p "value"


-- * Form types

data LoginForm = LoginForm
  { _loginFormUsername :: ST
  , _loginFormPassword :: ST
  } deriving (Eq, Generic, Show)

data RegistrationForm = RegistrationForm
  { _registrationFormUsername  :: ST
  , _registrationFormEmail1    :: ST
  , _registrationFormEmail2    :: ST
  , _registrationFormPassword  :: ST
  , _registrationFormAgree     :: Bool
  } deriving (Eq, Generic, Show)

makeRefineTypes [''LoginForm, ''RegistrationForm]

validateLoginForm :: HasCallStack => LoginForm -> [ST]
validateLoginForm form =
  [ "no user name" | form ^. loginFormUsername . to ST.null ] <>
  [ "no password" | form ^. loginFormPassword . to ST.null ]

validateRegistrationForm :: HasCallStack => RegistrationForm -> [ST]
validateRegistrationForm form =
  [ "email addresses do not match" | form ^. registrationFormEmail1 /= form ^. registrationFormEmail2 ] <>
  [ "no user name" | form ^. registrationFormUsername . to ST.null ] <>
  [ "no email address" | form ^. registrationFormEmail1 . to ST.null ] <>
  [ "no password" | form ^. registrationFormPassword . to ST.null ] <>
  [ "please agree to the terms & conditions" | form ^. registrationFormAgree . to not ]

loginOrLogout_ :: HasCallStack => CurrentUser -> Maybe ST -> ReactElementM eventHandler ()
loginOrLogout_ = \case
  UserLoggedOut  -> login_
  UserLoggedIn _ -> const logout_

defaultStyles :: HasCallStack => [Decl]
defaultStyles = []


-- * Login

loginStyles :: HasCallStack => [Decl]
loginStyles = defaultStyles

login :: HasCallStack => Maybe ST -> View '[]
login errors = mkStatefulView "Login" (LoginForm "" "") $ \st@(LoginForm lname lpass) ->
  div_ ["style" @@= loginStyles] $ do
    h1_ "Login"

    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      let lerrors = validateLoginForm st
      (p_ . elemCS) `mapM_` lerrors
      (p_ . elemCS) `mapM_` errors

      inputField "login-username" "text"     "Username" loginFormUsername >> br_ []
      inputField "login-password" "password" "Password" loginFormPassword >> br_ []

      iconButton_ $ defaultIconButtonProps @[RS.GlobalAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" True ("icon-Login", "dark") Large
        & iconButtonPropsElementName  .~ "submit"
        & iconButtonPropsLabel        .~ "submit"
        & iconButtonPropsDisabled     .~ not (null lerrors)
        & iconButtonPropsOnClick      .~ [RS.Login $ Login lname lpass]

login_ :: HasCallStack => Maybe ST -> ReactElementM eventHandler ()
login_ errors = view_ (login errors) "login_"


-- * Logout

logoutStyles :: HasCallStack => [Decl]
logoutStyles = defaultStyles

logout :: HasCallStack => View '[]
logout = mkView "Logout" $ do
  div_ ["style" @@= logoutStyles] $ do
    p_ "Profile page"
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      iconButton_ $ defaultIconButtonProps @[RS.GlobalAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" True ("icon-Logout", "dark") Large
        & iconButtonPropsElementName  .~ "submit"
        & iconButtonPropsLabel        .~ "logout"
        & iconButtonPropsDisabled     .~ False
        & iconButtonPropsOnClick      .~ [RS.Logout]

logout_ :: HasCallStack => ReactElementM eventHandler ()
logout_ = view_ logout "logout_"


-- * Registration

registrationStyles :: HasCallStack => [Decl]
registrationStyles = defaultStyles

registration :: HasCallStack => Maybe ST -> View '[]
registration errors = mkStatefulView "Registration" (RegistrationForm "" "" "" "" False) $ \st -> do
  div_ ["style" @@= registrationStyles] $ do
    h1_ "Registration"

    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      let lerrors = validateRegistrationForm st
      (p_ . elemCS) `mapM_` lerrors
      (p_ . elemCS) `mapM_` errors

      inputField "registration-username"  "text"     "Username"    registrationFormUsername >> br_ []
      inputField "registration-email1"    "email"    "Email"       registrationFormEmail1   >> br_ []
      inputField "registration-email2"    "email"    "Email again" registrationFormEmail2   >> br_ []
      inputField "registration-password1" "password" "Password"    registrationFormPassword >> br_ []

      inputFieldWithKey "registration-agree" "checkbox" "" "checked" registrationFormAgree
      "I agree with the terms of use." >> br_ []

      iconButton_ $ defaultIconButtonProps @[RS.GlobalAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" True ("icon-Register_user", "dark") Large
        & iconButtonPropsElementName  .~ "submit"
        & iconButtonPropsLabel        .~ "submit"
        & iconButtonPropsDisabled     .~ not (null lerrors)
        & iconButtonPropsOnClick      .~ [RS.CreateUser
                                              . (CreateUser <$> _registrationFormUsername
                                                            <*> _registrationFormEmail1
                                                            <*> _registrationFormPassword)
                                              $ st]

registration_ :: HasCallStack => Maybe ST -> ReactElementM eventHandler ()
registration_ errors = view_ (registration errors) "registration_"
