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
import           Refine.Frontend.Icon
import           Refine.Frontend.Login.Types
import qualified Refine.Frontend.Store.Types as RS
import           Refine.Frontend.Util
import           Refine.Prelude.TH (makeRefineType)


-- * Helper

inputFieldWithKey
  :: (FromJSVal c)
  => JSString -> JSString -> JSString -> JSString -> ASetter s a b c
  -> ReactElementM (s -> ([t], Maybe a)) ()
inputFieldWithKey fieldId fieldType fieldPlaceholder fieldKey lens =
  input_ [ "id" $= fieldId
         , "type" $= fieldType
         , "placeholder" $= fieldPlaceholder
         , onChange $ \evt st -> ([], Just (st & lens .~ target evt fieldKey))
         ]

inputField
  :: (FromJSVal c)
  => JSString -> JSString -> JSString -> ASetter s a b c
  -> ReactElementM (s -> ([t], Maybe a)) ()
inputField i t p = inputFieldWithKey i t p "value"


-- * Form types

data LoginForm = LoginForm
  { _loginFormUsername :: ST
  , _loginFormPassword :: ST
  , _loginFormErrors   :: FormError
  } deriving (Eq, Generic, Show)

makeRefineType ''LoginForm

data RegistrationForm = RegistrationForm
  { _registrationFormUsername  :: ST
  , _registrationFormEmail1    :: ST
  , _registrationFormEmail2    :: ST
  , _registrationFormPassword  :: ST
  , _registrationFormAgree     :: Bool
  , _registrationFormErrors    :: FormError
  } deriving (Eq, Generic, Show)

makeRefineType ''RegistrationForm

-- | FIXME: I used the pattern "return a list of errors, and then check if that list is null to get
-- the boolean" in `createChunkRangeErrors`, and I quite liked it, as it gives you more informative
-- error messages when you need them.  See also: 'invalidRegistrationForm'.
invalidLoginForm :: LoginForm -> Bool
invalidLoginForm form = form ^. loginFormUsername . to ST.null || form ^. loginFormPassword . to ST.null

invalidRegistrationForm :: RegistrationForm -> Bool
invalidRegistrationForm form =
  or [ form ^. registrationFormEmail1 /= form ^. registrationFormEmail2
     , form ^. registrationFormUsername . to ST.null
     , form ^. registrationFormEmail1 . to ST.null
     , form ^. registrationFormPassword . to ST.null
     , form ^. registrationFormAgree . to not
     ]

loginOrLogout_ :: CurrentUser -> FormError -> ReactElementM eventHandler ()
loginOrLogout_ = \case
  UserLoggedOut  -> login_
  UserLoggedIn _ -> const logout_

defaultStyles :: [Decl]
defaultStyles = []


-- * Login

loginStyles :: [Decl]
loginStyles = defaultStyles

login :: FormError -> View '[]
login errors = mkStatefulView "Login" (LoginForm "" "" errors) $ \curState ->
  div_ ["style" @@= loginStyles] $ do
    h1_ "Login"

    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      mapM_ (p_ . elemCS)
        (curState ^. loginFormErrors)

      inputField "login-username" "text"     "Username" loginFormUsername >> br_ []
      inputField "login-password" "password" "Password" loginFormPassword >> br_ []

      iconButton_ $ defaultIconButtonProps @[RS.GlobalAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") Large
        & iconButtonPropsElementName  .~ "submit"
        & iconButtonPropsLabel        .~ "submit"
        & iconButtonPropsDisabled     .~ invalidLoginForm curState
        & iconButtonPropsOnClick      .~ [RS.Login . (Login <$> _loginFormUsername <*> _loginFormPassword) $ curState]

login_ :: FormError -> ReactElementM eventHandler ()
login_ !errors = view_ (login errors) "login_"


-- * Logout

logoutStyles :: [Decl]
logoutStyles = defaultStyles

logout :: View '[]
logout = mkView "Logout" $ do
  div_ ["style" @@= logoutStyles] $ do
    p_ "Profile page"
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      iconButton_ $ defaultIconButtonProps @[RS.GlobalAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") Large
        & iconButtonPropsElementName  .~ "submit"
        & iconButtonPropsLabel        .~ "logout"
        & iconButtonPropsDisabled     .~ False
        & iconButtonPropsOnClick      .~ [RS.Logout]

logout_ :: ReactElementM eventHandler ()
logout_ = view_ logout "logout_"


-- * Registration

registrationStyles :: [Decl]
registrationStyles = defaultStyles

registration :: FormError -> View '[]
registration errors = mkStatefulView "Registration" (RegistrationForm "" "" "" "" False errors) $ \curState -> do
  div_ ["style" @@= registrationStyles] $ do
    h1_ "Registration"

    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      mapM_ (p_ . elemCS)
        (curState ^. registrationFormErrors)

      inputField "registration-username"  "text"     "Username"    registrationFormUsername >> br_ []
      inputField "registration-email1"    "email"    "Email"       registrationFormEmail1   >> br_ []
      inputField "registration-email2"    "email"    "Email again" registrationFormEmail2   >> br_ []
      inputField "registration-password1" "password" "Password"    registrationFormPassword >> br_ []

      inputFieldWithKey "registration-agree" "checkbox" "" "checked" registrationFormAgree
      "I agree with the terms of use." >> br_ []

      iconButton_ $ defaultIconButtonProps @[RS.GlobalAction]
        & iconButtonPropsIconProps    .~ IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") Large
        & iconButtonPropsElementName  .~ "submit"
        & iconButtonPropsLabel        .~ "submit"
        & iconButtonPropsDisabled     .~ invalidRegistrationForm curState
        & iconButtonPropsOnClick      .~ [RS.CreateUser
                                              . (CreateUser <$> _registrationFormUsername
                                                            <*> _registrationFormEmail1
                                                            <*> _registrationFormPassword)
                                              $ curState]

registration_ :: FormError -> ReactElementM eventHandler ()
registration_ !errors = view_ (registration errors) "registration_"
