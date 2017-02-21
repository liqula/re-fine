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

module Refine.Frontend.Login.Component where

import           Control.Lens ((&), (.~), (^.), ASetter, to)
import           Data.JSString (JSString)
import qualified Data.Text as DT
import           Data.String.Conversions (ST, cs)
import           GHC.Generics (Generic)
import           GHCJS.Marshal (FromJSVal)
import           React.Flux

import           Refine.Common.Types.User
import           Refine.Frontend.Login.Types
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets
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
         , onChange $ \evt state -> ([], Just (state & lens .~ target evt fieldKey))
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
  , _loginFormErrors   :: Maybe ST
  } deriving (Eq, Generic, Show)

makeRefineType ''LoginForm

data RegistrationForm = RegistrationForm
  { _registrationFormUsername  :: ST
  , _registrationFormEmail1    :: ST
  , _registrationFormEmail2    :: ST
  , _registrationFormPassword  :: ST
  , _registrationFormAgree     :: Bool
  , _registrationFormErrors    :: Maybe ST
  } deriving (Eq, Generic, Show)

makeRefineType ''RegistrationForm

-- | FIXME: I used the pattern "return a list of errors, and then check if that list is null to get
-- the boolean" in `createChunkRangeErrors`, and I quite liked it, as it gives you more informative
-- error messages when you need them.  See also: 'invalidRegistrationForm'.
invalidLoginForm :: LoginForm -> Bool
invalidLoginForm form = form ^. loginFormUsername . to DT.null || form ^. loginFormPassword . to DT.null

invalidRegistrationForm :: RegistrationForm -> Bool
invalidRegistrationForm form =
  or [ form ^. registrationFormEmail1 /= form ^. registrationFormEmail2
     , form ^. registrationFormUsername . to DT.null
     , form ^. registrationFormEmail1 . to DT.null
     , form ^. registrationFormPassword . to DT.null
     , form ^. registrationFormAgree . to not
     ]

loginOrLogout_ :: CurrentUser -> Maybe ST -> ReactElementM eventHandler ()
loginOrLogout_ = \case
  UserLoggedOut  -> login_
  UserLoggedIn _ -> const logout_

-- * Login

login_ :: Maybe ST -> ReactElementM eventHandler ()
login_ errors = view (login errors) () mempty

login :: Maybe ST -> ReactView ()
login errors = defineStatefulView "Login" (LoginForm "" "" errors) $ \curState () -> do
  h1_ "Login"

  div_ $ do
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      mapM_ (p_ . cs) (curState ^. loginFormErrors)

      inputField "login-username" "text"     "Username" loginFormUsername >> br_ []
      inputField "login-password" "password" "Password" loginFormPassword >> br_ []

      iconButton_
        (IconButtonProps
          (IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") L)
          "submit"
          ""
          ""
          "submit"
          (invalidLoginForm curState)
          (\_ -> (RS.dispatch . RS.Login) . (Login <$> _loginFormUsername <*> _loginFormPassword) $ curState)
          []
        )

-- * Logout

logout_ :: ReactElementM eventHandler ()
logout_ = view logout () mempty

logout :: ReactView ()
logout = defineView "Logout" $ \() -> do

  div_ $ do
    p_ "Profile page"
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      iconButton_
        (IconButtonProps
          (IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") L)
          "submit"
          ""
          ""
          "Logout"
          False
          (\_ -> RS.dispatch RS.Logout)
          []
        )

-- * Registration

registration_ :: Maybe ST -> ReactElementM eventHandler ()
registration_ errors = view (registration errors) () mempty

registration :: Maybe ST -> ReactView ()
registration errors = defineStatefulView "Registration" (RegistrationForm "" "" "" "" False errors) $ \curState () -> do
  h1_ "Registration"

  div_ $ do
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      mapM_ (p_ . cs) (curState ^. registrationFormErrors)

      inputField "registration-username"  "text"     "Username"    registrationFormUsername >> br_ []
      inputField "registration-email1"    "email"    "Email"       registrationFormEmail1   >> br_ []
      inputField "registration-email2"    "email"    "Email again" registrationFormEmail2   >> br_ []
      inputField "registration-password1" "password" "Password"    registrationFormPassword >> br_ []

      inputFieldWithKey "registration-agree" "checkbox" "" "checked" registrationFormAgree
      "I agree with the terms of use." >> br_ []

      iconButton_
        (IconButtonProps
          (IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") L)
          "submit"
          ""
          ""
          "submit"
          (invalidRegistrationForm curState)
          (\_ -> (RS.dispatch . RS.CreateUser) .
            (CreateUser <$> _registrationFormUsername
                        <*> _registrationFormEmail1
                        <*> _registrationFormPassword)
            $ curState)
          []
        )
