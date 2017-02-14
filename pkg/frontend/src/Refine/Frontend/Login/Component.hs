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
import           Data.String.Conversions (ST)
import           GHC.Generics (Generic)
import           GHCJS.Marshal (FromJSVal)
import           React.Flux

import           Refine.Common.Types.User
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets
import           Refine.Prelude.TH (makeRefineType)


-- * Helper

inputField :: (FromJSVal c) => JSString -> JSString -> JSString -> ASetter s a b c -> ReactElementM (s -> ([t], Maybe a)) ()
inputField id_ type_ placeholder_ lens =
  input_ [ "id" $= id_
         , "type" $= type_
         , "placeholder" $= placeholder_
         , onChange $ \evt state -> ([], Just (state & lens .~ target evt "value"))
         ]

-- * Form types

data LoginForm = LoginForm
  { _loginFormUsername :: ST
  , _loginFormPassword :: ST
  } deriving (Eq, Generic, Show)

makeRefineType ''LoginForm

data RegistrationForm = RegistrationForm
  { _registrationFormUsername  :: ST
  , _registrationFormEmail1    :: ST
  , _registrationFormEmail2    :: ST
  , _registrationFormPassword  :: ST
  } deriving (Eq, Generic, Show)

makeRefineType ''RegistrationForm

invalidLoginForm :: LoginForm -> Bool
invalidLoginForm form = form ^. loginFormUsername . to DT.null || form ^. loginFormPassword . to DT.null

invalidRegistrationForm :: RegistrationForm -> Bool
invalidRegistrationForm form =
  or [ form ^. registrationFormEmail1 /= form ^. registrationFormEmail2
     , form ^. registrationFormUsername . to DT.null
     , form ^. registrationFormEmail1 . to DT.null
     , form ^. registrationFormPassword . to DT.null
     ]

-- * Login

login_ :: ReactElementM eventHandler ()
login_ = view login () mempty

login :: ReactView ()
login = defineStatefulView "Login" (LoginForm "" "") $ \curState () -> do
  h1_ "Login"

  div_ $ do
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      inputField "login-username" "text"     "Username" loginFormUsername
      inputField "login-password" "password" "Password" loginFormPassword

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


-- * Registration

registration_ :: ReactElementM eventHandler ()
registration_ = view registration () mempty

registration :: ReactView ()
registration = defineStatefulView "Registration" (RegistrationForm "" "" "" "") $ \curState () -> do
  h1_ "Registration"

  div_ $ do
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      inputField "registration-username"  "text"     "Username"    registrationFormUsername
      inputField "registration-email1"    "email"    "Email"       registrationFormEmail1
      inputField "registration-email2"    "email"    "Email again" registrationFormEmail2
      inputField "registration-password1" "password" "Passwqord"   registrationFormPassword

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
