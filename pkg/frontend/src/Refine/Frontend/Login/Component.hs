{-# LANGUAGE CPP #-}
#include "language_frontend.hs"

module Refine.Frontend.Login.Component where
#include "import_frontend.hs"

import           Language.Css.Syntax

import           Refine.Common.Types.Prelude
import           Refine.Frontend.Icon
import qualified Refine.Frontend.Icon.Svg as Svg
import           Refine.Frontend.Types
import           Refine.Frontend.Login.Types
import qualified Refine.Frontend.Store.Types as RS
import qualified Refine.Frontend.Access as RS
import           Refine.Frontend.Util


-- * Helper

inputFieldWithKeyExtra
  :: (FromJSVal c)
  => [PropertyOrHandler ('StatefulEventHandlerCode s)]
  -> JSString -> JSString -> JSString -> JSString -> ASetter s s b c
  -> ReactElementM ('StatefulEventHandlerCode s) ()
inputFieldWithKeyExtra extra fieldId fieldType fieldPlaceholder fieldKey asetter =
  input_ $ extra <>
         [ "id" $= fieldId
         , "type" $= fieldType
         , "placeholder" $= fieldPlaceholder
         , onChange $ \evt -> simpleHandler $ \st -> ([], Just (st & asetter .~ target evt fieldKey))
         ]

inputFieldWithKey
  :: (FromJSVal c)
  => JSString -> JSString -> JSString -> JSString -> ASetter s s b c
  -> ReactElementM ('StatefulEventHandlerCode s) ()
inputFieldWithKey = inputFieldWithKeyExtra []

inputField
  :: (FromJSVal c)
  => JSString -> JSString -> JSString -> ASetter s s b c
  -> ReactElementM ('StatefulEventHandlerCode s) ()
inputField i t p = inputFieldWithKey i t p "value"


-- * Form types

data LoginForm = LoginForm
  { _loginFormUsername :: ST
  , _loginFormPassword :: ST
  , _loginFormErrors   :: FormError
  } deriving (Eq, Generic, Show)

data RegistrationForm = RegistrationForm
  { _registrationFormUsername  :: ST
  , _registrationFormEmail1    :: ST
  , _registrationFormEmail2    :: ST
  , _registrationFormPassword  :: ST
  , _registrationFormAgree     :: Bool
  , _registrationFormErrors    :: FormError
  } deriving (Eq, Generic, Show)

makeRefineTypes [''LoginForm, ''RegistrationForm]

-- | FIXME: I used the pattern "return a list of errors, and then check if that list is null to get
-- the boolean" in `createChunkRangeErrors`, and I quite liked it, as it gives you more informative
-- error messages when you need them.  See also: 'invalidRegistrationForm'.
invalidLoginForm :: HasCallStack => LoginForm -> Bool
invalidLoginForm form = form ^. loginFormUsername . to ST.null || form ^. loginFormPassword . to ST.null

invalidRegistrationForm :: HasCallStack => RegistrationForm -> Bool
invalidRegistrationForm form =
  or [ form ^. registrationFormEmail1 /= form ^. registrationFormEmail2
     , form ^. registrationFormUsername . to ST.null
     , form ^. registrationFormEmail1 . to ST.null
     , form ^. registrationFormPassword . to ST.null
     , form ^. registrationFormAgree . to not
     ]

loginOrLogout_ :: HasCallStack => CurrentUser (Lookup User) -> FormError -> ReactElementM eventHandler ()
loginOrLogout_ = \case
  UserLoggedOut  -> login_
  UserLoggedIn _ -> const logout_

defaultStyles :: HasCallStack => [Decl]
defaultStyles = []


-- * Login

loginStyles :: HasCallStack => [Decl]
loginStyles = defaultStyles

login :: HasCallStack => FormError -> View '[]
login errors = mkStatefulView "Login" (LoginForm "" "" errors) $ \curState ->
  div_ ["className" $= "menu-form m-t-1"] $ do
    div_ ["className" $= "menu-form-header"] .
      div_ ["className" $= "left-column"] .
        div_ ["className" $= "inner-column-1"] .
          div_ ["className" $= "menu-form-header__label"] $
            "Login"

    mapM_ (p_ . elemCS)
      (curState ^. loginFormErrors)

    div_ ["className" $= "menu-form__input-div m-b-1"] $
      inputFieldWithKeyExtra ["className" $= "menu-form__input"] "login-username" "text" "Username" "value" loginFormUsername

    div_ ["className" $= "menu-form__input-div m-b-1"] $
      inputFieldWithKeyExtra ["className" $= "menu-form__input"] "login-password" "password" "Password" "value" loginFormPassword

    div_ ["className" $= "menu-form-submenu m-b-1"] .
      ibutton_ $ emptyIbuttonProps
        (ButtonImageIcon Svg.Save ColorSchemaDark)
        -- TODO: create new svg file for this.  open issue, put it on the list of icons to be fixed.
        [RS.Login . (Login <$> _loginFormUsername <*> _loginFormPassword) $ curState]
        & ibEnabled .~ not (invalidLoginForm curState)

login_ :: HasCallStack => FormError -> ReactElementM eventHandler ()
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

      ibutton_ $ emptyIbuttonProps
        (ButtonImageIcon Svg.Save ColorSchemaDark)
        [RS.Logout]

logout_ :: HasCallStack => ReactElementM eventHandler ()
logout_ = view_ logout "logout_"


-- * Registration

registrationStyles :: HasCallStack => [Decl]
registrationStyles = defaultStyles

registration :: HasCallStack => FormError -> View '[]
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

      ibutton_ $ emptyIbuttonProps
        (ButtonImageIcon Svg.Save ColorSchemaDark)
        [RS.CreateUser
          . (CreateUser
              <$> _registrationFormUsername
              <*> _registrationFormEmail1
              <*> _registrationFormPassword
              <*> pure Nothing
              <*> pure "")
          $ curState]
        & ibEnabled .~ not (invalidRegistrationForm curState)

registration_ :: HasCallStack => FormError -> ReactElementM eventHandler ()
registration_ errors = view_ (registration errors) "registration_"
