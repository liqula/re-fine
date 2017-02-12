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

import           React.Flux

import           Refine.Common.Types.User
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS


login_ :: ReactElementM eventHandler ()
login_ = view login () mempty

login :: ReactView ()
login = defineView "Login" $ \() -> do
  h1_ "Login"
  button_ [ "id" $= "create-user"
          , onClick $ \_ _ -> RS.dispatch . RS.CreateUser $ CreateUser "user" "user@email.com" "password"
          ] $
          elemString "Create User"
  button_ [ "id" $= "login"
          , onClick $ \_ _ -> RS.dispatch . RS.Login $ Login "user" "password"
          ] $
          elemString "Login with the user"
  button_ [ "id" $= "logout"
          , onClick $ \_ _ -> RS.dispatch RS.Logout
          ] $
          elemString "Logout"
