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

import           Control.Lens ((&), (.~), (^.), to)
import qualified Data.Text as DT
import           Data.String.Conversions (ST)
import           GHC.Generics (Generic)
import           React.Flux

import           Refine.Common.Types.User
import qualified Refine.Frontend.Store as RS
import qualified Refine.Frontend.Types as RS
import           Refine.Frontend.UtilityWidgets
import           Refine.Prelude.TH (makeRefineType)


data LoginForm = LoginForm
  { _loginFormUsername :: ST
  , _loginFormPassword :: ST
  } deriving (Eq, Generic, Show)

makeRefineType ''LoginForm


login_ :: ReactElementM eventHandler ()
login_ = view login () mempty

login :: ReactView ()
login = defineStatefulView "Login" (LoginForm "" "") $ \curState () -> do
  h1_ "Login"

  div_ $ do
    form_ [ "target" $= "#"
          , "action" $= "POST" ] $ do

      input_ [ "id" $= "login-username"
             , "type" $= "text"
             , onChange $ \evt state -> ([], Just (state & loginFormUsername .~ target evt "value"))
             ]
      input_ [ "id" $= "login-password"
             , "type" $= "password"
             , onChange $ \evt state -> ([], Just (state & loginFormPassword .~ target evt "value"))
             ]

      iconButton_
        (IconButtonProps
          (IconProps "c-vdoc-overlay-content" True ("icon-Share", "dark") L)
          "submit"
          ""
          ""
          "submit"
          (curState ^. loginFormUsername . to DT.null || curState ^. loginFormPassword . to DT.null)
          (\_ -> (RS.dispatch . RS.Login) . (Login <$> _loginFormUsername <*> _loginFormPassword) $ curState)
        )
