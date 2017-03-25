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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.App.Process where

import Control.Lens ((^.))

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Backend.User.Class
import Refine.Common.ChangeAPI
import Refine.Common.Types.Process
import Refine.Common.Types.Prelude


addProcess
  :: ( StoreProcessData db Aula
     , StoreProcessData db CollaborativeEdit
     , DatabaseC db
     , UserHandleC uh
     )
  => AddProcess -> AppM db uh CreatedProcess
addProcess ap = do
  appLog "addProcess"
  db $ case ap of
    AddAulaProcess p       -> CreatedAulaProcess       <$> createProcess p
    AddCollabEditProcess p -> CreatedCollabEditProcess <$> createProcess p
