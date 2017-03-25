{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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

import Data.Typeable

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Backend.User.Class
import Refine.Common.ChangeAPI
import Refine.Common.Types.Process
import Refine.Common.Types.Prelude


type AppProcessConstraint db uh =
  ( StoreProcessData db Aula
  , StoreProcessData db CollaborativeEdit
  , DatabaseC db
  , UserHandleC uh
  )

type AppProcessConstraintT db uh a =
  ( StoreProcessData db a
  , DatabaseC db
  , Typeable a
  , UserHandleC uh
  )


addProcess :: AppProcessConstraint db uh => AddProcess -> AppM db uh CreatedProcess
addProcess ap = do
  appLog "addProcess"
  db $ case ap of
    AddAulaProcess p       -> CreatedAulaProcess       <$> createProcess p
    AddCollabEditProcess p -> CreatedCollabEditProcess <$> createProcess p

changeProcess :: AppProcessConstraint db uh => ChangeProcess -> AppM db uh ()
changeProcess change = do
  appLog "changeProcess"
  db $ case change of
    ChangeProcessCollaborativeEditPhase pid create -> do
      DB.updateProcess pid create

    ChangeProcessAulaClassName pid create -> do
      DB.updateProcess pid create
