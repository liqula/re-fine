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

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Backend.User.Class
import Refine.Common.ChangeAPI
import Refine.Common.Types.Process


type AppProcessConstraint db uh =
  ( StoreProcessData db Aula
  , StoreProcessData db CollaborativeEdit
  , DatabaseC db
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

removeProcess :: AppProcessConstraint db uh => RemoveProcess -> AppM db uh ()
removeProcess remove = do
  appLog "removeProcess"
  db $ case remove of
    RemoveCollabEditProcess pid -> DB.removeProcess pid
    RemoveAulaProcess       pid -> DB.removeProcess pid
