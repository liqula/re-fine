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

{-

How to add a new process type?

- Create a Process type (PT) in Refine.Common.Types.Process.
- Create a 'Create' type for PT (CPT)
- Add to the type family instance: type family (Process PT) = CPT
- Adjust the AddProcess, CreatedProcess, RemoveProcess types in the Refine.Common.Types.Process
- Adjust the ChangeProcess in the Refine.Common.ChangeAPI module.
- Create a table and a connection table in the Refine.Backend.Database.Schema module
- Implement the StoreProcessData typeclass for the PT type in the Refine.Backend.Database.Entity module.
- Adjust the combinators in this module.

-}

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
