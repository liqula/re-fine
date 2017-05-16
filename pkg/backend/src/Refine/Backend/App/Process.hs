{-# LANGUAGE NoImplicitPrelude          #-}
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

{- |

How to add a new process type?

- Create a Process type (PT) in Refine.Common.Types.Process.
- Create a 'Create' type for PT (CPT)
- Add to the type family instance: `type instance Create (Process PT) = CPT`
- Adjust the AddProcess, CreatedProcess, RemoveProcess types in the Refine.Common.Types.Process
- Adjust ChangeProcess in the Refine.Common.ChangeAPI module.
- Create a table and a connection table in the Refine.Backend.Database.Schema module
- Implement the StoreProcessData typeclass for the PT type in the Refine.Backend.Database.Entity module.
- Adjust the combinators in this module.

-}
module Refine.Backend.App.Process
  ( Refine.Backend.App.Process.addProcess
  , Refine.Backend.App.Process.changeProcess
  , Refine.Backend.App.Process.removeProcess
  ) where

import Refine.Backend.Prelude

import Control.Lens ((^.), to)

import Refine.Backend.App.Core
import Refine.Backend.App.VDoc
import Refine.Backend.Database.Class as DB hiding (createVDoc)
import Refine.Backend.Database.Types
import Refine.Common.ChangeAPI
import Refine.Common.Types


addProcessCollabEdit
  :: Create (Process CollaborativeEdit)
  -> App (Process CollaborativeEdit, CompositeVDoc)
addProcessCollabEdit aice = do
  appLog "addProcessCollabEdit"
  vdoc <- createVDoc (aice ^. createCollabEditProcessVDoc)
  process <- db $ do
    gid <- aice ^. createCollabEditProcessGroup . to groupRef
    createProcess CreateDBCollabEditProcess
      { _createDBCollabEditProcessPhase   = aice ^. createCollabEditProcessPhase
      , _createDBCollabEditProcessGroupID = gid
      , _createDBCollabEditProcessVDocID  = vdoc ^. vdocID
      }
  cvdoc <- getCompositeVDoc (vdoc ^. vdocID)
  pure (process, cvdoc)

-- | FIXME: currently, 'changeProcess' allows to overwrite the process data completely, but we
-- probably want something more subtle, like not destroying the VDoc, but changing the group.  needs
-- more thinking.
updateProcessCollabEdit
  :: ID (Process CollaborativeEdit) -> Create (Process CollaborativeEdit) -> App ()
updateProcessCollabEdit _pid _aice = do
  appLog "updateProcessCollabEdit"
  error "not implemented yet."

addProcess :: AddProcess -> App CreatedProcess
addProcess ap = do
  appLog "addProcess"
  case ap of
    AddCollabEditProcess p -> uncurry CreatedCollabEditProcess <$> addProcessCollabEdit p
    AddAulaProcess p       -> CreatedAulaProcess               <$> db (createProcess p)

changeProcess :: ChangeProcess -> App ()
changeProcess change = do
  appLog "changeProcess"
  case change of
    ChangeProcessCollaborativeEditPhase pid create -> updateProcessCollabEdit pid create
    ChangeProcessAulaClassName pid create          -> db $ DB.updateProcess pid create

removeProcess :: RemoveProcess -> App ()
removeProcess remove = do
  appLog "removeProcess"
  db $ case remove of
    RemoveCollabEditProcess pid -> DB.removeProcess pid
    RemoveAulaProcess       pid -> DB.removeProcess pid
