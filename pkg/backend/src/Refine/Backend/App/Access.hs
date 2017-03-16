{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Access where

import Control.Lens ((^.))
import Control.Monad (unless, when)
import Control.Monad.Except (throwError)

import Refine.Backend.App.Core
import Refine.Backend.App.User (doesUserExist)
import Refine.Backend.Database.Class as DB
import Refine.Common.Types
import Refine.Common.ChangeAPI


-- NOTE: There is a possible attack. The attacker can learn
-- the user ids, probing them to give access to his own document
-- if the access is granted, the user exists.
changeAccess :: ChangeAccess -> App ()
changeAccess (ChangeAccess ad a u) = do
  exists <- doesUserExist u
  unless exists . throwError $ AppSanityCheckError "Change access"
  case ad of
    ContribIDNote       nid -> changeNoteAccess nid a u
    ContribIDQuestion   qid -> changeQuestionAccess qid a u
    ContribIDDiscussion did -> changeDiscussionAccess did a u
    ContribIDEdit       _   -> error "not implemented."
    ContribIDHighlightMark  -> error "impossible."

changeNoteAccess :: ID Note -> Access -> ID User -> App ()
changeNoteAccess nid a uid = do
  appLog $ unwords [show a, " access on note ", show nid, " for ", show uid]
  db $ case a of
    Grant  -> addNoteUserAccess nid uid
    Revoke -> removeNoteUserAccess nid uid

changeDiscussionAccess :: ID Discussion -> Access -> ID User -> App ()
changeDiscussionAccess did a uid = do
  appLog $ unwords [show a, " access on discussion ", show did, " for ", show uid]
  db $ case a of
    Grant  -> addDiscussionUserAccess did uid
    Revoke -> removeDiscussionUserAccess did uid

changeQuestionAccess :: ID Question -> Access -> ID User -> App ()
changeQuestionAccess qid a uid = do
  appLog $ unwords [show a, " access on question ", show qid, " for ", show uid]
  db $ case a of
    Grant  -> addQuestionUserAccess qid uid
    Revoke -> removeQuestionUserAccess qid uid

changeRole :: ChangeRole -> App ()
changeRole cr = do
  appLog "changeRole"
  let cmd = case cr of
              AssignRole{}   -> Refine.Backend.App.Access.assignRole
              UnassignRole{} -> Refine.Backend.App.Access.unassignRole
  cmd (cr ^. crRole) (cr ^. crUser) (cr ^. crGroup)

-- | A user is assigned to a group (and not to subgroups).
assignRole :: Role -> ID User -> ID Group -> App ()
assignRole role uid gid = do
  appLog "assignRole"
  db $ DB.assignRole gid uid role

-- | Unassign a role from a user in a group.
unassignRole :: Role -> ID User -> ID Group -> App ()
unassignRole role uid gid = do
  appLog "unassignRole"
  db $ do
    actRole <- DB.getRole gid uid
    when (Just role == actRole) $ DB.unassignRole gid uid role

-- | Return (Just role) if the user has a role in that group
-- otherwise Nothing.
getRole :: ID User -> ID Group -> App (Maybe Role)
getRole uid gid = do
  appLog "getRole"
  db $ DB.getRole gid uid
