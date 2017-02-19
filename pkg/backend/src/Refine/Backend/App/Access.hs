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

module Refine.Backend.App.Access where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import Refine.Backend.App.Core
import Refine.Backend.App.User (doesUserExist)
import Refine.Backend.Database.Core (DB)
import Refine.Backend.Database.Class
import Refine.Common.Types


-- NOTE: There is a possible attack. The attacker can learn
-- the user ids, probing them to give access to his own document
-- if the access is granted, the user exists.
changeAccess :: ChangeAccess -> App DB ()
changeAccess (ChangeAccess ad a u) = do
  exists <- doesUserExist u
  unless exists . throwError $ AppSanityCheckError "Change access"
  case ad of
    AccessibleNote       nid -> changeNoteAccess nid a u
    AccessibleDiscussion did -> changeDiscussionAccess did a u
    AccessibleQuestion   qid -> changeQuestionAccess qid a u

changeNoteAccess :: ID Note -> Access -> ID User -> App DB ()
changeNoteAccess nid a uid = do
  appLog $ unwords [show a, " access on note ", show nid, " for ", show uid]
  db $ case a of
    Grant  -> addNoteUserAccess nid uid
    Revoke -> removeNoteUserAccess nid uid

changeDiscussionAccess :: ID Discussion -> Access -> ID User -> App DB ()
changeDiscussionAccess did a uid = do
  appLog $ unwords [show a, " access on discussion ", show did, " for ", show uid]
  db $ case a of
    Grant  -> addDiscussionUserAccess did uid
    Revoke -> removeDiscussionUserAccess did uid

changeQuestionAccess :: ID Question -> Access -> ID User -> App DB ()
changeQuestionAccess qid a uid = do
  appLog $ unwords [show a, " access on question ", show qid, " for ", show uid]
  db $ case a of
    Grant  -> addQuestionUserAccess qid uid
    Revoke -> removeQuestionUserAccess qid uid
