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

module Refine.Backend.App.Group where

import Refine.Backend.App.Core
import Refine.Backend.Database.Class as DB
import Refine.Common.Types.Group
import Refine.Common.Types.Prelude


-- * group manipulation

createGroup :: Create Group -> App Group -- TODO: Rename addGroup
createGroup group = do
  appLog "createGroup"
  db $ DB.createGroup group

-- | Modify the group using the new values from the `Create Group` information.
modifyGroup :: ID Group -> Create Group -> App Group
modifyGroup = undefined

-- | Remove a group and cleans up dangling references.
--
-- The users won't be transitive members of supergroups any more.
--
removeGroup :: ID Group -> App ()
removeGroup = undefined

-- * subgroups

-- | Add a new child group to a group
addSubGroup :: ID Group -> ID Group -> App ()
addSubGroup _child _parent = undefined

-- | Remove a child group from a parent
removeSubGroup :: ID Group -> ID Group -> App ()
removeSubGroup _child _parent = undefined
