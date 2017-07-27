{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
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

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Refine.Backend.App.Allow where

{- FIXME
import Refine.Backend.Prelude

import           Control.Lens ((^.))
import           Control.Monad.Except (throwError)
import           Control.Monad (join, unless)
import qualified Data.Set as Set

import           Refine.Backend.App.Core
import           Refine.Backend.App.User
import qualified Refine.Backend.Database.Class as DB
import           Refine.Common.Allow
import           Refine.Common.Types


assertPerms
  ::  ( MonadApp db
      , DB.GroupOf db target
      , DB.ProcessOf db target
      , Allow (DB.ProcessPayload target) target
      )
  => ID target -> [Perm] -> AppM db ()
assertPerms tid needPerms = do
  muserId <- currentUser
  join . db $ do
    group <- DB.groupOf tid
    prc   <- DB.processOf tid
    roles <- maybe (pure []) (DB.getRoles (group ^. groupID)) muserId
    pure $ do
      let perms = concatMap (allow muserId prc tid) roles
      unless (Set.fromList needPerms `Set.isSubsetOf` Set.fromList perms) $
        throwError AppUnauthorized
-}
