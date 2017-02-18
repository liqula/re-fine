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

module Refine.Common.Types.Access where

import GHC.Generics

import Refine.Common.Types.Prelude (ID)
import Refine.Common.Types.Comment (Note, Question, Discussion)
import Refine.Common.Types.User (User)
import Refine.Prelude.TH (makeRefineType)


data Access = Grant | Revoke
  deriving (Eq, Show, Generic)

data AccessibleData
  = AccessibleNote       (ID Note)
  | AccessibleDiscussion (ID Discussion)
  | AccessibleQuestion   (ID Question)
  deriving (Eq, Show, Generic)

data ChangeAccess = ChangeAccess AccessibleData Access (ID User)
  deriving (Eq, Show, Generic)

makeRefineType ''Access
makeRefineType ''AccessibleData
makeRefineType ''ChangeAccess
