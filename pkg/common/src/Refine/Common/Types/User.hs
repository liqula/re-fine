{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.User where

import Data.Int (Int64)
import Data.String.Conversions (ST)
import GHC.Generics (Generic)

import Refine.Prelude.TH


type Username = ST
type Password = ST

data CreateUser = CreateUser
  { _cuName :: Username
  , _cuMail :: ST
  , _cuPwd  :: Password
  }
  deriving (Eq, Ord, Show, Read, Generic)


-- | 'UserID's are stored in the database, the primary key is used
-- to identify the user.
type UserID = Int64

newtype User = User
  { _userID :: UserID
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Login = Login
  { _loginUserID :: Username
  , _loginPwd    :: Password
  }
  deriving (Eq, Ord, Show, Read, Generic)


-- * make refine types

makeRefineType ''CreateUser
makeRefineType ''User
makeRefineType ''Login
