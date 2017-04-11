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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Common.Types.Prelude where

import           Control.Lens
import           Data.Int
import           Data.String.Conversions (ST, cs)
import           GHC.Generics (Generic)
import           Text.Read
import           Web.HttpApiData

import Refine.Prelude (ClearTypeParameter(..), Timestamp)
import Refine.Prelude.TH


-- * ID

newtype ID a = ID { _unID :: Int64 }
  deriving (Eq, Ord, Show, Read, Generic)

instance ClearTypeParameter ID where
  clearTypeParameter (ID x) = ID x

type family Create a = b | b -> a

instance ToHttpApiData (ID a) where
  toUrlPiece (ID x) = cs $ show x

instance FromHttpApiData (ID a) where
  parseUrlPiece = either (Left . cs) (Right . ID) . (readEither :: String -> Either String Int64) . cs


-- * user related types

type Username = ST
type Email    = ST
type Password = ST

data CreateUser = CreateUser
  { _cuName :: Username
  , _cuMail :: Email
  , _cuPwd  :: Password
  }
  deriving (Eq, Ord, Show, Read, Generic)

newtype User = User
  { _userMetaID :: MetaID User -- ^ The primary key is used to identify the user.
  }
  deriving (Eq, Ord, Show, Read, Generic)

data Login = Login
  { _loginUsername :: Username
  , _loginPassword :: Password
  }
  deriving (Eq, Ord, Show, Read, Generic)

data UserInfo = UserIP ST | UserID (ID User) | Anonymous
  deriving (Eq, Ord, Show, Read, Generic)


-- * meta info

data MetaInfo = MetaInfo
  { _metaCreatedBy :: UserInfo
  , _metaCreatedAt :: Timestamp
  , _metaChangedBy :: UserInfo
  , _metaChangedAt :: Timestamp
  }
  deriving (Eq, Ord, Show, Read, Generic)

data MetaID a = MetaID
  { _miID   :: ID a
  , _miMeta :: MetaInfo
  }
  deriving (Eq, Ord, Show, Read, Generic)


-- * make refine types

makeRefineType ''CreateUser
makeRefineType ''User
makeRefineType ''Login
makeRefineType ''UserInfo
makeRefineType ''ID
makeRefineType ''MetaInfo
makeRefineType ''MetaID

userID :: Lens' User (ID User)
userID = userMetaID . miID
