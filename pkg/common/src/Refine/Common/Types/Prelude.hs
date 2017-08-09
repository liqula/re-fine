{-# LANGUAGE NoImplicitPrelude          #-}
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

import Refine.Common.Prelude as P

import Data.Int


-- * ID

newtype ID a = ID { _unID :: Int64 }
  deriving (Eq, Ord, Enum, Num, Read, Generic)

instance Show (ID a) where
  showsPrec p (ID i) = showParen (p > 10) (("ID " <>) . shows i)

makeRefineType ''ID

instance ToHttpApiData (ID a) where
  toUrlPiece (ID x) = cs $ show x

instance FromHttpApiData (ID a) where
  parseUrlPiece = either (Left . cs) (Right . ID) . (readEither :: String -> Either String Int64) . cs

instance (ToJSON a) => ToJSONKey (ID a) where
  toJSONKey = ToJSONKeyValue (P.Number . fromIntegral . _unID) toEncoding

instance (FromJSON a) => FromJSONKey (ID a)

-- FUTUREWORK: aeson has Int64 instances for ToJSONKey, FromJSONKey.  how can we use those?


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

data User = User
  { _userMetaID :: MetaID User -- ^ The primary key is used to identify the user.
  , _userName   :: Username
  , _userEmail  :: Email
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

-- | 'MetaInfo' and 'ID', packed together to be contained in all the many types that need both.
--
-- FUTUREWORK: we could add a field @_medaID :: ID@ to 'MetaInfo' and replace 'MetaID' with that.
-- That would be awkward if we had to handle 'MetaInfo' values in contexts where we don't have an
-- 'ID' yet, but if that never happens we should consider refactoring this.
data MetaID a = MetaID
  { _miID   :: ID a
  , _miMeta :: MetaInfo
  }
  deriving (Eq, Ord, Show, Read, Generic)


-- * make refine types

makeRefineTypes [''CreateUser, ''User, ''Login, ''UserInfo, ''MetaInfo, ''MetaID]

userID :: Lens' User (ID User)
userID = userMetaID . miID
