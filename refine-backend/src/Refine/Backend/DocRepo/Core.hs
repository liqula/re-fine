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

module Refine.Backend.DocRepo.Core where

import GHC.Generics (Generic)
import Control.Lens (makeLenses)
import Control.Exception (SomeException)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.String.Conversions (ST)

import Refine.Backend.Config
import Refine.Common.VDoc.HTML (VDocHTMLError)


data DocRepoError
  = DocRepoUnknownError String
  | DocRepoException SomeException
  | DocRepoVDocError VDocHTMLError
  deriving (Show)

newtype DocRepo a = DocRepo { unDocRepo :: ExceptT DocRepoError (ReaderT Config IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError DocRepoError
    , MonadReader Config
    )

docRepoIO :: IO a -> DocRepo a
docRepoIO = DocRepo . liftIO

-- * types

newtype RepoHandle = RepoHandle { _unRepoHandle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

newtype PatchHandle = PatchHandle { _unPatchHandle :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

-- * lenses

makeLenses ''RepoHandle
makeLenses ''PatchHandle
