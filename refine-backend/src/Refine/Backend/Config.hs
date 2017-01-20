{-# LANGUAGE TemplateHaskell #-}

module Refine.Backend.Config where

import Control.Lens (makeLenses, makePrisms)
import Data.Default (Default(..))
import System.FilePath ((</>))

-- FIXME: once we know what we need where, we can refactor this type into a tree of records, and
-- only pass those parts of the config to the respective parts of the code that are actually needed.

data Config = Config
  { _cfgShouldMigrate :: Bool
  , _cfgShouldLog     :: Bool
  , _cfgRootDir       :: FilePath
  , _cfgReposRoot     :: FilePath
  , _cfgDBKind        :: DBKind
  , _cfgPoolSize      :: Int
  }

data DBKind
  = DBInMemory
  | DBOnDisk FilePath

instance Default Config where
  def = Config
    { _cfgShouldMigrate = True
    , _cfgShouldLog     = True
    , _cfgRootDir       = "./.backend-data"
    , _cfgReposRoot     = _cfgRootDir def </> "repos"
    , _cfgDBKind        = DBOnDisk (_cfgRootDir def </> "refine.db")
    , _cfgPoolSize      = 5
    }


-- * lenses

makeLenses ''Config

makePrisms ''DBKind
makePrisms ''Config
