{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Refine.Backend.Config where

import           Control.Exception (throwIO, ErrorCall(ErrorCall))
import           Control.Lens (makeLenses, makePrisms)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Default (Default(..))
import           Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import           Data.Yaml (encode)
import           GHC.Generics
import           System.FilePath ((</>))


-- FIXME: once we know what we need where, we can refactor this type into a tree of records, and
-- only pass those parts of the config to the respective parts of the code that are actually needed.

data Config = Config
  { _cfgShouldMigrate :: Bool
  , _cfgShouldLog     :: Bool
  , _cfgRootDir       :: FilePath
  , _cfgReposRoot     :: FilePath
  , _cfgDBKind        :: DBKind
  , _cfgPoolSize      :: Int
  , _cfgFileServeRoot :: Maybe FilePath
  }
  deriving (Generic, FromJSON, ToJSON)

data DBKind
  = DBInMemory
  | DBOnDisk FilePath
  deriving (Generic, FromJSON, ToJSON)

instance Default Config where
  def = Config
    { _cfgShouldMigrate = True
    , _cfgShouldLog     = True
    , _cfgRootDir       = "./.backend-data"
    , _cfgReposRoot     = _cfgRootDir def </> "repos"
    , _cfgDBKind        = DBOnDisk (_cfgRootDir def </> "refine.db")
    , _cfgPoolSize      = 5
    , _cfgFileServeRoot = Just "../refine-frontend/js-build"
    }


-- * lenses

makeLenses ''Config

makePrisms ''DBKind
makePrisms ''Config


-- * crude config file support

-- | Initialize config from a given yaml file or, if 'Nothing' is given, the default value.  Print
-- config to stdout before returning with it, as a simple mechanism for providing a default yaml
-- file (people who run this for the first time can copy stdout into a file and edit that).
initConfig :: Maybe FilePath -> IO Config
initConfig mfp = do
  result <- maybe (pure $ pure def) Yaml.decodeFileEither mfp
  cfg <- case result of
    Left msg -> throwIO . ErrorCall . show $ msg
    Right v  -> pure v
  putStrLn $ unlines
    [ "config:"
    , "------------------------------"
    , cs (encode cfg)
    , "------------------------------"
    ]
  pure cfg
