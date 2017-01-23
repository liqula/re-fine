{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Refine.Backend.Config where

import           Control.Exception (throwIO, ErrorCall(ErrorCall))
import           Control.Lens (makeLenses, makePrisms, (&), (^.))
import           Data.Aeson (FromJSON, ToJSON, object, withObject, (.=), (.:))
import           Data.Default (Default(..))
import           Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import           Data.Yaml (encode)
import           GHC.Generics
import           Network.Wai.Handler.Warp as Warp
import           System.FilePath ((</>))
import           Text.Read (readMaybe)


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
  , _cfgWarpSettings  :: WarpSettings
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data DBKind
  = DBInMemory
  | DBOnDisk FilePath
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Default Config where
  def = Config
    { _cfgShouldMigrate = True
    , _cfgShouldLog     = True
    , _cfgRootDir       = "./.backend-data"
    , _cfgReposRoot     = _cfgRootDir def </> "repos"
    , _cfgDBKind        = def
    , _cfgPoolSize      = 5
    , _cfgFileServeRoot = Just "../refine-frontend/js-build"
    , _cfgWarpSettings  = def
    }

instance Default DBKind where
  def = DBOnDisk (_cfgRootDir def </> "refine.db")


-- * warp settings

-- | "Network.Wai.Handler.Warp" contains 'Settings', which we could use here, but that module is not
-- exposed from warp.
data WarpSettings = WarpSettings
  { _warpSettingsPort :: Warp.Port
  , _warpSettingsHost :: Warp.HostPreference
  }
  deriving (Eq, Show, Generic)

instance ToJSON WarpSettings where
  toJSON (WarpSettings port host) = object
    [ "_warpSettingsPort" .= port
    , "_warpSettingsHost" .= show host
    ]

instance FromJSON WarpSettings where
  parseJSON = withObject "WarpSettings" $ \o -> WarpSettings
    <$> o .: "_warpSettingsPort"
    <*> (parseHost =<< o .: "_warpSettingsHost")
    where
      parseHost = maybe (fail "WarpSettings._warpSettingsHost") pure . readMaybe

instance Default WarpSettings where
  def = WarpSettings
    { _warpSettingsPort = 3000
    , _warpSettingsHost = "*4"
    }


-- * lenses

makeLenses ''WarpSettings
makeLenses ''Config

makePrisms ''WarpSettings
makePrisms ''DBKind
makePrisms ''Config


warpSettings :: Config -> Settings
warpSettings cfg = Warp.defaultSettings
  & setPort (cfg ^. cfgWarpSettings . warpSettingsPort)
  & setHost (cfg ^. cfgWarpSettings . warpSettingsHost)


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
