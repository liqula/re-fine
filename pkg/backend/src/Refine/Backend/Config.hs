{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Refine.Backend.Config where

import           Control.Exception (throwIO, ErrorCall(ErrorCall))
import           Control.Lens (makeLenses, makePrisms, (&), (^.))
import           Data.Aeson (FromJSON, ToJSON, object, withObject, (.=), (.:))
import           Data.Default (Default(..))
import           Data.Maybe (isNothing)
import           Data.String.Conversions (ST, cs, (<>))
import qualified Data.Yaml as Yaml
import           Data.Yaml (encode)
import           GHC.Generics
import           Network.Wai.Handler.Warp as Warp
import           Text.Read (readMaybe)

import           Refine.Prelude (Timespan(..))


-- FIXME: once we know what we need where, we can refactor this type into a tree of records, and
-- only pass those parts of the config to the respective parts of the code that are actually needed.

data Config = Config
  { _cfgShouldLog     :: Bool           -- ^ Should log messages during the server run
  , _cfgDBKind        :: DBKind         -- ^ SQLite database, memory or file on the disk
  , _cfgPoolSize      :: Int            -- ^ The size of the connection pool towards the database
  , _cfgFileServeRoot :: Maybe FilePath -- ^ Directory for the static files
  , _cfgWarpSettings  :: WarpSettings   -- ^ check test suite for examples of what can be put in here.
  , _cfgCsrfSecret    :: ST             -- ^ The secret for csrf
  , _cfgSessionLength :: Timespan       -- ^ Session cookie life expectancy
  , _cfgDevMode       :: Bool           -- ^ In dev mode all login attempt will be successful and dev-mode user will be forced login into the sessions
  , _cfgPoFilesRoot   :: FilePath       -- ^ The directory of Po translation files
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data DBKind
  = DBInMemory
  | DBOnDisk FilePath
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Default Config where
  def = Config
    { _cfgShouldLog     = True
    , _cfgDBKind        = def
    , _cfgPoolSize      = 5
    , _cfgFileServeRoot = Just "../frontend/js-build"
    , _cfgWarpSettings  = def
    , _cfgCsrfSecret    = "CSRF-SECRET"
    , _cfgSessionLength = TimespanHours 72
    , _cfgDevMode       = False
    , _cfgPoFilesRoot   = "./po"
    }

instance Default DBKind where
  def = DBOnDisk "./.backend-data/refine.db"


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
    Left msg -> throwIO . ErrorCall $ show msg <> explainConfig def "\n\nTry the following default config" False
    Right v  -> pure v
  putStrLn $ explainConfig cfg
    (maybe "Using default config" (\fp -> "Using config from " <> show fp) mfp)
    (isNothing mfp)
  pure cfg


explainConfig :: Config -> String -> Bool -> String
explainConfig cfg intro mentionServerConf = unlines $
  [ intro <> ":"
  , "------------------------------"
  , cs (encode cfg)
  , "------------------------------"
  , ""
  ] <>
  if mentionServerConf
    then [ "You can copy the yaml code between the lines above into 'server.conf',"
         , "edit to your liking, and pass it as first argument to the server."
         , ""
         ]
    else []
