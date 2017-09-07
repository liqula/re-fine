{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.Config where
#include "import_backend.hs"

import qualified Data.Yaml as Yaml
import           Network.Wai.Handler.Warp as Warp

import Refine.Common.Types.Config
import Refine.Common.Types.Core
import Refine.Common.Types.Prelude
import Refine.Common.Types.Role


-- * config tree

data Config = Config
  { _cfgLogger        :: LogCfg         -- ^ logging
  , _cfgDBKind        :: DBKind         -- ^ SQLite database, memory or file on the disk
  , _cfgPoolSize      :: Int            -- ^ The size of the connection pool towards the database
  , _cfgFileServeRoot :: Maybe FilePath -- ^ Directory for the static files
  , _cfgWarpSettings  :: WarpSettings   -- ^ check test suite for examples of what can be put in here.
  , _cfgCsrfSecret    :: ST             -- ^ The secret for csrf
  , _cfgSessionLength :: Timespan       -- ^ Session cookie life expectancy
  , _cfgPoFilesRoot   :: FilePath       -- ^ The directory of Po translation files
  , _cfgSmtp          :: Maybe SmtpCfg  -- ^ for sending notification emails to users
  , _cfgClient        :: ClientCfg      -- ^ what we send to the client
  , _cfgWSPingPeriod  :: Timespan       -- ^ how long between two pings to every connected client
  , _cfgAllAreGods    :: Bool           -- ^ run 'unsafeBeAGod' in the beginning of every servant
                                        --   and websockets transaction (See #358)
  , _cfgHaveRestApi   :: Bool           -- ^ whether we want to talk to client via servant at all.
                                        --   refine-frontend just needs web sockets to work.  if
                                        --   this is False, we do not set or read any cookies in the
                                        --   connecting browsers.
  }
  deriving (Eq, Show, Generic)

data LogCfg = LogCfg
  { _logCfgTarget :: LogTarget
  , _logCfgLevel  :: LogLevel
  }
  deriving (Eq, Show, Generic)

data LogTarget =
    LogCfgFile { _logcfgFilePath :: FilePath }
  | LogCfgStdOut
  | LogCfgDevNull
  deriving (Eq, Show, Generic)

data LogLevel = LogError | LogWarning | LogInfo | LogDebug
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

-- | IMPORTANT: 'DBInMemory' is fragile, it tends to lose its contents between transactions.  I
-- haven't investigated, but DBOnDisk definitely works better!
data DBKind
  = DBOnDisk FilePath
  | DBInMemory
  deriving (Eq, Show, Generic)

data SmtpCfg = SmtpCfg
  { _smtpSenderName       :: ST
  , _smtpSenderEmail      :: ST
  , _smtpDefaultRecipient :: ST  -- ^ (will receive a test email on start, email address for demo users, ...)
  , _smtpSendmailPath     :: ST  -- ^ e.g., @/usr/sbin/sendmail@
  , _smtpSendmailArgs     :: [ST]
  }
  deriving (Eq, Show, Generic)

instance Default Config where
  def = Config
    { _cfgLogger        = def
    , _cfgDBKind        = def
    , _cfgPoolSize      = 5
    , _cfgFileServeRoot = Just "../frontend/js-build"
    , _cfgWarpSettings  = def
    , _cfgCsrfSecret    = "CSRF-SECRET"
    , _cfgSessionLength = TimespanHours 72
    , _cfgPoFilesRoot   = "./po"
    , _cfgSmtp          = Nothing
    , _cfgClient        = def
    , _cfgWSPingPeriod  = TimespanSecs 14
    , _cfgAllAreGods    = False
    , _cfgHaveRestApi   = True
    }

instance Default LogCfg where
  def = LogCfg def def

instance Default LogTarget where
  def = LogCfgStdOut

instance Default LogLevel where
  def = LogInfo

instance Default DBKind where
  def = DBOnDisk "./.backend-data/refine.db"

instance Default SmtpCfg where
  def = SmtpCfg
    { _smtpSenderName       = "Re-fine Notification System"
    , _smtpSenderEmail      = "postmaster@localhost"
    , _smtpDefaultRecipient = "postmaster@localhost"
    , _smtpSendmailPath     = "/usr/sbin/sendmail"
    , _smtpSendmailArgs     = ["-t"]
    }


-- * warp settings

-- | "Network.Wai.Handler.Warp" contains 'Settings', which we could use here, but that module is not
-- exposed from warp.
data WarpSettings = WarpSettings
  { _warpSettingsPort :: Warp.Port
  , _warpSettingsHost :: Warp.HostPreference
  }
  deriving (Eq, Show, Generic)

warpSettings :: Config -> Settings
warpSettings cfg = Warp.defaultSettings
  & setPort (_warpSettingsPort . _cfgWarpSettings $ cfg)
  & setHost (_warpSettingsHost . _cfgWarpSettings $ cfg)


-- * lenses/TH

deriveClasses [([''Config, ''LogCfg, ''LogTarget, ''LogLevel, ''DBKind, ''SmtpCfg], [''SOP.Generic, ''Lens', ''FromJSON])]
deriveClasses [([''WarpSettings], [''SOP.Generic, ''Lens'])]

instance ToJSON WarpSettings where
  toJSON (WarpSettings port host) = object
    [ "_warpSettingsPort" .:= port
    , "_warpSettingsHost" .:= show host
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
  , cs (Yaml.encode cfg)
  , "------------------------------"
  , ""
  ] <>
  if mentionServerConf
    then [ "You can copy the yaml code between the lines above into 'server.conf',"
         , "edit to your liking, and pass it as first argument to the server."
         , ""
         ]
    else []


-- * add users and other content via command line

data CliCreate =
    CliCreateUser (CreateUser, [(GroupRole, ST{- group title -})], [GlobalRole])
  | CliCreateGroup CreateGroup
  deriving (Eq, Show, Generic)

deriveClasses [([''CliCreate], [''SOP.Generic, ''Lens', ''FromJSON])]


-- | Initialize config from a given yaml file or, if 'Nothing' is given, the default value.  Print
-- config to stdout before returning with it, as a simple mechanism for providing a default yaml
-- file (people who run this for the first time can copy stdout into a file and edit that).
readCliCreate :: FilePath -> IO [CliCreate]
readCliCreate fp = either bad pure =<< Yaml.decodeFileEither fp
  where
    bad msg = throwIO . ErrorCall $ show msg <> "\n\n" <> helpCliCreateMsg


helpCliCreate :: IO ()
helpCliCreate = putStrLn helpCliCreateMsg

helpCliCreateMsg :: String
helpCliCreateMsg = unlines
 [ "to create initial database groups and users, adapt the following sample data file:"
 , ""
 , cs $ Yaml.encode sampleContent
 , ""
 , "Note that groups are defined twice under the same name if you run the same data"
 , "twice.  Users will NOT be created, and a warning will be issued.  If a user is assigned"
 , "to a group that has a non-unique title, she is assigned to both groups that have that"
 , "title.  If no such group exists, it is created."
 ]

sampleContent :: [CliCreate]
sampleContent =
  [ CliCreateGroup $ CreateGroup "Universe" "The group that contains everything" [] [] mempty
  , CliCreateGroup $ CreateGroup "Greek Party" "Something about ethnics and politics" [] [] mempty
  , CliCreateUser (CreateUser "admin" "admin@localhost" "pass" Nothing "", [], [GlobalAdmin])
  , CliCreateUser (CreateUser "edna" "edna@localhost" "pass" Nothing "", [(GroupMember, "Edna's home group"), (GroupMember, "Greek Party")], [])
  , CliCreateUser (CreateUser "joe" "joe@localhost" "pass" Nothing "", [(GroupMember, "Joe's home group")], [])
  ]
