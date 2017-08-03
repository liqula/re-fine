{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.Config where

import Refine.Backend.Prelude

import qualified Data.Yaml as Yaml
import qualified Generics.SOP as SOP
import           Network.Wai.Handler.Warp as Warp


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
  }
  deriving (Eq, Show, Generic)

data LogCfg =
    LogCfgFile { _logcfgFilePath :: FilePath }
  | LogCfgStdOut
  | LogCfgDevNull
  deriving (Eq, Show, Generic)

data DBKind
  = DBInMemory
  | DBOnDisk FilePath
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
    }

instance Default LogCfg where
  def = LogCfgStdOut

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

deriveClasses [([''Config, ''LogCfg, ''DBKind, ''SmtpCfg], [''SOP.Generic, ''Lens', ''FromJSON])]
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
