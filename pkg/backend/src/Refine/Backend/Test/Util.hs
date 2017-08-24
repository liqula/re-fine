{-# LANGUAGE CPP #-}
#include "language.hs"

module Refine.Backend.Test.Util
where

import Refine.Backend.Prelude

import           System.IO.Temp (withSystemTempDirectory)
import           System.Directory (withCurrentDirectory)
import           Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Refine.Prelude (Timestamp(..))
import Refine.Common.Types


withTempCurrentDirectory :: IO a -> IO a
withTempCurrentDirectory action = withSystemTempDirectory "refine.tmp" (`withCurrentDirectory` action)

sampleID :: ID a
sampleID = ID 1

sampleTime :: Timestamp
sampleTime = Timestamp $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2017-04-06 08:44:40 CEST"

sampleUserInfo :: UserInfo
sampleUserInfo = Anonymous

sampleMetaInfo :: MetaInfo
sampleMetaInfo = MetaInfo sampleUserInfo sampleTime sampleUserInfo sampleTime

sampleMetaID :: MetaID a
sampleMetaID = MetaID sampleID sampleMetaInfo
