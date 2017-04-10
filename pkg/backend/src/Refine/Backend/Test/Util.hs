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
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.Test.Util
where

import           System.IO.Temp (withSystemTempDirectory)
import           System.Directory (withCurrentDirectory)
import           Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Refine.Prelude (Timestamp(..))
import Refine.Common.Types


withTempCurrentDirectory :: IO a -> IO a
withTempCurrentDirectory action = withSystemTempDirectory "refine.tmp" (`withCurrentDirectory` action)

-- | Pattern match on the result will trigger the evaluation of the term under test.  The trick is
-- that there are two different units: the one that is returned by 'm' and pattern-matched, and the
-- one that is returned from 'forceEval'.  This way, even if outside the call to 'forceEval' the
-- unit is not matched and thus forced, the unit of the 'm' argument is still forced.
forceEval :: Monad m => m () -> m ()
forceEval m = do
  () <- m
  pure ()

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
